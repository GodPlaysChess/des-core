package core

import cats.Monoid
import cats.data._
import cats.implicits._
import core.Distance.shortestPath
import core.Sim.Time
import core.VehicleFollowsLogicalRoute.Location
import monocle.macros.Lenses
import monocle.macros.syntax.lens._

import scala.collection.immutable.Queue
import scala.util.Random

object Sim {
  type Time      = Double
  type HandlerId = Long

  def pure[W, A](a: A): Sim[W, A] = Sim(State.pure(a))

  def modifyW[W](f: W => W): Sim[W, Unit] = Sim(State.modify[SimState[W]](SimState.world.modify(f)))

  def modifyAndReturn[A, W](f: W => (W, A)): Sim[W, A] =
    Sim(State.apply[SimState[W], A] { w: SimState[W] =>
      val (w1, a) = f(w.world)
      SimState.world.set(w1)(w) -> a
    })

  def currTime[W]: Sim[W, Time] = Sim(State.inspect(_.currentTime))

  def getWorld[W]: Sim[W, W] = Sim(State.inspect(_.world))

  /**
    *  Priority for defining the precedence if 2 different handlers react to same event
    *  @param interval - when handler is active. After end of the interval it will be deregistered.
    */
  def registerHandler[W](handler: Handler[W],
                         priority: Int = 10,
                         interval: (Time, Time) = (0D, Double.MaxValue)): Sim[W, HandlerId] = {
    val l = new Random().nextLong()
    Sim(State.modify[SimState[W]](SimState.handlers.modify(hs => hs.add(???, l, handler)))).map(_ => l)
  }

  def deRegisterHanlder[W](handlerId: HandlerId): Sim[W, Unit] =
    Sim(State.modify[SimState[W]](SimState.handlers.modify(hs => hs - handlerId)))

  def registerReplaner[W](replaner: Replaner[W]): Sim[W, Unit] =
    Sim(State.modify[SimState[W]](SimState.replanner.set(replaner)))

  // priority queue should do it for us
  def addEvents[W](list: List[Event]): Sim[W, Unit] =
    Sim(State.modify[SimState[W]](SimState.eventQueue.modify(_.enqueue(list).sortBy(_.when))))

  /**
    *   Register handler, which is de-registers after executing once
    */
  def registerOnce[W](handler: Handler[W], after: Time): Sim[W, Unit] = {
    registerHandler(handler, interval = (after, Double.MaxValue))
    ???
  }

  def showState[W](when: Time): Sim[W, SimState[W]] = ???

}

@Lenses // todo optimisation would be to have a data structure to do fast query for all handlers for the particular event
case class SimState[W](handlers: Handlers[W],
                       replanner: Replaner[W],
                       eventQueue: Queue[Event],
                       currentTime: Time,
                       eventLog: List[Event],
                       world: W) {
  override def toString: String = s"simulationTime: $currentTime \n world: $world \n queue: $eventQueue"
}

/**
  * It is MonadState and MonadReader
  * //todo @tparam I - input configuration
  *
  * @tparam W - world
  * @tparam A - output
  */
case class Sim[W, A](underlying: State[SimState[W], A]) {
  def exec(initialState: W, upTo: Time): (List[Event], W) = {
    val (result, _) =
      loop(upTo, underlying.map(_ => ()))
        .run(SimState(Map.empty, Replaners.identityReplaner, Queue.empty, 0d, List.empty, initialState))
        .value
    (result.eventLog.reverse, result.world)
  }

  // could be various combinators such as `once` - for de-registering handler, or `upTo` - to specify when handler should kick in
  def addEvent(e: Event): Sim[W, Unit] =
    Sim(underlying.modify(SimState.eventQueue.modify(_.enqueue(e).sortBy(_.when))).map(_ => ()))

  def map[B](f: A => B): Sim[W, B] = Sim(underlying.map(f))

  def flatMap[B](f: A => Sim[W, B]): Sim[W, B] = Sim(underlying.flatMap(f(_).underlying))

  //alternative is to implement MonadState to sim -> then we can avoid propagation of the input parameter.
  private def loop(upTo: Time, initial: State[SimState[W], Unit]): State[SimState[W], Unit] =
    for {
      simState <- initial.get
      _ <- simState.eventQueue.dequeueOption match {
        case None                                   => State.pure[SimState[W], Unit](()) // finished simulation - no more events
        case Some((event, _)) if event.when >= upTo =>
          // stop a simulation, because no mor events should happen in given period of time.
          println("end simulation")
          State.set(simState.lens(_.currentTime).set(event.when))

        case Some((event, queue)) if event.when < upTo => // still have time to go on
//          println(s"processing $event")
          val newState = for { // running all the handlers (though can filter only the required one them here)
            newEvents <- simState.handlers.get(event).values.toList
              .sortBy(_._1)
              .map(_._2)
              .flatTraverse[State[SimState[W], ?], Event](_.run(event).underlying)
            newWorld <- State.inspect[SimState[W], W](_.world) //we need world update dy handlers
            updQueue = simState.replanner.replan(newWorld, event, queue.enqueue(newEvents)) // need to calculate current location, otherwise it will be always outdated.
            newState = SimState(simState.handlers,
                                simState.replanner,
                                updQueue,
                                event.when,
                                event :: simState.eventLog,
                                newWorld)
            _ <- State.set(newState)
          } yield ()
          loop(upTo, newState)
      }
    } yield ()
}

/**
  * Reacts to the event, modifying the world and possibly emitting more events
  */
trait Handler[W] {
  def run: Event => Sim[W, List[Event]]

  // todo make combinators to
  // 1. that is valid only in Interval
  // 2. expires after being executed N times
}

object Handler {
  def apply[W](runH: Event => Sim[W, List[Event]]): Handler[W] = new Handler[W] {
    override def run = runH
  }

  def fromPartial[W](pf: PartialFunction[Event, W => (W, List[Event])]): Handler[W] =
    new Handler[W] {
      override def run: Event => Sim[W, List[Event]] = event => {
        if (pf.isDefinedAt(event)) Sim.modifyAndReturn(pf.apply(event))
        else Sim.pure(List.empty[Event])
      }
    }

  implicit def handlerInstance[W]: Monoid[Handler[W]] = new Monoid[Handler[W]] {
    override def empty = Handler(_ => Sim.pure(List.empty))

    override def combine(x: Handler[W], y: Handler[W]) = new Handler[W] {
      override def run = ev => x.run(ev).flatMap(li => y.run(ev).map(_ ++ li))
    }
  }
}

// when here contaminates the domain. Look for usages
trait Event {
  def when: Time

  def reschedule(time: Time): Event
}

trait Replaner[W] {

  /**
    * Replaner reacts to the event by changing the agenda
    * (It does not modify the world. Handlers do it)
    */
  def replan(world: W, event: Event, agenda: Queue[Event]): Queue[Event]
}

// maybe do it with replanning handlers as well?
object Replaners {
  import Distance._
  import VehicleFollowsLogicalRoute._

  def identityReplaner[W]: Replaner[W] = (_: W, _: Event, agenda: Queue[Event]) => agenda

  /**
    * In this case replaner only changes the order of events in the queue, but do not add anything.
    */
  val SimulationReplaner: Replaner[Vehicle] = new Replaner[Vehicle] {

    def replan(world: Vehicle, event: Event, agenda: Queue[Event]): Queue[Event] = event match {
      case RideRequest(fr, to, when) =>
        val all = world.location :: world.route
        val timeIntervals = all
          .zip(all.tail) // if rideRequest comes in -> there must be at least 2 elements in the route
          .foldLeft((List.empty[(Location, Time)], when)) {
            case ((acc, dist), (fr, to)) =>
              val max = distance(fr, to) + dist
              ((to, max) :: acc) -> max
          }
          ._1
          .toMap // deliveryTime is distance/speed
        println(all)
        println("Time intervals:" + timeIntervals)
        // rescheduling only pickup and deliveries
        val updAgenda = agenda
          .map {
            case e @ Pickup(location, _)   => e.reschedule(timeIntervals(location))
            case e @ Delivery(location, _) => e.reschedule(timeIntervals(location))
            case e                         => e // do not reschedule anything else, for example RideRequests, since we are not determine them
          }
          .sortBy(_.when)

        updAgenda

      case _ => agenda //just do nothing

    }
  }
}

object Distance {

  /**
    * Manhattan distance between 2 points.
    */
  def distance(fr: Location, to: Location): Int = (to._1 - fr._1).abs + (to._2 - fr._2).abs

  /**
    * For simplicity shortest path is calculated that first you go vertical, then horizontal
    */
  def shortestPath(start: Location, route: List[Location]): List[Location] = start :: route.sortBy(_._1).sortBy(_._2)

  /**
    * starting from current, heading to destination, where vehicle will be in time
    */
  def position(current: Location, destination: Location, time: Time): Location = {
    val vDist = destination._1 - current._1
    val hDist = destination._2 - current._2
    if (time < vDist.abs) (current._1 + time.toInt * vDist.signum, current._2)
    else if (time > vDist.abs + hDist.abs) destination
    else (destination._1, current._2 + (time.toInt - vDist.abs) * hDist.signum)
  }

}

object VehicleFollowsLogicalRoute extends App {

  type Location = (Int, Int)
  type Route    = List[Location]
  type World    = Vehicle
  case class Vehicle(passengers: Int, location: Location, route: List[Location] = List.empty)

  case class RideRequest(from: Location, to: Location, when: Time) extends Event {
    def reschedule(time: Time): Event = this.copy(when = time)
  }
  case class Pickup(from: Location, when: Time) extends Event {
    def reschedule(time: Time): Event = this.copy(when = time)
  }
  case class Delivery(location: Location, when: Time) extends Event {
    def reschedule(time: Time): Event = this.copy(when = time)
  }
  case class ChangeRoute(newRoute: Route, when: Time) extends Event {
    def reschedule(time: Time): Event = this.copy(when = time)
  }

  /***
    *    Simulate two pickups:
    *   - P2 - -  -
    *  P1  - - D1 D2
    *  V   - - -  -
    */
  val eventList = List(
    RideRequest((1, 0), (1, 3), 10d),
    RideRequest((2, 1), (1, 4), 12d)
  )

  val lnpTest = (1 to 10000).map(i => RideRequest((i, i), (i + 10, i + 10), i * 10)).toList

  def rideRequestHandler = Handler.fromPartial[World] {
    case RideRequest(from, to, when) =>
      w =>
        {
          // calculating a new route here based on 2 new waypoints.
          val newRoute = shortestPath(w.location, from :: to :: w.route).tail //current location should not be included
          w.lens(_.route)
            .set(newRoute) -> // changing route here instead of emitting additional event, due replaner should have updated route
          List(Pickup(from, Double.MaxValue), Delivery(to, Double.MaxValue))
        }
  }

  /**
    * Updates the number of passengers in the car after pickup and modifies the route of the vehicle
    */
  def pickUpHandler = Handler.fromPartial[World] {
    case Pickup(location, when) =>
      w =>
        Vehicle(w.passengers + 1, location, w.route.tail) -> List.empty
  }

  /**
    * Updates the number of passengers in the car after delivery
    */
  def deliveryHandler = Handler.fromPartial[World] {
    case Delivery(location, when) =>
      w =>
        Vehicle(w.passengers - 1, location, w.route.tail) -> List.empty
  }

  /**
    * This handler updates the vehicle position on any event
    */
  def updateVehiclePositionHandler = Handler[World] { ev =>
    for {
      currTime <- Sim.currTime
      _ <- Sim.modifyW[World](
        world => {
          println(s"modifying vehicle location. Current world $world")
          world
            .lens(_.location)
            .set( //todo  here's the problem. If it kicks in after rideRequest -> then it assumes that vehicle was moving towards the route, request *just* added.
              world.route.headOption.fold(world.location)(Distance.position(world.location, _, ev.when - currTime))
            )
        }
      )
    } yield List.empty[Event]
  }

  /**
    * This handler updates the vehicle route
    */
  def changeRouteHandler = Handler.fromPartial[World] {
    case ChangeRoute(route, when) => _.lens(_.route).set(route) -> List.empty
  }

  val simulation = for {
    pickupH      <- Sim.registerHandler(pickUpHandler)
    rideRequestH <- Sim.registerHandler(rideRequestHandler)
    deliveryH    <- Sim.registerHandler(deliveryHandler)
    updatePosH   <- Sim.registerHandler(updateVehiclePositionHandler, 1)
    changeRouteH <- Sim.registerHandler(changeRouteHandler)
    _            <- Sim.registerReplaner(Replaners.SimulationReplaner)
    _            <- Sim.addEvents(eventList) // lnpTest
  } yield ()

  // alternative way would be to compose handlers:
  val routeHandler = rideRequestHandler |+| pickUpHandler |+| deliveryHandler
  val simulation2 = for {
    vehicleH <- Sim.registerHandler(routeHandler)
    worldH   <- Sim.registerHandler(updateVehiclePositionHandler |+| changeRouteHandler, 1)
    _        <- Sim.registerReplaner(Replaners.SimulationReplaner)
    _        <- Sim.addEvents(eventList) // lnpTest
  } yield ()

  val begin  = System.currentTimeMillis()
  val result = simulation.exec(Vehicle(0, (0, 0)), 500000)
  println(result)
  println(s"Calculation took ${System.currentTimeMillis() - begin} millis")
}
