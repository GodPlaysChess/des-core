package core
import cats.Monoid
import core.Sim.HandlerId

class Handlers[W](private[gleb] val all: Map[Int, List[(HandlerId, Handler[W])]]) {

  // this can be slow, since it's rarely done. Can rly go through all data structure.
  def remove(id: HandlerId): Handlers[W] = new Handlers(
    all.mapValues(_.filter(_._1 != id))
  )

  // compute to get the key? How can I ensure, that different instances of the same type map to the same key?
  def add(ev: Class[Event], id: HandlerId, handler: Handler[W]) =
    new Handlers(all.updated(ev.##, (id -> handler) :: all.getOrElse(ev.##, List.empty[(HandlerId, Handler[W])])))

}

object Handlers {
  import cats.implicits._

  implicit def handlersMonoid[W]: Monoid[Handlers[W]] = new Monoid[Handlers[W]] {
    override def empty = new Handlers(Map.empty)
    override def combine(x: Handlers[W], y: Handlers[W]) =
      new Handlers[W](x.all |+| y.all)
  }
}
