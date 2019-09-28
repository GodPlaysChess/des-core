name := "des-core"

version := "0.1"

scalaVersion := "2.12.9"

lazy val Version = new {
  val dispatchingCommons = "3.2.0"
  val pureConfig         = "0.11.1"
  val log4j              = "2.11.1"
  val scalaTest          = "3.0.7"
  val monocleVersion     = "2.0.0"
}

lazy val library = new {

  val log4jApi           = "org.apache.logging.log4j"   % "log4j-api"            % Version.log4j
  val log4jCore          = "org.apache.logging.log4j"   % "log4j-core"           % Version.log4j
  val logSlfOverLog4j    = "org.apache.logging.log4j"   % "log4j-slf4j-impl"     % Version.log4j
  val pureConfig         = "com.github.pureconfig"      %% "pureconfig"          % Version.pureConfig
  val scalaTest          = "org.scalatest"              %% "scalatest"           % Version.scalaTest
  val cats               = "org.typelevel"              %% "cats-core"           % "2.0.0"
  val monocleCore        = "com.github.julien-truffaut" %% "monocle-core"        % Version.monocleVersion
  val monocleMacro       = "com.github.julien-truffaut" %% "monocle-macro"       % Version.monocleVersion
}

libraryDependencies ++= Seq(
  library.log4jApi           % Compile,
  library.log4jCore          % Compile,
  library.pureConfig         % Compile,
  library.cats               % Compile,
  library.monocleCore        % Compile,
  library.monocleMacro       % Compile,
  library.scalaTest          % Test
)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")