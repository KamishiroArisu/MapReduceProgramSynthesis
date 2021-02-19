name := "MapReduceProgramSynthesis"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.0" % "test"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.4"

val osInf = Option(System.getProperty("os.name")).getOrElse("")
val osArch = System.getProperty("sun.arch.data.model")

val isUnix    = osInf.indexOf("nix") >= 0 || osInf.indexOf("nux") >= 0
val isMac     = osInf.indexOf("Mac") >= 0

val osName = if (isMac) "mac" else if (isUnix) "unix" else "unknown"

unmanagedJars in Compile += file(s"lib/scalaz3-$osName-x64-2.13.jar")