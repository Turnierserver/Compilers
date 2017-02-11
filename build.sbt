name := "Compilers"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "net.lingala.zip4j" % "zip4j" % "1.3.2"

mainClass in (Compile, run) := Some("Main")
