// The simplest possible sbt build file is just one line:

scalaVersion := "2.13.3"

scalacOptions ++= Seq("-unchecked", "-deprecation")

name := "streams"
organization := "ca.uvic"
version := "1.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",

  // Note: This is not actually used in the submission, it was just for messing around with parallelizing the flajolet
  // martin
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

