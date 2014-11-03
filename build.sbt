name := "SVG path to polylines"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang" % "scala-xml" % "2.11.0-M4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"