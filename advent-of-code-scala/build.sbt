name := """advent-of-code"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"

fork in run := true