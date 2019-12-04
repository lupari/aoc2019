name := "aoc2019"

version := "0.1"

scalaVersion := "2.12.4"

fork in test := true

parallelExecution in test := false

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
