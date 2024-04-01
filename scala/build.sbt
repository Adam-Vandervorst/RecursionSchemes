ThisBuild / organization := "be.adamv"
ThisBuild / version := "0.2.4"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "RecursionSchemes",
  ).jvmSettings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )

publishTo := Some(Resolver.file("local-ivy", file("~")))
