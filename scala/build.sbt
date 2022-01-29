ThisBuild / organization := "be.adamv"
ThisBuild / version := "0.2.3"
ThisBuild / scalaVersion := "3.1.2-RC1-bin-20220118-9e14f5f-NIGHTLY"

lazy val root = (project in file("."))
  .settings(
    name := "RecursionSchemes",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)

publishTo := Some(Resolver.file("local-ivy", file("~")))
