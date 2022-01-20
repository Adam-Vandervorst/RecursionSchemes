ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2-RC1-bin-20220118-9e14f5f-NIGHTLY"

lazy val root = (project in file("."))
  .settings(
    name := "RecursionSchemes",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)
