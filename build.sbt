ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test

lazy val root = (project in file("."))
  .settings(
    name := "LFA-2022-2023"
  )
