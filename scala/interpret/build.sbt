ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

val enumeratumVersion = "1.7.5"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "org.scalatest" %% "scalatest" % "3.2.19" % "test"
)

lazy val root = (project in file("."))
  .settings(
    name := "interpret"
  )
