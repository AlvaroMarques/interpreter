ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

val enumeratumVersion = "1.7.5"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
  "ch.qos.logback" % "logback-classic" % "1.3.5",
  "org.slf4j" % "slf4j-api" % "2.0.0"
)

Compile / unmanagedResourceDirectories += baseDirectory.value / "src" / "main" / "resources"

Compile / mainClass := Some("main")

lazy val root = (project in file("."))
  .settings(
    name := "interpret"
  )
