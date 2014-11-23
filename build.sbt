name := """scala-akka-mars-rover"""

version := "1.0"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.6",
  "com.typesafe.akka" %% "akka-persistence-experimental" % "2.3.4",
  "com.typesafe.akka" %% "akka-stream-experimental" % "0.8",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
