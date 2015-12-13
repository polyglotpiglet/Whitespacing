name := "whitespacing"

version := "1.0"

scalaVersion := "2.11.5"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= {
  val scalaTestVersion  = "2.2.4"
  Seq(
    "org.scalatest"     %% "scalatest" % scalaTestVersion % "test"
  )
}