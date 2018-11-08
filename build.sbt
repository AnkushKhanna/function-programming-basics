
name := "function-programming-basics"

version := "0.1"

scalaVersion := "2.12.5"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scala-lang" % "scala-reflect" % "2.12.6",
  "org.specs2" %% "specs2-core" % "3.9.4" % Test,
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  "org.mockito" % "mockito-core" % "2.15.0" % Test
)