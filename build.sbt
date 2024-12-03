import scala.collection.immutable.Seq

name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.14"
val testVersion = "3.2.15"

resolvers ++= Seq(
  Resolver.mavenLocal,
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % testVersion,
  "org.scalatest" %% "scalatest" % testVersion % Test,
  "org.scalatest" %% "scalatest-flatspec" % testVersion % Test,
  "org.scalatestplus" %% "junit-4-13" % "3.2.15.0" % Test,
  "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % Test,
  "org.typelevel" %% "cats-core" % "2.9.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "com.beachape" %% "enumeratum" % "1.7.2"
)

enablePlugins(JmhPlugin)