name := "advent-of-code"

version := "0.1"

scalaVersion := "2.13.10"

resolvers += Resolver.mavenLocal
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

val testVersion = "3.2.15"

libraryDependencies += "org.scalactic" %% "scalactic" % testVersion
libraryDependencies += "org.scalatest" %% "scalatest" % testVersion % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % testVersion % "test"
libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.15.0" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0")
libraryDependencies += ("com.beachape" %% "enumeratum" % "1.7.2")

testForkedParallel in IntegrationTest := true