//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.12")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.3.3")

// Plugin to check not up-to-date dependencies from maven/ivy/...
// Use: "sbt dependencyUpdates"
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")