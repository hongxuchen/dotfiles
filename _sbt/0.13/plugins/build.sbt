addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")

addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.5")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.1")

addSbtPlugin("com.scalapenos" % "sbt-prompt" % "1.0.0")

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M14")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.1.0")

// if (sys.props("java.version").startsWith("1.6"))
//   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.0.0")
// else
//   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.9.1")
