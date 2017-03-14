resolvers ++= Seq(
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com",
  "Jenkins repo" at "http://repo.jenkins-ci.org/public/"
  )

/////////////////////////////////////////////////////////////////////////////////

addSbtPlugin("com.scalapenos" % "sbt-prompt" % "1.0.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.1")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M15-2")

addSbtPlugin("de.heikoseeberger" % "sbt-fresh" % "1.6.9")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.2.0")

addSbtPlugin("com.github.xuwei-k" % "sbt-class-diagram" % "0.2.0")

addSbtPlugin("ch.jodersky" % "sbt-jni" % "1.2.5")

// addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.3.1")
// addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.2.0-RC1")
//  addSbtPlugin("com.scalakata" % "sbt-scalakata" % "1.1.5")
// addSbtPlugin("ohnosequences" % "sbt-github-release" % "0.4.0")
// addSbtPlugin("net.vonbuchholtz" % "sbt-dependency-check" % "0.1.7")

// addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")
// addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.5")

// if (sys.props("java.version").startsWith("1.6"))
//   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.0.0")
// else
//   addSbtPlugin("org.ensime" % "sbt-ensime" % "1.9.1")

addSbtPlugin("com.softwaremill.clippy" % "plugin-sbt" % "0.5.2")

addSbtPlugin("com.dwijnand" % "sbt-project-graph" % "0.2.0")

addSbtPlugin("net.ruippeixotog" % "sbt-classfinder" % "0.1.1")
