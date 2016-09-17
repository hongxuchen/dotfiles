load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
load.ivy("org.scalaj" %% "scalaj-http" % "2.2.0")
load.ivy("com.lihaoyi" %% "upickle" % "0.4.0")
load.ivy("com.lihaoyi" %% "pprint" % "0.4.0")
// load.ivy("org.scalanlp" %% "breeze" % "0.11.2")
// load.ivy("org.scalanlp" %% "breeze-natives" % "0.11.2")
// load.ivy("org.scalanlp" %% "breeze-viz" % "0.11.2")
// load.ivy("org.scalaz" % "scalaz-core_2.11" % "7.3.0-M2")
load.ivy("org.typelevel" % "cats-core_2.11" % "0.7.2")
@
import scalaj.http._
// import upickle.default._
val shellSession = ammonite.shell.ShellSession()
import shellSession._
// import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell._
ammonite.shell.Configure(repl, wd)
