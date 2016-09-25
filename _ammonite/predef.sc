interp.load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
interp.load.ivy("org.typelevel" %% "cats-core" % "0.7.2")
interp.load.ivy("org.scalaj" %% "scalaj-http" % "2.2.0")
interp.load.ivy("com.lihaoyi" %% "upickle" % "0.4.0")
interp.load.ivy("com.lihaoyi" %% "pprint" % "0.4.0")
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell._
ammonite.shell.Configure(repl, wd)
repl.prompt() = "> "
