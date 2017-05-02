scalacOptions += "-P:clippy:colors=true"
import com.softwaremill.clippy.ClippySbtPlugin._
clippyColorsEnabled := true

// clippyColorLiteral := Some(ClippyColor.Magenta)

initialize ~= (_ => if (ConsoleLogger.formatEnabled) sys.props("scala.color") = "true")

import de.heikoseeberger.sbtfresh.FreshPlugin.autoImport._
import de.heikoseeberger.sbtfresh.license.License

freshOrganization := "com.github.hongxuchen"        // Organization – "default" by default
freshAuthor       := "Hongxu Chen"        // Author – value of "user.name" system property or "default" by default
freshLicense      := Some(License.mit) // Optional license – `apache20` by default
freshSetUpGit     := true              // Initialize a Git repo and create an initial commit – `true` by default
freshSetUpTravis  := true              // Configure Travis for Continuous Integration - `true` by default
freshUseGitPrompt := true              // Use the prompt from the sbt-git plugin - `false` by default
