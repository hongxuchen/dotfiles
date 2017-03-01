scalacOptions += "-P:clippy:colors=true"
import com.softwaremill.clippy.ClippySbtPlugin._
clippyColorsEnabled := true

// clippyColorLiteral := Some(ClippyColor.Magenta)

initialize ~= (_ => if (ConsoleLogger.formatEnabled) sys.props("scala.color") = "true")
