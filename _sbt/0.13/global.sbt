// shellPrompt := { state =>
//   "%s|> ".format(Project.extract(state).currentProject.id)
// }
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
import com.scalapenos.sbt.prompt._
import SbtPrompt.autoImport._
