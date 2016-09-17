/* shellPrompt := { state => */
/*   "%s|> ".format(Project.extract(state).currentProject.id) */
/* } */

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

import com.scalapenos.sbt.prompt._
import SbtPrompt.autoImport._
