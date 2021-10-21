ThisBuild / homepage      := Some(url("https://github.com/japgolly/nyaya"))
ThisBuild / licenses      += ("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))
ThisBuild / organization  := "com.github.japgolly.nyaya"
ThisBuild / shellPrompt   := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / startYear     := Some(2014)
ThisBuild / versionScheme := Some("early-semver")

val root      = NyayaBuild.root
val utilJVM   = NyayaBuild.utilJVM
val utilJS    = NyayaBuild.utilJS
val propJVM   = NyayaBuild.propJVM
val propJS    = NyayaBuild.propJS
val genJVM    = NyayaBuild.genJVM
val genJS     = NyayaBuild.genJS
val testJVM   = NyayaBuild.testJVM
val testJS    = NyayaBuild.testJS
val benchmark = NyayaBuild.benchmark
