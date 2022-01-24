import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Ver {

    // Externally observable
    val cats             = "2.6.1"
    val microlibs        = "4.0.0"
    val monocle          = "3.1.0"
    val scala2           = "2.13.6"
    val scala3           = "3.0.1"

    // Internal
    val betterMonadicFor = "0.3.1"
    val kindProjector    = "0.13.2"
    val utest            = "0.7.11"
  }

  object Dep {
    val alleyCats         = Def.setting("org.typelevel"                 %%% "alleycats-core" % Ver.cats)
    val cats              = Def.setting("org.typelevel"                 %%% "cats-core"      % Ver.cats)
    val microlibsMultimap = Def.setting("com.github.japgolly.microlibs" %%% "multimap"       % Ver.microlibs)
    val microlibsTestUtil = Def.setting("com.github.japgolly.microlibs" %%% "test-util"      % Ver.microlibs)
    val monocleCore       = Def.setting("dev.optics"                    %%% "monocle-core"   % Ver.monocle)
    val utest             = Def.setting("com.lihaoyi"                   %%% "utest"          % Ver.utest)

    // Compiler plugins
    val betterMonadicFor = compilerPlugin("com.olegpy"     %% "better-monadic-for" % Ver.betterMonadicFor)
    val kindProjector    = compilerPlugin("org.typelevel"  %% "kind-projector"     % Ver.kindProjector cross CrossVersion.full)
  }
}
