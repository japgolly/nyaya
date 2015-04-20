import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin._
import ScalaJSPlugin.autoImport._
import Dialect._
import Typical.{settings => _, _}

object Nyaya extends Build {

  val Scala211 = "2.11.6"

  val commonSettings: CDS =
    CDS.all(
      _.settings(
        organization       := "com.github.japgolly.nyaya",
        version            := "0.5.11",
        homepage           := Some(url("https://github.com/japgolly/nyaya")),
        licenses           += ("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
        scalaVersion       := Scala211,
        crossScalaVersions := Seq("2.10.4", Scala211),
        scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature",
                                "-language:postfixOps", "-language:implicitConversions",
                                "-language:higherKinds", "-language:existentials"),
        updateOptions      := updateOptions.value.withCachedResolution(true))
    ) :+ Typical.settings("nyaya")

  val scalaz = Library("org.scalaz", "scalaz-core", "7.1.1").myJsFork("scalaz").jsVersion(_+"-2")
  val rng    = Library("com.nicta",  "rng",         "1.3.0").myJsFork("nicta" ).jsVersion(_+"-2")

  def monocle(m: String) = Library("com.github.julien-truffaut", "monocle-"+m, "1.1.0").myJsFork("monocle")//.jsVersion(_+"-2")
  val monocleCore  = monocle("core")
  val monocleMacro = monocle("macro")

  // ==============================================================================================
  override def rootProject = Some(root)

  lazy val root = Project("root", file("."))
    .configure(commonSettings(None))
    .aggregate(core, ntest)

  // lazy val allJvm = Project("jvm", file(".")) .configure(commonSettings(None))
    // .aggregate(coreJvm, ntestJvm)
  // lazy val allJs = Project("js", file(".")) .configure(commonSettings(None))
    // .aggregate(coreJs, ntestJs)

  lazy val (core, coreJvm, coreJs) =
    crossDialectProject("nyaya-core", commonSettings
      .configure(utestSettings())
      .addLibs(scalaz)
    )

  lazy val (ntest, ntestJvm, ntestJs) =
    crossDialectProject("nyaya-test", commonSettings
      .dependsOn(coreJvm, coreJs)
      .configure(utestSettings("compile"))
      .addLibs(monocleCore, monocleMacro % "test", rng)
    )
}
