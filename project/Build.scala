import sbt._
import Keys._
import pl.project13.scala.sbt.JmhPlugin
import com.typesafe.sbt.pgp.PgpKeys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin._
import ScalaJSPlugin.autoImport._
import Dialect._
import Typical.{settings => _, _}

object Nyaya extends Build {

  val Scala211 = "2.11.8"

  val commonSettings: CDS =
    CDS.all(
      _.settings(
        organization       := "com.github.japgolly.nyaya",
        version            := "0.7.2-SNAPSHOT",
        homepage           := Some(url("https://github.com/japgolly/nyaya")),
        licenses           += ("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
        scalaVersion       := Scala211,
        crossScalaVersions := Seq(Scala211),
        scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature",
                                "-language:postfixOps", "-language:implicitConversions",
                                "-language:higherKinds", "-language:existentials"),
        triggeredMessage   := Watched.clearWhenTriggered,
        updateOptions      := updateOptions.value.withCachedResolution(true))
      .configure(
        addCommandAliases(
          "C"    -> "root/clean",
          "/"    -> "project root",
          "BM"   -> "project benchmark",
          "cc"   -> ";clear;compile",
          "ctc"  -> ";clear;test:compile",
          "ct"   -> ";clear;test",
          "cq"   -> ";clear;testQuick",
          "ccc"  -> ";clear;clean;compile",
          "cctc" -> ";clear;clean;test:compile",
          "cct"  -> ";clear;clean;test"))
    ) :+ Typical.settings("nyaya")

  val scalaz = Library("org.scalaz", "scalaz-core", "7.2.2")

  def monocle(m: String) = Library("com.github.julien-truffaut", "monocle-"+m, "1.2.1")
  val monocleCore  = monocle("core")
  val monocleMacro = monocle("macro")

  // ==============================================================================================
  override def rootProject = Some(root)

  lazy val root = Project("root", file("."))
    .configure(commonSettings(None))
    .aggregate(util, prop, gen, ntest, benchmark)

  // lazy val allJvm = Project("jvm", file(".")) .configure(commonSettings(None))
    // .aggregate(coreJvm, ntestJvm)
  // lazy val allJs = Project("js", file(".")) .configure(commonSettings(None))
    // .aggregate(coreJs, ntestJs)

  lazy val (util, utilJvm, utilJs) =
    crossDialectProject("nyaya-util", commonSettings
      .configure(utestSettings())
      .addLibs(scalaz))

  lazy val (prop, propJvm, propJs) =
    crossDialectProject("nyaya-prop", commonSettings
      .dependsOn(utilJvm, utilJs)
      .configure(utestSettings())
      .addLibs(scalaz))

  lazy val (gen, genJvm, genJs) =
    crossDialectProject("nyaya-gen", commonSettings
      .dependsOn(utilJvm, utilJs)
      .configure(utestSettings())
      .addLibs(scalaz, monocleCore, monocleMacro % "test"))

  lazy val (ntest, ntestJvm, ntestJs) =
    crossDialectProject("nyaya-test", commonSettings
      .dependsOn(propJvm, propJs)
      .dependsOn(genJvm, genJs)
      .configure(utestSettings())
      .addLibs(monocleCore, monocleMacro % "test"))

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(JmhPlugin)
      .configure(commonSettings(JVM), preventPublication)
      .dependsOn(propJvm, genJvm, ntestJvm)
}
