import sbt._
import Keys._
import pl.project13.scala.sbt.JmhPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._

object NyayaBuild {

  private val ghProject = "nyaya"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    final val KindProjector = "0.9.3"
    final val Monocle       = "1.3.2"
    final val MTest         = "0.4.4"
    final val Scala211      = "2.11.8"
    final val Scala212      = "2.12.0"
    final val Scalaz        = "7.2.7"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization             := "com.github.japgolly.nyaya",
      homepage                 := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                 += ("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
      scalaVersion             := Ver.Scala211,
      crossScalaVersions       := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions           ++= scalacFlags,
      scalacOptions in Test   --= Seq("-Ywarn-dead-code"),
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage         := Watched.clearWhenTriggered,
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % Ver.KindProjector))
    .configure(
      addCommandAliases(
        "BM"  -> "project benchmark",
        "/"   -> "project root",
        "L"   -> "root/publishLocal",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "TL"  -> ";T;L",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "to"  -> "test-only",
        "tq"  -> "test-quick",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test")))

    def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % "test",
      testFrameworks      += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      // Not mandatory; just faster.
      _.settings(jsEnv in Test := PhantomJSEnv().value))


  // ==============================================================================================

  lazy val root = (project in file("."))
    .configure(commonSettings.jvm, preventPublication)
    .aggregate(
      utilJVM, propJVM, genJVM, testJVM,
      utilJS, propJS, genJS, testJS,
      benchmark)

  lazy val utilJVM = util.jvm
  lazy val utilJS  = util.js
  lazy val util = crossProject
    .in(file("util"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      moduleName := "nyaya-util",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val propJVM = prop.jvm
  lazy val propJS  = prop.js
  lazy val prop = crossProject
    .in(file("prop"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-prop",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val genJVM = gen.jvm
  lazy val genJS  = gen.js
  lazy val gen = crossProject
    .in(file("gen"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-gen",
      libraryDependencies ++= Seq(
        "org.scalaz" %%% "scalaz-core" % Ver.Scalaz,
        "com.github.julien-truffaut" %%% "monocle-core" % Ver.Monocle,
        "com.github.julien-truffaut" %%% "monocle-macro" % Ver.Monocle % "test"
      ))

  lazy val testJVM = testModule.jvm
  lazy val testJS  = testModule.js
  lazy val testModule = crossProject
    .in(file("test"))
      .configureCross(commonSettings, publicationSettings)
      .dependsOn(prop, gen)
      .configureCross(utestSettings)
    .settings(
      name := "test",
      moduleName := "nyaya-test")

  lazy val benchmark = (project in file("benchmark"))
    .enablePlugins(JmhPlugin)
    .configure(commonSettings.jvm, preventPublication)
    .dependsOn(propJVM, genJVM, testJVM)
}
