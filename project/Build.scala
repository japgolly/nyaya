import sbt._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import pl.project13.scala.sbt.JmhPlugin
import sbtrelease.ReleasePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import ScalaJSPlugin.autoImport._
import Lib._

object NyayaBuild {
  import sbtcrossproject.CrossPlugin.autoImport._

  private val ghProject = "nyaya"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  object Ver {
    val BetterMonadicFor = "0.3.1"
    val KindProjector    = "0.11.0"
    val Monocle          = "1.6.3"
    val MTest            = "0.7.4"
    val Scala212         = "2.12.11"
    val Scala213         = "2.13.5"
    val ScalaCollCompat  = "2.1.6"
    val Scalaz           = "7.2.30"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-opt:l:inline",
    "-opt-inline-from:scala.**",
    "-opt-inline-from:nyaya.**",
    "-Ywarn-dead-code",
    // "-Ywarn-unused",
    "-Ywarn-value-discard")

  val commonSettings = ConfigureBoth(
    _.settings(
      organization                  := "com.github.japgolly.nyaya",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags,
      scalacOptions in Test        --= Seq("-Ywarn-dead-code"),
      testFrameworks                := Nil,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      libraryDependencies           += "org.scala-lang.modules" %%% "scala-collection-compat" % Ver.ScalaCollCompat,
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.BetterMonadicFor),
      addCompilerPlugin("org.typelevel" %% "kind-projector" % Ver.KindProjector cross CrossVersion.full)))

    def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % Test,
      testFrameworks      += new TestFramework("utest.runner.Framework")))
    .jsConfigure(
      // Not mandatory; just faster.
      _.settings(jsEnv in Test := new JSDOMNodeJSEnv()))


  // ==============================================================================================

  lazy val root = (project in file("."))
    .configure(commonSettings.jvm, preventPublication)
    .aggregate(
      utilJVM, propJVM, genJVM, testJVM,
      utilJS, propJS, genJS, testJS,
      benchmark)

  lazy val utilJVM = util.jvm
  lazy val utilJS  = util.js
  lazy val util = crossProject(JVMPlatform, JSPlatform)
    .in(file("util"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      moduleName := "nyaya-util",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val propJVM = prop.jvm
  lazy val propJS  = prop.js
  lazy val prop = crossProject(JVMPlatform, JSPlatform)
    .in(file("prop"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-prop",
      libraryDependencies += "org.scalaz" %%% "scalaz-core" % Ver.Scalaz)

  lazy val genJVM = gen.jvm
  lazy val genJS  = gen.js
  lazy val gen = crossProject(JVMPlatform, JSPlatform)
    .in(file("gen"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-gen",
      libraryDependencies ++= Seq(
        "org.scalaz" %%% "scalaz-core" % Ver.Scalaz,
        "com.github.julien-truffaut" %%% "monocle-core" % Ver.Monocle,
        "com.github.julien-truffaut" %%% "monocle-macro" % Ver.Monocle % Test
      ))

  lazy val testJVM = testModule.jvm
  lazy val testJS  = testModule.js
  lazy val testModule = crossProject(JVMPlatform, JSPlatform)
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
