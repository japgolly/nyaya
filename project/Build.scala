import sbt._
import sbt.Keys._
import com.jsuereth.sbtpgp.PgpKeys
import dotty.tools.sbtplugin.DottyPlugin.autoImport._
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
    val KindProjector    = "0.11.3"
    val Monocle          = "1.6.3"
    val MTest            = "0.7.7"
    val Scala212         = "2.12.13"
    val Scala213         = "2.13.5"
    val Scala3           = "3.0.0-RC1"
    val ScalaCollCompat  = "2.4.2"
    val Scalaz           = "7.2.31"
  }

  object Dep {
    val MonocleCore     = Def.setting("com.github.julien-truffaut" %%% "monocle-core"            % Ver.Monocle         withDottyCompat scalaVersion.value)
    val MTest           = Def.setting("com.lihaoyi"                %%% "utest"                   % Ver.MTest                                             )
    val ScalaCollCompat = Def.setting("org.scala-lang.modules"     %%% "scala-collection-compat" % Ver.ScalaCollCompat                                   )
    val Scalaz          = Def.setting("org.scalaz"                 %%% "scalaz-core"             % Ver.Scalaz          withDottyCompat scalaVersion.value)

    // Compiler plugins
    val BetterMonadicFor = compilerPlugin("com.olegpy"    %% "better-monadic-for" % Ver.BetterMonadicFor)
    val KindProjector    = compilerPlugin("org.typelevel" %% "kind-projector"     % Ver.KindProjector cross CrossVersion.full)
  }

  val isScala2 = settingKey[Boolean]("Is this project compiled with Scala 2.x?")

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
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213, Ver.Scala3),
      testFrameworks                := Nil,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      libraryDependencies           += Dep.ScalaCollCompat.value,
      isScala2                      := scalaVersion.value startsWith "2.",

      scalacOptions ++= Seq(
        "-deprecation",
        "-unchecked",
        "-feature",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:higherKinds",
        "-language:existentials"
      ),

      scalacOptions ++= Seq(
        "-opt:l:inline",
        "-opt-inline-from:scala.**",
        "-opt-inline-from:nyaya.**",
        "-Ywarn-dead-code",
        "-Ywarn-value-discard"
      ).filter(_ => isScala2.value),

      scalacOptions ++= Seq(
        "-source", "3.0-migration",
        "-Ykind-projector",
      ).filter(_ => isDotty.value),

      Test / scalacOptions --= Seq("-Ywarn-dead-code"),

      libraryDependencies ++= Seq(
        Dep.BetterMonadicFor,
        Dep.KindProjector
      ).filter(_ => isScala2.value)
    )
  )

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += Dep.MTest.value % Test,
      testFrameworks      := Seq(new TestFramework("utest.runner.Framework"))))
    .jsConfigure(
      // Not mandatory; just faster.
      _.settings(jsEnv in Test := new JSDOMNodeJSEnv()))

  def crossProjectScalaDirs: CPE =
    _.settings(
      unmanagedSourceDirectories in Compile ++= {
        val root   = (baseDirectory in Compile).value / ".."
        val shared = root / "shared" / "src" / "main"
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, _))  => Seq(shared / "scala-3")
          case Some((2, _))  => Seq(shared / "scala-2")
          case _             => Nil
        }
      },
      unmanagedSourceDirectories in Test ++= {
        val root   = (baseDirectory in Test).value / ".."
        val shared = root / "shared" / "src" / "test"
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, _))  => Seq(shared / "scala-3")
          case Some((2, _))  => Seq(shared / "scala-2")
          case _             => Nil
        }
      }
    )

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
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings, utestSettings)
    .settings(
      moduleName := "nyaya-util",
      libraryDependencies += Dep.Scalaz.value)

  lazy val propJVM = prop.jvm
  lazy val propJS  = prop.js
  lazy val prop = crossProject(JVMPlatform, JSPlatform)
    .in(file("prop"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-prop",
      libraryDependencies += Dep.Scalaz.value)

  lazy val genJVM = gen.jvm
  lazy val genJS  = gen.js
  lazy val gen = crossProject(JVMPlatform, JSPlatform)
    .in(file("gen"))
    .configureCross(commonSettings, crossProjectScalaDirs, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-gen",
      libraryDependencies ++= Seq(
        Dep.Scalaz.value,
        Dep.MonocleCore.value))

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
