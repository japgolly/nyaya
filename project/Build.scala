import sbt._
import sbt.Keys._
import com.jsuereth.sbtpgp.PgpKeys
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import pl.project13.scala.sbt.JmhPlugin
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import ScalaJSPlugin.autoImport._
import Dependencies._
import Lib._

object NyayaBuild {
  import sbtcrossproject.CrossPlugin.autoImport._

  private val ghProject = "nyaya"

  private val publicationSettings =
    Lib.publicationSettings(ghProject)

  val isScala2 = settingKey[Boolean]("Is this project compiled with Scala 2.x?")

  def scalacCommonFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
  )

  def scalac2Flags = Seq(
    "-opt:l:inline",
    "-opt-inline-from:scala.**",
    "-opt-inline-from:nyaya.**",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ywarn-value-discard",
  )

  def scalac3Flags = Seq(
    "-source", "3.0-migration",
    "-Ykind-projector",
  )

  val commonSettings = ConfigureBoth(
    _.settings(
      scalaVersion        := Ver.scala2,
      crossScalaVersions  := Seq(Ver.scala2, Ver.scala3),
      scalacOptions      ++= scalacCommonFlags,
      scalacOptions      ++= scalac2Flags.filter(_ => isScala2.value),
      scalacOptions      ++= scalac3Flags.filter(_ => scalaVersion.value.startsWith("3")),
      testFrameworks      := Nil,
      updateOptions       := updateOptions.value.withCachedResolution(true),
      isScala2            := scalaVersion.value startsWith "2.",

      Test / scalacOptions --= Seq("-Ywarn-dead-code"),

      libraryDependencies ++= Seq(
        Dep.betterMonadicFor,
        Dep.kindProjector
      ).filter(_ => isScala2.value),

      // Why?
      Compile / unmanagedSourceDirectories := (Compile / unmanagedSourceDirectories).value.distinct,
      Test / unmanagedSourceDirectories := (Test / unmanagedSourceDirectories).value.distinct,
    )
  )

  def utestSettings = ConfigureBoth(
    _.settings(
      libraryDependencies += Dep.utest.value % Test,
      testFrameworks      := Seq(new TestFramework("utest.runner.Framework"))))
    .jsConfigure(
      // Not mandatory; just faster.
      _.settings(Test / jsEnv := new JSDOMNodeJSEnv()))

  // ==============================================================================================

  lazy val root = (project in file("."))
    .configure(commonSettings.jvm, preventPublication)
    .aggregate(
      utilJVM, propJVM, genJVM, testJVM, genCirceJVM,
      utilJS, propJS, genJS, testJS, genCirceJS,
      benchmark)

  lazy val utilJVM = util.jvm
  lazy val utilJS  = util.js
  lazy val util = crossProject(JVMPlatform, JSPlatform)
    .in(file("util"))
    .configureCross(commonSettings, publicationSettings, utestSettings)
    .settings(
      moduleName := "nyaya-util",
    )

  lazy val propJVM = prop.jvm
  lazy val propJS  = prop.js
  lazy val prop = crossProject(JVMPlatform, JSPlatform)
    .in(file("prop"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(util)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-prop",
      libraryDependencies ++= Seq(
        Dep.cats.value,
        Dep.microlibsMultimap.value,
      ),
    )

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
        Dep.cats.value,
        Dep.microlibsMultimap.value,
        Dep.microlibsRecursion.value,
        Dep.monocleCore.value,
        Dep.alleyCats.value % Test,
      ),
    )

  lazy val genCirceJVM = genCirce.jvm
  lazy val genCirceJS  = genCirce.js
  lazy val genCirce = crossProject(JVMPlatform, JSPlatform)
    .in(file("gen-circe"))
    .configureCross(commonSettings, publicationSettings)
    .dependsOn(gen)
    .configureCross(utestSettings)
    .settings(
      moduleName := "nyaya-gen-circe",
      libraryDependencies ++= Seq(
        Dep.circeCore.value,
      ),
    )

  lazy val testJVM = testModule.jvm
  lazy val testJS  = testModule.js
  lazy val testModule = crossProject(JVMPlatform, JSPlatform)
    .in(file("test"))
      .configureCross(commonSettings, publicationSettings)
      .dependsOn(prop, gen)
      .configureCross(utestSettings)
    .settings(
      name := "test",
      moduleName := "nyaya-test",
    )

  lazy val benchmark = (project in file("benchmark"))
    .enablePlugins(JmhPlugin)
    .configure(commonSettings.jvm, preventPublication)
    .dependsOn(propJVM, genJVM, testJVM)
}
