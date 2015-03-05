import sbt._
import Keys._

import com.typesafe.sbt.pgp.PgpKeys._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin._
import ScalaJSPlugin.autoImport._

object Nyaya extends Build {

  val Scala211 = "2.11.5"

  type PE = Project => Project

  sealed abstract class Dialect(val name: String)
  case object JVM extends Dialect("jvm")
  case object JS  extends Dialect("js")

  def commonSettings(d: Option[Dialect]): PE = {
    def jvmSettings: PE = identity
    def jsSettings : PE = _.enablePlugins(ScalaJSPlugin).configure(sourceMapsToGithub)
    def aggSettings: PE = preventPublication

    _.settings(
      organization       := "com.github.japgolly.nyaya",
      version            := "0.5.9-SNAPSHOT",
      homepage           := Some(url("https://github.com/japgolly/nyaya")),
      licenses           += ("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")),
      scalaVersion       := Scala211,
      crossScalaVersions := Seq("2.10.4", Scala211),
      scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature",
                              "-language:postfixOps", "-language:implicitConversions",
                              "-language:higherKinds", "-language:existentials"),
      updateOptions      := updateOptions.value.withCachedResolution(true))
    .configure(
      d.map(_.name).fold[PE](identity)(addSourceDialect),
      addCommandAliases(
        "t"  -> "; test:compile ; test",
        "tt" -> ";+test:compile ;+test",
        "T"  -> "; clean ;t",
        "TT" -> ";+clean ;tt"),
      d match {case None => aggSettings; case Some(JVM) => jvmSettings; case Some(JS) => jsSettings})
  }

  def addSourceDialect(name: String): PE =
    _.settings(unmanagedSourceDirectories in Compile += baseDirectory.value / s"src/main/scala-$name")

  def preventPublication: PE =
    _.settings(
      publish := (),
      // dummy to make SBT shut up http://stackoverflow.com/a/18522706/871202
      publishTo := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
      publishArtifact := false,
      publishLocalSigned := (),       // doesn't work
      publishSigned := (),            // doesn't work
      packagedArtifacts := Map.empty) // doesn't work - https://github.com/sbt/sbt-pgp/issues/42

  def publicationSettings: PE =
    _.settings(
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },
      pomExtra :=
        <scm>
          <connection>scm:git:github.com/japgolly/nyaya</connection>
          <developerConnection>scm:git:git@github.com:japgolly/nyaya.git</developerConnection>
          <url>github.com:japgolly/nyaya.git</url>
        </scm>
        <developers>
          <developer>
            <id>japgolly</id>
            <name>David Barri</name>
          </developer>
        </developers>)

  def sourceMapsToGithub: PE =
    p => p.settings(
      scalacOptions ++= (if (isSnapshot.value) Seq.empty else Seq({
        val a = p.base.toURI.toString.replaceFirst("[^/]+/?$", "")
        val g = "https://raw.githubusercontent.com/japgolly/nyaya"
        s"-P:scalajs:mapSourceURI:$a->$g/v${version.value}/"
      }))
    )

  def utestSettings(d: Dialect, scope: String = "test"): PE = {
    val utestVer = "0.3.0"
    val a: PE = _.settings(
      testFrameworks += new TestFramework("utest.runner.Framework")
    )
    val b: PE = d match {
      case JVM => _.settings(
                    libraryDependencies += "com.lihaoyi" %% "utest" % utestVer % scope)
      case JS  => _.settings(
                    libraryDependencies += "com.lihaoyi" %%%! "utest" % utestVer % scope,
                    scalaJSStage in Test := FastOptStage)
    }
    a compose b
  }

  def addCommandAliases(m: (String, String)*) = {
    val s = m.map(p => addCommandAlias(p._1, p._2)).reduce(_ ++ _)
    (_: Project).settings(s: _*)
  }

  def crossDialect(shortName: String, common: PE, specific: Dialect => PE) = {
    val pname, dir = s"nyaya-$shortName"
    def mk(d: Dialect) = {
      val n = s"$pname-${d.name}"
      Project(n, file(dir))
        .configure(commonSettings(Some(d)))
        .configure(common, specific(d))
        .settings(
          name       := n,
          moduleName := pname,
          target     := baseDirectory.value / "target" / d.name)
    }
    lazy val jvm = mk(JVM)
    lazy val js  = mk(JS)
    lazy val agg = Project(pname, file("."))
                     .aggregate(jvm, js)
                     .configure(commonSettings(None))
                     .settings(target := file(dir) / "target")
    (agg, jvm, js)
  }

  object vers {
    val scalaz  = "7.1.1"
    val monocle = "1.0.1"
    val rng     = "1.3.0"

    val scalazJs  = scalaz  + "-2"
    val monocleJs = monocle + "-2"
    val rngJs     = rng     + "-2"
  }

  // ==============================================================================================
  override def rootProject = Some(root)

  lazy val root = Project("root", file(".")) .configure(commonSettings(None))
    .aggregate(core, ntest)

  // lazy val allJvm = Project("jvm", file(".")) .configure(commonSettings(None))
    // .aggregate(coreJvm, ntestJvm)
  // lazy val allJs = Project("js", file(".")) .configure(commonSettings(None))
    // .aggregate(coreJs, ntestJs)

  lazy val (core, coreJvm, coreJs) = crossDialect("core",
    _.configure(publicationSettings),
    d => utestSettings(d) compose (d match {
      case JVM =>
        _.settings(libraryDependencies ++= Seq(
          "org.scalaz" %% "scalaz-core" % vers.scalaz))
      case JS  =>
        _.settings(libraryDependencies ++= Seq(
          "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % vers.scalazJs))
    }))

  lazy val (ntest, ntestJvm, ntestJs) = crossDialect("test",
    _.configure(publicationSettings),
    d => utestSettings(d, "compile") compose (d match {
      case JVM =>
        _.dependsOn(coreJvm)
          .settings(libraryDependencies ++= Seq(
            "com.github.julien-truffaut" %% "monocle-core"  % vers.monocle,
            "com.github.julien-truffaut" %% "monocle-macro" % vers.monocle % "test",
            "com.nicta"                  %% "rng"           % vers.rng))
      case JS  =>
        _.dependsOn(coreJs)
          .settings(libraryDependencies ++= Seq(
            "com.github.japgolly.fork.monocle" %%% "monocle-core"  % vers.monocleJs,
            "com.github.japgolly.fork.monocle" %%% "monocle-macro" % vers.monocleJs % "test",
            "com.github.japgolly.fork.nicta"   %%% "rng"           % vers.rngJs))
    }))
}
