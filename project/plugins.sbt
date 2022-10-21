addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"             % "0.9.34")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.10")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.11.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.3")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"
