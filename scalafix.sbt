ThisBuild / semanticdbEnabled := true
// ThisBuild / semanticdbVersion := "4.5.13"

ThisBuild / scalacOptions ++= {
  if (scalaVersion.value startsWith "2")
    "-Yrangepos" :: Nil
  else
    Nil
}
