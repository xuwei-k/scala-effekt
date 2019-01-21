lazy val effektSettings = Seq(
  scalaVersion := "0.13.0-bin-20190120-2ce89da-NIGHTLY", // dottyLatestNightlyBuild.get,
  version := "0.3-SNAPSHOT",
  organization := "de.b-studios",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-language:implicitConversions"
  )
)

lazy val root = project
  .in(file("."))
  .settings(moduleName := "effekt", name := "effekt")
  .settings(effektSettings)
