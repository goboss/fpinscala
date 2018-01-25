lazy val commonSettings = Seq(
  scalaVersion := "2.12.4",
  projectDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
  )
)

lazy val exercises = (project in file("exercises"))
  .settings(
    commonSettings,
    name := "exercises"
  )

lazy val chaptercode = (project in file("chaptercode"))
  .settings(
    commonSettings,
    name := "chaptercode"
  )

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "fpinscala"
  )
  .aggregate(exercises, chaptercode)