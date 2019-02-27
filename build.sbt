lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "fi.kapsi.kosmik",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "Functional Programming in Scala - Exercises",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
      scalaTest % Test,
      "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % Test
    )
  )
