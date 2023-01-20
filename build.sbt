ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "scala-macros-example",
    idePackagePrefix := Some("de.codecentric")
  )

lazy val macros = (project in file("macros"))
  .settings(
    name := "macros",
    idePackagePrefix := Some("de.codecentric"),
    scalacOptions := Seq("-Ymacro-annotations"),
    libraryDependencies := Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val examples = (project in file("examples"))
  .settings(
    name := "examples",
    idePackagePrefix := Some("de.codecentric"),
    scalacOptions := Seq("-Ymacro-annotations"),
    libraryDependencies := Seq(
      "com.google.guava" % "guava" % "31.1-jre"
    )
  )
  .dependsOn(macros)

