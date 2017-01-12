name := "scala"

version := "1.0"

scalaVersion := "2.11.8"

lazy val root = project.in(file(".")).
  aggregate(purelispJS, purelispJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val purelisp = crossProject.in(file(".")).
  settings(
    name := "yet-another-lisp",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "fastparse" % "0.4.1",
      "com.lihaoyi" %%% "utest" % "0.4.3" % "test",
      "org.typelevel" %%% "cats" % "0.8.1"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val purelispJVM = purelisp.jvm
lazy val purelispJS = purelisp.js
