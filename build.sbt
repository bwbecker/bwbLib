
organization := "ca.bwbecker"

name := "bwbLib"

version := "1.5-SNAPSHOT"

scalaVersion := "2.12.9"
  
crossScalaVersions := Seq("2.11.8", "2.12.9")

// See https://blog.threatstack.com/useful-scalac-options-for-better-scala-development-part-1
scalacOptions ++= Seq(
  "-feature",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)


libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.7.1" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")
