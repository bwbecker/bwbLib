
organization := "ca.bwbecker"

name := "bwbLib"

version := "1.6-SNAPSHOT"

scalaVersion := "2.12.11"
  
crossScalaVersions := Seq("2.12.11")

// See https://blog.threatstack.com/useful-scalac-options-for-better-scala-development-part-1
scalacOptions ++= Seq(
  "-feature",
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
//  "-Yno-adapted-args"
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen"
//  "-Ywarn-unused"
)


libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.7.4"
)

testFrameworks += new TestFramework("utest.runner.Framework")
