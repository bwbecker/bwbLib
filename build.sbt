
organization := "ca.bwbecker"

name := "bwbLib"

version := "1.3"

scalaVersion := "2.11.8"


libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.4.5" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")
