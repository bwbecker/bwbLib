
organization := "ca.bwbecker"

name := "bwbLib"

version := "1.2"

scalaVersion := "2.11.8"


libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.4.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")
