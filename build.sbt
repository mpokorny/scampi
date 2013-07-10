// -*- mode: scala -*-
//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
name := "ScaMPI"

version := "0.2.0-SNAPSHOT"

organization := "org.truffulatree"

scalaVersion := "2.10.2"

scalaHome in Test := Some(file("/opt/scala-2.10.2"))

// We need the export jars in the build because of the way we pass the classpath
// to spawned processes (see project/Build.sbt)
exportJars := true

libraryDependencies ++= Seq(
  "com.nativelibs4java" % "bridj" % "0.6.2",
  "org.specs2" %% "specs2" % "1.14" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test")

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "https://oss.sonatype.org/content/repositories/releases")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-explaintypes",
  "-Xlint")

compileOrder := CompileOrder.JavaThenScala

initialCommands in console := """|val mpi2 = org.truffulatree.scampi2.mpi2;
                                 |mpi2.init()""".stripMargin

resourceGenerators in Test <+= (
  resourceManaged in Test,
  scalaHome in Test,
  classDirectory in Test,
  dependencyClasspath in Test) map {
    (dir, home, cd, cp) => testResources(dir, home, cd, cp)
  }

testOptions in Test += Tests.Filter(_.endsWith("Spec"))
