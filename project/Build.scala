//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
import sbt._
import Keys._

import java.io.File
import java.util.jar

object ScampiBuild extends Build {
  val testProperties = TaskKey[Unit](
    "test-properties", "Create a properties file for multi-process tests")

  def testResources(
    dir: File,
    home: Option[File],
    classDir: File,
    depClasspath: Classpath): Seq[File] = {
    // Write paths to "scala" executable, test class directory path, and
    // "pathing jar" into properties file. The pathing jar is needed because pmi
    // does not like arguments that exceed 1024 characters in length (which the
    // required classpath for testing would violate). Note that we use the
    // exported Scampi jar to put the Scampi classes into the classpath, but we
    // use an explicit path to the test class directory because a dependency
    // cycle would appear otherwise in the build (for Test).
    if (!dir.exists) dir.mkdirs()
    val propertiesFile = buildFile(dir, "test.properties")
    val writer = new java.io.FileWriter(propertiesFile)
    val properties = List(
      "command = " + buildFile(home.get, "bin", "scala").getPath,
      "classDir = " + classDir.getPath,
      "pathJarPath = " + buildFile(dir, "path.jar").getPath).mkString("\n")
    writer.write(properties, 0, properties.length)
    writer.close()
    val manifest = new jar.Manifest()
    val attributes = manifest.getMainAttributes
    attributes.put(jar.Attributes.Name.MANIFEST_VERSION, "1.0")
    attributes.put(
      jar.Attributes.Name.CLASS_PATH,
      (depClasspath map (_.data)).mkString(" "))
    val jarFile = buildFile(dir, "path.jar")
    val jarOutputStream = new jar.JarOutputStream(
      new java.io.FileOutputStream(jarFile),
      manifest)
    jarOutputStream.finish()
    jarOutputStream.close()
    Seq(propertiesFile, jarFile)
  }

  lazy val root = Project(
    id = "ScaMPI",
    base = file("."),
    settings = Project.defaultSettings)

  private def buildFile(dir: File, parts: String*): File =
    (dir /: parts) (new File(_, _))
}
