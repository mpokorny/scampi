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

  lazy val root = Project(
    id = "ScaMPI",
    base = file("."),
    settings = Project.defaultSettings :+ (
      (testProperties in Test) <<=
        (resourceDirectory in Test, fullClasspath in Test, scalaHome) map { (dir, cp, home) =>
          // Write path to "scala" executable and path to "pathing jar" into
          // properties file. The pathing jar is needed because 1) pmi does not
          // like arguments that exceed 1024 characters in length (which the
          // required classpath for testing would violate), and 2) relying on
          // the pathing jar that's put into the test-classes jar by sbt doesn't
          // work when starting scala (java). Finally, note that we need to
          // build the main and test jars during the build because apparently
          // only jars work in the pathing jar's "Class-Path" attribute (see
          // exportJars setting in ../build.sbt).
          val propertiesFile = buildFile(dir, "test.properties")
          val writer = new java.io.FileWriter(propertiesFile)
          val properties = List(
            "command = " + buildFile(home.get, "bin", "scala").getPath,
            "pathJarPath = " + buildFile(dir, "path.jar").getPath).mkString("\n")
          writer.write(properties, 0, properties.length)
          writer.close()
          val manifest = new jar.Manifest()
          val attributes = manifest.getMainAttributes
          attributes.put(jar.Attributes.Name.MANIFEST_VERSION, "1.0")
          attributes.put(
            jar.Attributes.Name.CLASS_PATH,
            (cp map (_.data)).mkString(" "))
          val jarFile = buildFile(dir, "path.jar")
          val jarOutputStream = new jar.JarOutputStream(
            new java.io.FileOutputStream(jarFile),
            manifest)
          jarOutputStream.finish()
          jarOutputStream.close()
        }))

  private def buildFile(dir: File, parts: String*): File =
    (dir /: parts) (new File(_, _))
}
