//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import java.io.{File => JFile}

class Ch13Spec extends ScampiSpecification {
  skipAllIf(libraryName == "openmpi")
  "ScaMPI" should {
    "pass example 13.2" in {
      runTest("Ex13_2", 1) must beTrue
    }
    "pass example 13.3" in {
      runTest("Ex13_3", 1) must beTrue
    }
  }
}

object Ex13_2 extends ScampiApp {
  import mpi2._
  val filename = new JFile("ex13_2")
  val bufsize = 100
  val localbuffer = MpiDouble.alloc(bufsize)

  // Write the test file (not in Ex 13.2)
  val numbuffers = 100
  val tfh =
    new File(Comm.world, filename, List(FileMode.WriteOnly, FileMode.Create))
  tfh.view = FileView(0, MpiDouble, MpiDouble, "native")
  for (i <- 0 until numbuffers) {
    for (j <- 0 until bufsize) localbuffer(j) = i * bufsize + j
    tfh.doWrite(localbuffer)
  }
  tfh.close()

  // Read the test file (in Ex 13.2)
  val myfh = new File(
    Comm.world,
    filename,
    List(FileMode.ReadOnly, FileMode.DeleteOnClose))
  myfh.view = FileView(0, MpiDouble, MpiDouble, "native")
  var totprocessed = 0
  var numread = bufsize
  while (numread >= bufsize) {
    val status = myfh.read(localbuffer)
    numread = status.count(MpiDouble).getOrElse(0)
    totprocessed += numread
  }
  myfh.close()
  result = totprocessed == (numbuffers * bufsize)
}

object Ex13_3 extends ScampiApp {
  import mpi2._
  val filename = new JFile("ex13_3")
  val bufsize = 10
  val buf1 = MpiInt.alloc(bufsize)
  val buf2 = MpiInt.alloc(bufsize)

  // Write the test file (not in Ex 13.3)
  val numreps = 100
  val tfh =
    new File(Comm.world, filename, List(FileMode.WriteOnly, FileMode.Create))
  tfh.view = FileView(0, MpiInt, MpiInt, "native")
  for (i <- 0 until numreps) {
    for (j <- 0 until bufsize) {
      buf1(j) = 2 * (i * bufsize + j)
      buf2(j) = buf1(j) + 1
    }
    tfh.doWrite(buf1)
    tfh.doWrite(buf2)
  }
  tfh.close()

  // Read the test file (in Ex 13.3)
  val myfh = new File(
    Comm.world,
    filename,
    List(FileMode.ReadOnly, FileMode.DeleteOnClose))
  myfh.view = FileView(0, MpiInt, MpiInt, "native")
  result = true
  for (i <- 0 until numreps) {
    val req1 = myfh.iread(buf1)
    val req2 = myfh.iread(buf2)
    Request.doWaitAll(List(req1, req2))
    result = (result &&
      buf1 == ((0 until bufsize) map ((j: Int) => 2 * (i * bufsize + j))) &&
        buf2 == ((0 until bufsize) map ((j: Int) => 2 * (i * bufsize + j) + 1)))
  }
  myfh.close()
}
