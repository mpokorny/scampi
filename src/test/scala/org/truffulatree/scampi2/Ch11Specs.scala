//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.util.Random
import scala.collection.mutable

class Ch11Spec extends ScampiSpecification {
  skipAllIf(libraryName == "openmpi")
  "ScaMPI" should {
    "pass example 11.1" in {
      runTest("Ex11_1", 4) must beTrue
    }
    "pass example 11.2" in {
      runTest("Ex11_2", 4) must beTrue
    }
    "pass example 11.3" in {
      runTest("Ex11_3", 4) must beTrue
    }
  }
}

object Ex11_1 extends ScampiApp {
  import mpi2._
  val m = 10
  val globalMap = MpiInt(Random.shuffle((0 until (m * Comm.world.size)).toList):_*)
  val map = MpiInt.alloc(m)
  Comm.world.scatter(globalMap, m, map, 0)
  val a = MpiInt.alloc(m)
  val b = MpiInt.alloc(m)
  for (i <- 0 until m) b(i) = m * Comm.world.rank + i

  // This part does the work that depends on the locations of B. Can
  // be reused while this does not change

  val win = Comm.world.winCreate(
    b.pointer,
    b.region.size,
    b.datatype.extent.range.toInt,
    InfoNull)

  // This part does the work that depends on the value of map and the
  // locations of the arrays. Can be reused while these do not change

  // Compute number of entries to be received from each process

  val count = (new mutable.ArrayBuffer[Int]()).padTo(Comm.world.size, 0)
  for (v <- map) count(v / m) += 1
  val total = count.scanLeft(0)(_ + _)
  for (p <- 0 until count.length) count(p) = 0

  // compute origin and target indices of entries.
  // entry i at current process is received from location
  // k at process (j-1), where map(i) = (j-1)*m + (k-1),
  // j = 1..p and k = 1..m

  val oindex = (new mutable.ArrayBuffer[Int]()).padTo(m, 0)
  val tindex = (new mutable.ArrayBuffer[Int]()).padTo(m, 0)
  for (i <- 0 until m) {
    val j = map(i) / m
    val off = total(j) + count(j)
    oindex(off) = i
    tindex(off) = map(i) % m
    count(j) += 1
  }

  // create origin and target datatypes for each get operation
  val types = List(oindex, tindex) map { index =>
    (0 until Comm.world.size) map { p =>
      val dt = new IndexedBlockDatatype(
        MpiInt,
        1,
        index.slice(total(p), total(p) + count(p)))
      dt.commit()
      dt
    }
  }

  // this part does the assignment itself
  win.fenceFor() { w =>
    (0 until Comm.world.size) foreach { p =>
      w.get((types(0)(p) * 1) @: a, Target(p, 0, 1, types(1)(p)))
    }
  }
  win.free()

  // test the contents of 'a', which should be the same as 'map'
  result = a.equals(map)
}

object Ex11_2 extends ScampiApp {
  import mpi2._
  val m = 10
  val globalMap = MpiInt(Random.shuffle((0 until (m * Comm.world.size)).toList):_*)
  val map = MpiInt.alloc(m)
  Comm.world.scatter(globalMap, m, map, 0)
  val a = MpiInt.alloc(m)
  val b = MpiInt.alloc(m)
  for (i <- 0 until m) b(i) = m * Comm.world.rank + i

  val win = Comm.world.winCreate(
    b.pointer,
    b.region.size,
    b.datatype.extent.range.toInt,
    InfoNull)

  win.fenceFor() { w =>
    for (i <- 0 until m) {
      val j = map(i) / m
      val k = map(i) % m
      w.get((MpiInt * 1) @: (a + i), Target(j, k.toLong, 1, MpiInt))
    }
  }
  win.free()

  // test the contents of 'a', which should be the same as 'map'
  result = a.equals(map)
}

object Ex11_3 extends ScampiApp {
  import mpi2._
  val m = 10
  val globalMap = MpiInt(Random.shuffle((0 until (m * Comm.world.size)).toList):_*)
  val map = MpiInt.alloc(m)
  Comm.world.scatter(globalMap, m, map, 0)
  val a = MpiInt.alloc(m)
  val b = MpiInt.alloc(m)
  for (i <- 0 until m) a(i) = map(i)

  val win = Comm.world.winCreate(
    b.pointer,
    b.region.size,
    b.datatype.extent.range.toInt,
    InfoNull)

  win.fenceFor() { w =>
    for (i <- 0 until m) {
      val j = map(i) / m
      val k = map(i) % m
      w.accumulate(
        (MpiInt * 1) @: (a + i),
        Target(j, k.toLong, 1, MpiInt),
        Op.sum)
    }
  }
  win.free()

  // test the contents of 'b', which should be the sequence
  // {m * rank, ... m * rank + m - 1}
  result = b.equals((0 until m).map(_ + m * Comm.world.rank))
}
