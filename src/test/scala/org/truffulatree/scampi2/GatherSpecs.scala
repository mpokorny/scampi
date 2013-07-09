//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class GatherSpec extends ScampiSpecification {
  "gather" should {
    "complete" in {
      runTest("GatherSpec0", 3) must beTrue
    }
    "support in-place buffers" in {
      runTest("GatherSpec1", 5) must beTrue
    }
  }
}

object GatherSpec0 extends ScampiApp {
  import mpi2._
  val intval = MpiInt(1)
  intval(0) = (Comm.world.rank + 1) * (Comm.world.rank + 1)
  Comm.world.rank match {
    case 0 => {
      val recvints = MpiInt.alloc(Comm.world.size)
      Comm.world.gather(intval, recvints, 0)
      result = (0 until Comm.world.size).forall(i => {
        recvints(i) == (i + 1) * (i + 1)
      })
    }
    case _ => {
      Comm.world.gather(intval, EmptyBuffer, 0)
      result = true
    }
  }
}

object GatherSpec1 extends ScampiApp {
  import mpi2._
  val intval = MpiInt.alloc(1)
  intval(0) = (Comm.world.rank + 1) * (Comm.world.rank + 1)
  Comm.world.rank match {
    case 0 => {
      val recvints = MpiInt.alloc(Comm.world.size)
      val myint = MpiInt @: recvints
      myint(0) = intval(0)
      Comm.world.gather(ValueInPlace(myint), recvints, 0)
      result = (0 until Comm.world.size).forall(i => {
        recvints(i) == (i + 1) * (i + 1)
      })
    }
    case _ => {
      Comm.world.gather(intval, EmptyBuffer, 0)
      result = true
    }
  }
}
