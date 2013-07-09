//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class P2PSpec extends ScampiSpecification {
  "send/recv" should {
    "complete" in {
      runTest("P2PSpecsRecv", 3) must beTrue
    }
  }
}

object P2PSpecsRecv extends ScampiApp {
  val intval = mpi2.MpiInt.alloc(1)
  val testval = 42
  mpi2.Comm.world.rank match {
    case 0 => {
      intval(0) = testval
      mpi2.Comm.world.send(intval, 1, 0)
      mpi2.Comm.world.doRecv(intval, 2, 0)
    }
    case 1 => {
      mpi2.Comm.world.doRecv(intval, 0, 0)
      mpi2.Comm.world.send(intval, 2, 0)
    }
    case 2 => {
      mpi2.Comm.world.doRecv(intval, 1, 0)
      mpi2.Comm.world.send(intval, 0, 0)
    }
  }
  result = (intval(0) == testval)
}
