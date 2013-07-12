//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.{Pointer, CLong}
import scala.util.Random

class ScampiSpec extends ScampiSpecification {
  // "initialization" should {
  //   "succeed" in {
  //     init()
  //     initialized must_== true
  //   }
  // }
  // "finalization" should {
  //   "succeed" in {
  //     mpiFinalize()
  //     finalized must_== true
  //   }
  // }
  "conditional initialization" should {
    "succeed" in {
      if (!mpi2.initialized && !mpi2.finalized)
        mpi2.initThread(mpi2.ThreadLevel.Multiple)
      (mpi2.initialized && !mpi2.finalized) must_== true
    }
    "complete isThreadMain" in {
      mpi2.isThreadMain must beOneOf(true, false)
    }
  }
  "getAddress" should {
    val bottom = Pointer.getPeer(mpi2.lib.MPI_BOTTOM)
    "conform to MPI_BOTTOM" in {
      mpi2.getAddress(mpi2.lib.MPI_BOTTOM) must_== mpi2.aintFromLong(bottom)
    }
    "return addresses" in {
      val noRelease = new Pointer.Releaser {
        def release(p: Pointer[_]) {}
      }
      forall(0 until 1000) {
        _ => {
          val a = bottom + Random.nextLong
          val p = Pointer.pointerToAddress(a, 1, noRelease)
          mpi2.getAddress(p) must_== mpi2.aintFromLong(a)
        }
      }
    }
  }
  // "conditional finalization" should {
  //   "succeed" in {
  //     if (initialized && !finalized)
  //       mpiFinalize()
  //     (!initialized || finalized) must_== true
  //   }
  // }
}
