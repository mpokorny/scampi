//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class CommSpec extends ScampiSpecification {
  "Comm" should {
    "have the proper handle attribute value" in {
      val c = mpi2.Comm.self.dup
      mpi2.Comm(c.handle).handle must_== c.handle
    }
    "exhibit value equality" in {
      val c = mpi2.Comm.self.dup
      mpi2.Comm(c.handle) must_== mpi2.Comm(c.handle)
    }
    "have Comm.world correspond to MPI_COMM_WORLD" in {
      mpi2.Comm.world.handle must_== mpi2.lib.MPI_COMM_WORLD
    }
    "have Comm.self correspond to MPI_COMM_SELF" in {
      mpi2.Comm.self.handle must_== mpi2.lib.MPI_COMM_SELF
    }
    "not support instantiation of MPI_COMM_NULL" in {
      mpi2.Comm(mpi2.lib.MPI_COMM_NULL) must throwA[mpi2.Exception]
    }
  }
}
