//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class GroupSpec extends ScampiSpecification {
  "Group" should {
    "have the proper handle attribute value" in {
      val g = mpi2.Comm.self.dup.group
      mpi2.Group(g.handle).handle must_== g.handle
    }
    "exhibit value equality" in {
      val g = mpi2.Comm.self.dup.group
      mpi2.Group(g.handle) must_== mpi2.Group(g.handle)
    }
    "have GroupEmpty correspond to MPI_GROUP_EMPTY" in {
      mpi2.GroupEmpty.handle must_== mpi2.lib.MPI_GROUP_EMPTY
    }
    "not support instantiation of MPI_GROUP_NULL" in {
      mpi2.Group(mpi2.lib.MPI_GROUP_NULL) must throwA[mpi2.Exception]
    }
  }
}
