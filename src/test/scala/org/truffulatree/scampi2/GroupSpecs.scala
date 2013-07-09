//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class GroupSpec extends ScampiSpecification {
  implicit def intToGroup(i: Int): mpi2.lib.MPI_Group =
    i.asInstanceOf[mpi2.lib.MPI_Group]

  "Group" should {
    "have the proper handle attribute value" in {
      mpi2.Group(42).handle must_== 42
    }
    "exhibit value equality" in {
      mpi2.Group(10) must_== mpi2.Group(10)
    }
    "have GroupEmpty correspond to MPI_GROUP_EMPTY" in {
      mpi2.GroupEmpty.handle must_== mpi2.lib.MPI_GROUP_EMPTY
    }
    "not support instantiation of MPI_GROUP_NULL" in {
      mpi2.Group(mpi2.lib.MPI_GROUP_NULL) must throwA[mpi2.Exception]
    }
  }
}
