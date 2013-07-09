//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait ThreadsComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  def queryThread: mpi2.ThreadLevel.ThreadLevel =
    withOutVar { provided: Pointer[Int] =>
      mpi2.mpiCall(mpi2.lib.MPI_Query_thread(provided))
      mpi2.ThreadLevel(provided(0))
    }

  def isThreadMain: Boolean = withOutVar { flag: Pointer[Int] =>
    mpi2.mpiCall(mpi2.lib.MPI_Is_thread_main(flag))
    flag(0) != 0
  }
}
