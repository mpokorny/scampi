//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import org.bridj.Pointer

trait ThreadsComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  def queryThread: mpi3.ThreadLevel.ThreadLevel =
    withOutVar { provided: Pointer[Int] =>
      mpi3.mpiCall(mpi3.lib.MPI_Query_thread(provided))
      mpi3.ThreadLevel(provided(0))
    }

  def isThreadMain: Boolean = withOutVar { flag: Pointer[Int] =>
    mpi3.mpiCall(mpi3.lib.MPI_Is_thread_main(flag))
    flag(0) != 0
  }
}
