//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import org.bridj.Pointer

trait MessageComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  final class Message {
    protected[scampi3] final val handlePtr: Pointer[mpi3.lib.MPI_Message] = {
      val result = mpi3.allocateMessage()
      result(0) = mpi3.lib.MPI_MESSAGE_NULL
      result
    }

    def handle = handlePtr(0)

    def isNull: Boolean = handle == mpi3.lib.MPI_MESSAGE_NULL

    def fromNoProc: Boolean = handle == mpi3.lib.MPI_MESSAGE_NO_PROC
  }

}
