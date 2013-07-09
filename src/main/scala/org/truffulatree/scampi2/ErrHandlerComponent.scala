//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait ErrHandlerComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  trait WithErrHandler {
    type ErrHandlerType <: ErrHandler

    protected var currentErrHandler: ErrHandlerType

    def errHandler: ErrHandlerType = currentErrHandler

    def errHandler_=(newErrHandler: ErrHandlerType) {
      mpi2.mpiCall(mpiSetErrhandler(newErrHandler.handle))
      currentErrHandler = newErrHandler
    }
    protected def mpiSetErrhandler(errhandler: mpi2.lib.MPI_Errhandler): Int
  }

  trait ErrHandler {
    protected val handlePtr: Pointer[mpi2.lib.MPI_Errhandler] = {
      val result = mpi2.allocateErrhandler()
      result.set(mpi2.lib.MPI_ERRHANDLER_NULL)
      result
    }

    def handle: mpi2.lib.MPI_Errhandler = handlePtr(0)
  }

  trait UserErrHandler extends ErrHandler {
    override def finalize() {
      lifecycleSync {
        if (!mpi2.finalized)
          mpi2.mpiCall(mpi2.lib.MPI_Errhandler_free(handlePtr))
      }
      super.finalize()
    }
  }
}
