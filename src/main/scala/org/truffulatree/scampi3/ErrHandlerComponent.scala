//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import org.bridj.Pointer

trait ErrHandlerComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  trait WithErrHandler {
    type ErrHandlerType <: ErrHandler

    protected var currentErrHandler: ErrHandlerType

    def errHandler: ErrHandlerType = currentErrHandler

    def errHandler_=(newErrHandler: ErrHandlerType) {
      mpi3.mpiCall(mpiSetErrhandler(newErrHandler.handle))
      currentErrHandler = newErrHandler
    }
    protected def mpiSetErrhandler(errhandler: mpi3.lib.MPI_Errhandler): Int
  }

  trait ErrHandler {
    protected val handlePtr: Pointer[mpi3.lib.MPI_Errhandler] = {
      val result = mpi3.allocateErrhandler()
      result.set(mpi3.lib.MPI_ERRHANDLER_NULL)
      result
    }

    def handle: mpi3.lib.MPI_Errhandler = handlePtr(0)
  }

  trait UserErrHandler extends ErrHandler {
    override def finalize() {
      lifecycleSync {
        if (!mpi3.finalized)
          mpi3.mpiCall(mpi3.lib.MPI_Errhandler_free(handlePtr))
      }
      super.finalize()
    }
  }
}
