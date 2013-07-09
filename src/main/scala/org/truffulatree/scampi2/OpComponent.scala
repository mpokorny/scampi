//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait OpComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  class Op protected[scampi2](op: mpi2.lib.MPI_Op=mpi2.lib.MPI_OP_NULL) {

    protected final val handlePtr: Pointer[mpi2.lib.MPI_Op] = {
      val result = mpi2.allocateOp()
      result(0) = op
      result
    }

    final def handle = handlePtr(0)

    def free() { if (!isNull) mpi2.mpiCall(mpi2.lib.MPI_Op_free(handlePtr)) }

    def isNull = handle == mpi2.lib.MPI_OP_NULL

    override def finalize() {
      mpi2.lifecycleSync { if (!mpi2.finalized) free() }
      super.finalize()
    }

  }

  object Op {
    def apply(fn: mpi2.lib.MPI_User_function, commute: Boolean): Op = {
      val result = new Op
      mpi2.mpiCall(
        mpi2.lib.MPI_Op_create(
          Pointer.pointerTo(fn),
          if (commute) 1 else 0,
          result.handlePtr))
      result
    }

    val max = new Op(mpi2.lib.MPI_MAX)
    val min = new Op(mpi2.lib.MPI_MIN)
    val sum = new Op(mpi2.lib.MPI_SUM)
    val product = new Op(mpi2.lib.MPI_PROD)
    val logicalAnd = new Op(mpi2.lib.MPI_LAND)
    val bitwiseAnd = new Op(mpi2.lib.MPI_BAND)
    val logicalOr = new Op(mpi2.lib.MPI_LOR)
    val bitwiseOr = new Op(mpi2.lib.MPI_BOR)
    val logicalXor = new Op(mpi2.lib.MPI_LXOR)
    val bitwiseXor = new Op(mpi2.lib.MPI_BXOR)
    val minLoc = new Op(mpi2.lib.MPI_MINLOC)
    val maxLoc = new Op(mpi2.lib.MPI_MAXLOC)
    val replace = new Op(mpi2.lib.MPI_REPLACE)
  }

}
