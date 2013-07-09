//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait Errors {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  def errorClass(errorcode: Int): Int = withOutVar { result: Pointer[Int] =>
    mpi2.mpiCall(mpi2.lib.MPI_Error_class(errorcode, result))
    result(0)
  }

  def errorString(errorcode: Int): String =
    getString(mpi2.lib.MPI_MAX_ERROR_STRING) { (len, buffer) =>
      withOutVar { strlen: Pointer[Int] =>
        mpi2.mpiCall(mpi2.lib.MPI_Error_string(errorcode, buffer, strlen))
      }
    }

  def addErrorClass: Int = withOutVar { result: Pointer[Int] =>
    mpi2.mpiCall(mpi2.lib.MPI_Add_error_class(result))
    result(0)
  }

  def addErrorCode(errorclass: Int): Int = withOutVar { result: Pointer[Int] =>
    mpi2.mpiCall(mpi2.lib.MPI_Add_error_code(errorclass, result))
    result(0)
  }

  def addErrorString(errorcode: Int, string: String) {
    require(
      errorcode > mpi2.lib.MPI_ERR_LASTCODE,
      s"Error string for ${errorcode} cannot be set")
    mpi2.mpiCall(
      mpi2.lib.MPI_Add_error_string(
        errorcode,
        Pointer.pointerToCString(string).as(classOf[Byte])))
  }

  val indexOutOfRangeErrorMsg = "Index out of range"

  val invalidValueErrorMsg = "Invalid value"

  val bufferCompatibilityErrorMsg =
    "Datatype is incompatible with provided buffer and offset"

  val countExceedsLengthErrorMsg =
    "Element count exceeds buffer length"

  val numBlocksUnequalToSizeErrorMsg =
    "Number of blocks not equal to communicator size"

  val structBlocksAlignmentErrorMsg =
    "Displacements in StructBlock are improperly aligned"

  val scatterBufferSizeErrorMsg =
    "Scatter buffer size is incompatible with communicator size"

  val gatherBufferSizeErrorMsg =
    "Gather buffer size is incompatible with communicator size"

  val reduceBufferLengthErrorMsg =
    "Unequal buffer lengths in call to reduction function"

  val structBlockLengthErrorMsg =
    "StructBlock length must be non-negative"
}
