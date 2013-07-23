//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import org.bridj.Pointer

trait StatusComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  class Status protected[scampi3] (libStatus: mpi3.lib.MPI_Status) {
    def source: Int = libStatus.MPI_SOURCE

    def source_=(x: Int) { libStatus.MPI_SOURCE = x }

    def tag: Int = libStatus.MPI_TAG

    def tag_=(x: Int) { libStatus.MPI_TAG = x }

    def error: Int = libStatus.MPI_ERROR

    def error_=(x: Int) { libStatus.MPI_ERROR = x }

    def count(datatype: mpi3.Datatype[_]): Option[Int] =
      withOutVar { result: Pointer[Int] =>
        mpi3.lib.MPI_Get_count(
          Pointer.pointerTo(libStatus),
          datatype.handle,
          result)
        if (result(0) != mpi3.lib.MPI_UNDEFINED) Some(result(0))
        else None
      }

    def cancelled: Boolean = withOutVar { flag: Pointer[Int] =>
      mpi3.lib.MPI_Test_cancelled(Pointer.pointerTo(libStatus), flag)
      flag(0) != 0
    }

    def cancelled_=(x: Boolean) {
      mpi3.lib.MPI_Status_set_cancelled(
        Pointer.pointerTo(libStatus),
        if (x) 1 else 0)
    }

    def getElements(datatype: mpi3.Datatype[_]): Int =
      withOutVar { result: Pointer[Int] =>
        mpi3.lib.MPI_Get_elements(
          Pointer.pointerTo(libStatus), datatype.handle,
          result)
        result(0)
      }

    def setElements(datatype: mpi3.Datatype[_], x: Int) {
      mpi3.lib.MPI_Status_set_elements(
        Pointer.pointerTo(libStatus),
        datatype.handle,
        x)
    }

    override def toString(): String = s"Status(${source},${tag},${error})"
  }
  object Status {
    def apply() = new Status(mpi3.newStatus()(0))
  }
}
