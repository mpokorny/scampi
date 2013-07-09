//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait LifecycleMethods {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  private def scampiInit(
      ifn: (Pointer[Int], Pointer[Pointer[Pointer[Byte]]]) => Int,
      args: Seq[String]) {
    lifecycleSync {
      if (!initialized) {
        val argc: Pointer[Int] =
          if (args.length > 0) Pointer.pointerToInt(args.length).as(classOf[Int])
          else Pointer.NULL.asInstanceOf[Pointer[Int]]
        val argv: Pointer[Pointer[Pointer[Byte]]] =
          if (args.length > 0)
            Pointer.pointerToPointer(
              Pointer.pointerToCStrings(args:_*).as(classOf[Pointer[Byte]]))
          else
            mpi2.lib.MPI_ARGVS_NULL
        mpi2.mpiCall(ifn(argc, argv))
      } else if (finalized) {
        throw new mpi2.Exception(
          "MPI may not be re-initialized after finalization")
      }
    }
  }

  private val lifecycleLock = new Tuple1(0)

  def lifecycleSync[A](f: => A): A = lifecycleLock.synchronized { f }

  def init(args: Seq[String] = List.empty) {
    scampiInit(mpi2.lib.MPI_Init, args)
  }

  def initThread(required: mpi2.ThreadLevel.ThreadLevel,
    args: Seq[String] = List.empty): mpi2.ThreadLevel.ThreadLevel = {
    withOutVar { provided: Pointer[Int] =>
      def initFn(argc: Pointer[Int], argv: Pointer[Pointer[Pointer[Byte]]]) =
        mpi2.lib.MPI_Init_thread(argc, argv, required.id, provided)
      scampiInit(initFn, args)
      mpi2.ThreadLevel(provided(0))
    }
  }

  override def finalize() {
    lifecycleSync { fin() }
    super.finalize()
  }

  def fin() {
    lifecycleSync {
      if (initialized && !finalized) {
        mpi2.lib.MPI_Finalize()
      }
    }
  }

  def initialized = lifecycleSync {
    withOutVar { flag: Pointer[Int] =>
      mpi2.lib.MPI_Initialized(flag)
      flag(0) != 0
    }
  }

  // def abort(comm: mpi2.Comm, errorcode: Int) {
  //   lifecycleSync { mpi2.lib.MPI_Abort(comm.handle, errorcode) }
  // }

  def finalized = lifecycleSync {
    withOutVar { flag: Pointer[Int] =>
      mpi2.lib.MPI_Finalized(flag)
      flag(0) != 0
    }
  }
}
