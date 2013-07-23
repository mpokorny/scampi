//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import org.bridj.Pointer

trait LifecycleMethods {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

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
            mpi3.lib.MPI_ARGVS_NULL
        mpi3.mpiCall(ifn(argc, argv))
      } else if (finalized) {
        throw new mpi3.Exception(
          "MPI may not be re-initialized after finalization")
      }
    }
  }

  private val lifecycleLock = new Tuple1(0)

  def lifecycleSync[A](f: => A): A = lifecycleLock.synchronized { f }

  def init(args: Seq[String] = List.empty) {
    scampiInit(mpi3.lib.MPI_Init, args)
  }

  def initThread(required: mpi3.ThreadLevel.ThreadLevel,
    args: Seq[String] = List.empty): mpi3.ThreadLevel.ThreadLevel = {
    withOutVar { provided: Pointer[Int] =>
      def initFn(argc: Pointer[Int], argv: Pointer[Pointer[Pointer[Byte]]]) =
        mpi3.lib.MPI_Init_thread(argc, argv, required.id, provided)
      scampiInit(initFn, args)
      mpi3.ThreadLevel(provided(0))
    }
  }

  override def finalize() {
    lifecycleSync { fin() }
    super.finalize()
  }

  def fin() {
    lifecycleSync {
      if (initialized && !finalized) {
        mpi3.lib.MPI_Finalize()
      }
    }
  }

  def initialized = lifecycleSync {
    withOutVar { flag: Pointer[Int] =>
      mpi3.lib.MPI_Initialized(flag)
      flag(0) != 0
    }
  }

  // def abort(comm: mpi3.Comm, errorcode: Int) {
  //   lifecycleSync { mpi3.lib.MPI_Abort(comm.handle, errorcode) }
  // }

  def finalized = lifecycleSync {
    withOutVar { flag: Pointer[Int] =>
      mpi3.lib.MPI_Finalized(flag)
      flag(0) != 0
    }
  }
}
