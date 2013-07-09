//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable.ArraySeq
import org.bridj.Pointer

trait RequestComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  sealed class Request {
    protected[scampi2] final val handlePtr = {
      val result = allocateRequest()
      result.set(mpi2.lib.MPI_REQUEST_NULL)
      result
    }

    final def handle = handlePtr(0)

    override def finalize() {
      mpi2.lifecycleSync { if (!mpi2.finalized) free() }
      super.finalize()
    }

    def await: Status = {
      val status = mpi2.newStatus()
      mpi2.mpiCall(mpi2.lib.MPI_Wait(handlePtr, Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doAwait() {
      mpiCall(mpi2.lib.MPI_Wait(handlePtr, mpi2.lib.MPI_STATUS_IGNORE))
    }

    private def mpiTest(status: Pointer[mpi2.lib.MPI_Status]) =
      withOutVar { flag: Pointer[Int] =>
        mpiCall(mpi2.lib.MPI_Test(handlePtr, flag, status))
        flag(0) != 0
      }

    def test: Status = {
      val status = mpi2.newStatus()
      mpiTest(Pointer.pointerTo(status(0)))
      new mpi2.Status(status(0))
    }

    def doTest: Boolean = { mpiTest(mpi2.lib.MPI_STATUS_IGNORE) }

    def free() {
      if (!isNull) mpi2.mpiCall(mpi2.lib.MPI_Request_free(handlePtr))
    }

    private def mpiStatus(status: Pointer[mpi2.lib.MPI_Status]): Boolean =
      withOutVar { flag: Pointer[Int] =>
        mpi2.mpiCall(mpi2.lib.MPI_Request_get_status(handle, flag, status))
        flag(0) != 0
      }

    def status: Status = {
      val status = mpi2.newStatus()
      mpiStatus(Pointer.pointerTo(status(0)))
      new mpi2.Status(status(0))
    }

    def doStatus: Boolean = { mpiStatus(mpi2.lib.MPI_STATUS_IGNORE) }

    def cancel() { mpi2.mpiCall(mpi2.lib.MPI_Cancel(handlePtr)) }

    def isNull: Boolean = handle == mpi2.lib.MPI_REQUEST_NULL

    override def toString = s"Request(${handle})"
  }

  private def pointerToRequests(requests: Seq[Request]) = {
    val result = mpi2.allocateRequest(requests.length)
    for (i <- 0 until requests.length) result.set(i, requests(i).handle)
    result
  }

  object Request {
    private def mpiWaitAny(
        requests: Seq[Request],
        status: Pointer[mpi2.lib.MPI_Status]) =
      withOutVar { index: Pointer[Int] =>
        val reqsPtr = pointerToRequests(requests)
        try {
          mpi2.mpiCall(
            mpi2.lib.MPI_Waitany(requests.length, reqsPtr, index, status))
          if (index(0) != mpi2.lib.MPI_UNDEFINED) {
            val req = requests(index(0))
            req.handlePtr.set(reqsPtr(index(0)))
            Some(req)
          }
          else None
        } finally reqsPtr.release()
      }

    def waitAny(requests: Seq[Request]): (Option[Request], mpi2.Status) = {
      val status = mpi2.newStatus()
      (mpiWaitAny(requests, Pointer.pointerTo(status(0))),
        new mpi2.Status(status(0)))
    }

    def doWaitAny(requests: Seq[Request]): Option[Request] =
      mpiWaitAny(requests, mpi2.lib.MPI_STATUS_IGNORE)

    private def mpiTestAny(
        requests: Seq[Request],
        status: Pointer[mpi2.lib.MPI_Status]) =
      withOutVar { index: Pointer[Int] =>
        withOutVar { flag: Pointer[Int] =>
          val reqsPtr = pointerToRequests(requests)
          try {
            mpiCall(
              mpi2.lib.MPI_Testany(
                requests.length,
                reqsPtr,
                index,
                flag,
                status))
            if (index(0) != mpi2.lib.MPI_UNDEFINED) {
              assert(flag(0) != 0)
              val req = requests(index(0))
              req.handlePtr.set(reqsPtr(index(0)))
              (Some(req), true)
            }
            else {
              assert(flag(0) == 0)
              (None, false)
            }
          } finally reqsPtr.release()
        }
      }

    def testAny(requests: Seq[Request]): (Option[Request], mpi2.Status) = {
      val status = mpi2.newStatus()
      val testResult = mpiTestAny(requests, Pointer.pointerTo(status(0)))
      (testResult._1, new mpi2.Status(status(0)))
    }

    def doTestAny(requests: Seq[Request]): (Option[Request], Boolean) =
      mpiTestAny(requests, mpi2.lib.MPI_STATUS_IGNORE)

    def waitAll(requests: Seq[Request]): Seq[mpi2.Status] = {
      val reqsPtr = pointerToRequests(requests)
      try {
        val result = mpi2.newStatus(requests.length)
        mpi2.mpiCall(
          mpi2.lib.MPI_Waitall(
            requests.length,
            reqsPtr,
            Pointer.pointerTo(result(0))))
        for (i <- 0 until requests.length)
          requests(i).handlePtr.set(reqsPtr(i))
        result.map(new mpi2.Status(_))
      } finally reqsPtr.release()
    }

    def doWaitAll(requests: Seq[Request]) {
      val reqsPtr = pointerToRequests(requests)
      try {
        mpi2.mpiCall(
          mpi2.lib.MPI_Waitall(
            requests.length,
            reqsPtr,
            mpi2.lib.MPI_STATUSES_IGNORE))
        for (i <- 0 until requests.length)
          requests(i).handlePtr.set(reqsPtr(i))
      } finally reqsPtr.release()
    }

    def testAll(requests: Seq[Request]): Seq[mpi2.Status] = {
      val result = mpi2.newStatus(requests.length)
      withOutVar { flag: Pointer[Int] =>
        val reqsPtr = pointerToRequests(requests)
        try {
          mpi2.mpiCall(
            mpi2.lib.MPI_Testall(
              requests.length,
              reqsPtr,
              flag,
              Pointer.pointerTo(result(0))))
          if (flag(0) != 0) {
            for (i <- 0 until requests.length)
              requests(i).handlePtr.set(reqsPtr(i))
            result map (s => new mpi2.Status(s))
          } else {
            List.empty
          }
        } finally reqsPtr.release()
      }
    }

    def doTestAll(requests: Seq[Request]): Boolean =
      withOutVar { flag: Pointer[Int] =>
        val reqsPtr = pointerToRequests(requests)
        try {
          mpi2.mpiCall(
            mpi2.lib.MPI_Testall(
              requests.length,
              reqsPtr,
              flag,
              mpi2.lib.MPI_STATUSES_IGNORE))
          val result = flag(0) != 0
          if (result)
            for (i <- 0 until requests.length)
              requests(i).handlePtr.set(reqsPtr(i))
          result
        } finally reqsPtr.release()
      }

    private def trySome(
        fn: (Int, Pointer[mpi2.lib.MPI_Request], Pointer[Int], Pointer[Int],
          Pointer[mpi2.lib.MPI_Status]) => Int,
        requests: Seq[Request]): Option[Seq[(Request, mpi2.Status)]] = {
      val status = mpi2.newStatus(requests.length)
      withOutVar { outcount: Pointer[Int] =>
        val indices = Pointer.allocateInts(requests.length).as(classOf[Int])
        val reqsPtr = pointerToRequests(requests)
        try {
          mpi2.mpiCall(
            fn(
              requests.length,
              reqsPtr,
              outcount,
              indices,
              Pointer.pointerTo(status(0))))
          if (outcount(0) != mpi2.lib.MPI_UNDEFINED) {
            val numCompleted = outcount(0)
            val result = (0 until numCompleted) map { i =>
              val idx = indices(i)
              val req = requests(idx)
              req.handlePtr.set(reqsPtr(idx))
              (req, new mpi2.Status(status(i)))
            }
            Some(result.toSeq)
          } else None
        } finally {
          indices.release()
          reqsPtr.release()
        }
      }
    }

    private def doTrySome(
        fn: (Int, Pointer[mpi2.lib.MPI_Request], Pointer[Int], Pointer[Int],
          Pointer[mpi2.lib.MPI_Status]) => Int,
        requests: Seq[Request]): Option[Seq[Request]] = {
      withOutVar { outcount: Pointer[Int] =>
        val indices = Pointer.allocateInts(requests.length).as(classOf[Int])
        val reqsPtr = pointerToRequests(requests)
        try {
          mpi2.mpiCall(
            fn(
              requests.length,
              reqsPtr,
              outcount,
              indices,
              mpi2.lib.MPI_STATUSES_IGNORE))
          if (outcount(0) != mpi2.lib.MPI_UNDEFINED) {
            val numCompleted = outcount(0)
            val result = (0 until numCompleted) map { i =>
              val idx = indices(i)
              val req = requests(idx)
              req.handlePtr.set(reqsPtr(idx))
              req
            }
            Some(result.toSeq)
          } else None
        } finally {
          indices.release()
          reqsPtr.release()
        }
      }
    }

    def waitSome(requests: Seq[Request]) =
      trySome(mpi2.lib.MPI_Waitsome _, requests)

    def doWaitSome(requests: Seq[Request]) =
      doTrySome(mpi2.lib.MPI_Waitsome _, requests)

    def testSome(requests: Seq[Request]) =
      trySome(mpi2.lib.MPI_Testsome _, requests)

    def doTestSome(requests: Seq[Request]) =
      doTrySome(mpi2.lib.MPI_Testsome _, requests)
  }

  class PersistentRequest extends Request {
    def start() { mpi2.mpiCall(mpi2.lib.MPI_Start(handlePtr)) }
  }

  object PersistentRequest {
    def startAll(requests: Seq[PersistentRequest]) {
      val reqsPtr = pointerToRequests(requests)
      try {
        mpi2.mpiCall(mpi2.lib.MPI_Startall(requests.length, reqsPtr))
        for (i <- 0 until requests.length)
          requests(i).handlePtr.set(reqsPtr(i))
      } finally {
        reqsPtr.release()
      }
    }
  }
}
