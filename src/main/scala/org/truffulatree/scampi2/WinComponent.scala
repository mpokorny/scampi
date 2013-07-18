//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.language.existentials
import scala.collection.mutable
import scala.ref.WeakReference
import org.bridj.Pointer

trait WinComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  case class Target(
    rank: Int,
    displacement: mpi2.lib.MPI_Aint,
    count: Int,
    datatype: mpi2.Datatype[_])

  final class Win(
    base: Pointer[_],
    size: Long,
    dispUnit: Int,
    info: mpi2.Info,
    comm: mpi2.IntraComm)
      extends mpi2.Named
      with mpi2.WinCache
      with mpi2.WithErrHandler {

    private val handlePtr: Pointer[mpi2.lib.MPI_Win] = {
      val result = allocateWin()
      result.set(mpi2.lib.MPI_WIN_NULL)
      result
    }

    protected[scampi2] final def handle = handlePtr(0)

    protected val selfException = mpi2.WinException.curried(this)

    protected def mpiCall(c: => Int) = mpi2.mpiCall(c, selfException)

    mpi2.mpiCall(
      mpi2.lib.MPI_Win_create(
        base,
        size,
        dispUnit,
        info.handle,
        comm.handle,
        handlePtr),
      CommException.curried(comm))

    Win.register(this)

    override def equals(other: Any): Boolean = {
      other.isInstanceOf[Win] &&
      other.asInstanceOf[Win].handle == handle
    }

    override def hashCode: Int = handle.##

    override def finalize() {
      mpi2.lifecycleSync { if (!mpi2.finalized) free() }
      super.finalize()
    }

    def free() {
      if (!isNull) {
        Win.remove(this)
        mpiCall(mpi2.lib.MPI_Win_free(handlePtr))
      }
    }

    protected[scampi2] def fromMpiHandle(h: mpi2.lib.MPI_Win): Win =
      Win.lookup(h).get

    def mpiSetAttr(keyval: Int, attribute: Pointer[_]) {
      mpiCall(mpi2.lib.MPI_Win_set_attr(handle, keyval, attribute))
    }

    def mpiGetAttr(
        keyval: Int,
        attribute: Pointer[Pointer[_]],
        flag: Pointer[Int]) {
      mpiCall(mpi2.lib.MPI_Win_get_attr(handle, keyval, attribute, flag))
    }

    def mpiDeleteAttr(keyval: Int) {
      mpiCall(mpi2.lib.MPI_Win_delete_attr(handle, keyval))
    }

    def isNull: Boolean = handle == mpi2.lib.MPI_WIN_NULL

    lazy val group: mpi2.Group =
      withOutVar { group: Pointer[mpi2.lib.MPI_Group] =>
        mpiCall(mpi2.lib.MPI_Win_get_group(handle, group))
        mpi2.Group(group(0))
      }

    def put(origin: mpi2.ValueBuffer[_], target: Target) {
      mpiCall(
        mpi2.lib.MPI_Put(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          handle
        ))
    }

    def get(origin: mpi2.ValueBuffer[_], target: Target) {
      mpiCall(
        mpi2.lib.MPI_Get(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          handle))
    }

    def accumulate(origin: mpi2.ValueBuffer[_], target: Target, op: mpi2.Op) {
      mpiCall(
        mpi2.lib.MPI_Accumulate(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          op.handle,
          handle))
    }

    def fence(assert: Seq[mpi2.WinMode.WinMode]=List.empty) {
      mpiCall(mpi2.lib.MPI_Win_fence(mpi2.WinMode.assert(assert), handle))
    }

    def start(group: mpi2.Group, assert: Seq[mpi2.WinMode.WinMode]=List.empty) {
      mpiCall(
        mpi2.lib.MPI_Win_start(
          group.handle,
          mpi2.WinMode.assert(assert),
          handle))
    }

    def complete() { mpiCall(mpi2.lib.MPI_Win_complete(handle)) }

    def post(group: mpi2.Group, assert: Seq[mpi2.WinMode.WinMode]=List.empty) {
      mpiCall(
        mpi2.lib.MPI_Win_post(group.handle, mpi2.WinMode.assert(assert), handle))
    }

    def await() { mpiCall(mpi2.lib.MPI_Win_wait(handle)) }

    def test: Boolean = withOutVar { result: Pointer[Int] =>
        mpiCall(mpi2.lib.MPI_Win_test(handle, result))
        result(0) != 0
    }

    def lock(
        lockType: mpi2.Lock.Lock,
        rank: Int,
        assert: Seq[mpi2.WinMode.WinMode]=List.empty) {
      mpiCall(
        mpi2.lib.MPI_Win_lock(
          lockType.id,
          rank,
          mpi2.WinMode.assert(assert),
          handle))
    }

    def unlock(rank: Int) { mpiCall(mpi2.lib.MPI_Win_unlock(rank, handle)) }

    private def fencedEval[A](
        assertOpen: Seq[mpi2.WinMode.WinMode],
        assertClose: Seq[mpi2.WinMode.WinMode],
        f: Win => A): A = {
      fence(assertOpen)
      try f(this) finally fence(assertClose)
    }

    def fenceFor[A](
        assertOpen: Seq[mpi2.WinMode.WinMode]=List.empty,
        assertClose: Seq[mpi2.WinMode.WinMode]=List.empty): (Win => A) => A =
      fencedEval(assertOpen, assertClose, _)

    private def accessEval[A](
        group: Group,
        assert: Seq[mpi2.WinMode.WinMode],
        f: Win => A): A = {
      start(group, assert)
      try f(this) finally complete()
    }

    def accessFor[A](
        group: Group,
        assert: Seq[mpi2.WinMode.WinMode]=List.empty): (Win => A) => A =
      accessEval(group, assert, _)

    private def exposeEval[A](
        group: Group,
        assert: Seq[mpi2.WinMode.WinMode],
        f: Win => A): A = {
      post(group, assert)
      try f(this) finally await()
    }

    def exposeFor[A](
        group: Group,
        assert: Seq[mpi2.WinMode.WinMode]=List.empty): (Win => A) => A =
      exposeEval(group, assert, _)

    private def lockEval[A](
        lockType: mpi2.Lock.Lock,
        rank: Int,
        assert: Seq[mpi2.WinMode.WinMode], f: Win => A): A = {
      lock(lockType, rank, assert)
      try f(this) finally unlock(rank)
    }

    def lockFor[A](
        lockType: mpi2.Lock.Lock,
        rank: Int,
        assert: Seq[mpi2.WinMode.WinMode]=List.empty): (Win => A) => A =
      lockEval(lockType, rank, assert, _)

    protected def setName(s: Pointer[Byte]) {
      mpiCall(mpi2.lib.MPI_Win_set_name(handle, s))
    }

    protected def getName(buffer: Pointer[Byte]) {
      withOutVar { resultlen: Pointer[Int] =>
        mpiCall(mpi2.lib.MPI_Win_get_name(handle, buffer, resultlen))
      }
    }

    type ErrHandlerType = WinErrHandler

    protected var currentErrHandler: WinErrHandler = WinErrHandler.Return

    errHandler = currentErrHandler

    protected def mpiSetErrhandler(errhandler: mpi2.lib.MPI_Errhandler): Int =
      mpi2.lib.MPI_Win_set_errhandler(handle, errhandler)
  }

  object Win {
    private val wins: mutable.Map[mpi2.lib.MPI_Win, WeakReference[Win]] =
      mutable.Map.empty

    protected[scampi2] def register(win: Win) {
      wins.synchronized {
        require(!win.isNull, "May not register a null Win")
        wins(win.handle) = WeakReference(win)
      }
    }

    protected[scampi2] def remove(win: Win) {
      wins.synchronized { wins -= win.handle }
    }

    protected[scampi2] def lookup(win: mpi2.lib.MPI_Win): Option[Win] =
      wins.synchronized {
        if (wins.contains(win)) {
          wins(win) match {
            case WeakReference(w) if !w.isNull => Some(w)
            case _ => None
          }
        } else None
      }

    object PredefWinIntKeyval
        extends mpi2.RestrictedAttributeKeyval[mpi2.Win,Int]
        with mpi2.KeyvalInt

    object BaseKey
        extends mpi2.CacheKey(PredefWinIntKeyval, mpi2.lib.MPI_WIN_BASE)

    object SizeKey
        extends mpi2.CacheKey(PredefWinIntKeyval, mpi2.lib.MPI_WIN_SIZE)

    object DispUnitKey
        extends mpi2.CacheKey(PredefWinIntKeyval, mpi2.lib.MPI_WIN_DISP_UNIT)
  }

  trait WinErrHandler extends mpi2.ErrHandler

  class WinUserErrHandler(fn: Function2[Win, Int, (Win, Int)])
      extends WinErrHandler
      with mpi2.UserErrHandler {
    // The error handler should only be called within the context of a
    // an mpiCall function.
    def handleError(win: Pointer[mpi2.lib.MPI_Win], err: Pointer[Int]) {
      fn(Win.lookup(win(0)).get, err(0)) match {
        case (newwin, code) => {
          win(0) = newwin.handle
          err(0) = code
        }
      }
    }

    private val errhandlerFunction =
      mpi2.lib.MPI_Win_errhandler_function(handleError)

    mpi2.mpiCall(
      mpi2.lib.MPI_Win_create_errhandler(
        Pointer.pointerTo(errhandlerFunction),
        handlePtr))
  }

  object WinErrHandler {
    object Abort extends WinErrHandler {
      handlePtr.set(mpi2.lib.MPI_ERRORS_ARE_FATAL)
    }
    object Return extends WinErrHandler {
      handlePtr.set(mpi2.lib.MPI_ERRORS_RETURN)
    }
  }
}
