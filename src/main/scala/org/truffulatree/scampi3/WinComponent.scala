//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.language.existentials
import scala.collection.mutable
import scala.ref.WeakReference
import org.bridj.Pointer

trait WinComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  def allocMem(size: Long, info: Info): Pointer[_] =
    withOutVar { pptr: Pointer[Pointer[_]] =>
      mpiCall(mpi3.lib.MPI_Alloc_mem(size, info.handle, pptr))
      pptr(0)
    }

  def freeMem(base: Pointer[_]) {
    mpiCall(mpi3.lib.MPI_Free_mem(base))
  }

  case class Target(
    rank: Int,
    displacement: Long,
    count: Int,
    datatype: mpi3.Datatype[_])

  sealed abstract class Win
      extends mpi3.Named
      with mpi3.WinCache
      with mpi3.WithErrHandler {

    protected[scampi3] val handlePtr: Pointer[mpi3.lib.MPI_Win] = {
      val result = allocateWin()
      result.set(mpi3.lib.MPI_WIN_NULL)
      result
    }

    protected[scampi3] final def handle = handlePtr(0)

    protected val selfException = mpi3.WinException.curried(this)

    protected def mpiCall(c: => Int) = mpi3.mpiCall(c, selfException)

    override def equals(other: Any): Boolean = {
      other.isInstanceOf[Win] &&
      other.asInstanceOf[Win].handle == handle
    }

    override def hashCode: Int = handle.##

    def free() {
      if (!isNull) {
        Win.remove(this)
        mpiCall(mpi3.lib.MPI_Win_free(handlePtr))
      }
    }

    protected[scampi3] def fromMpiHandle(h: mpi3.lib.MPI_Win): Win =
      Win.lookup(h).get

    def mpiSetAttr(keyval: Int, attribute: Pointer[_]) {
      mpiCall(mpi3.lib.MPI_Win_set_attr(handle, keyval, attribute))
    }

    def mpiGetAttr(
        keyval: Int,
        attribute: Pointer[Pointer[_]],
        flag: Pointer[Int]) {
      mpiCall(mpi3.lib.MPI_Win_get_attr(handle, keyval, attribute, flag))
    }

    def mpiDeleteAttr(keyval: Int) {
      mpiCall(mpi3.lib.MPI_Win_delete_attr(handle, keyval))
    }

    def isNull: Boolean = handle == mpi3.lib.MPI_WIN_NULL

    lazy val group: mpi3.Group =
      withOutVar { group: Pointer[mpi3.lib.MPI_Group] =>
        mpiCall(mpi3.lib.MPI_Win_get_group(handle, group))
        mpi3.Group(group(0))
      }

    def put(origin: mpi3.ValueBuffer[_], target: Target) {
      mpiCall(
        mpi3.lib.MPI_Put(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          handle))
    }

    def get(origin: mpi3.ValueBuffer[_], target: Target) {
      mpiCall(
        mpi3.lib.MPI_Get(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          handle))
    }

    def accumulate(origin: mpi3.ValueBuffer[_], target: Target, op: mpi3.Op) {
      mpiCall(
        mpi3.lib.MPI_Accumulate(
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

    def compareAndSwap(
      origin: mpi3.ValueBuffer[_],
      compare: mpi3.ValueBuffer[_],
      result: mpi3.ValueBuffer[_],
      target: Target) {
      require(
        origin.valueCount > 0,
        "Origin buffer element count must be positive")
      require(
        compare.valueCount > 0,
        "Compare buffer element count must be positive")
      require(
        result.valueCount > 0,
        "Result buffer element count must be positive")
      require(
        target.count > 0,
        "Target element count must be positive")
      mpiCall(
        mpi3.lib.MPI_Compare_and_swap(
          origin.pointer,
          compare.pointer,
          result.pointer,
          target.datatype.handle,
          target.rank,
          target.displacement,
          handle))
    }

    def fetchAndOp(
      origin: mpi3.ValueBuffer[_],
      result: mpi3.ValueBuffer[_],
      target: Target,
      op: mpi3.Op) {
      require(
        origin.valueCount > 0,
        "Origin buffer element count must be positive")
      require(
        result.valueCount > 0,
        "Result buffer element count must be positive")
      require(
        target.count > 0,
        "Target element count must be positive")
      mpiCall(
        mpi3.lib.MPI_Fetch_and_op(
          origin.pointer,
          result.pointer,
          target.datatype.handle,
          target.rank,
          target.displacement,
          op.handle,
          handle))
    }

    def getAccumulate(
      origin: mpi3.ValueBuffer[_],
      result: mpi3.ValueBuffer[_],
      target: Target,
      op: mpi3.Op) {
      mpiCall(
        mpi3.lib.MPI_Get_accumulate(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          result.pointer,
          result.valueCount,
          result.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          op.handle,
          handle))
    }

    def raccumulate(
      origin: mpi3.ValueBuffer[_],
      target: Target,
      op: mpi3.Op): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Raccumulate(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          op.handle,
          handle,
          result.handlePtr))
      result
    }

    def rget(origin: mpi3.ValueBuffer[_], target: Target): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Rget(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          handle,
          result.handlePtr))
      result
    }

    def rgetAccumulate(
      origin: mpi3.ValueBuffer[_],
      result: mpi3.ValueBuffer[_],
      target: Target,
      op: mpi3.Op): mpi3.Request = {
      val req = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Rget_accumulate(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          result.pointer,
          result.valueCount,
          result.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          op.handle,
          handle,
          req.handlePtr))
      req
    }

    def rput(origin: mpi3.ValueBuffer[_], target: Target): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Rput(
          origin.pointer,
          origin.valueCount,
          origin.datatype.handle,
          target.rank,
          target.displacement,
          target.count,
          target.datatype.handle,
          handle,
          result.handlePtr))
      result
    }

    def fence(assert: Seq[mpi3.WinMode.WinMode]=List.empty) {
      mpiCall(mpi3.lib.MPI_Win_fence(mpi3.WinMode.assert(assert), handle))
    }

    def start(group: mpi3.Group, assert: Seq[mpi3.WinMode.WinMode]=List.empty) {
      mpiCall(
        mpi3.lib.MPI_Win_start(
          group.handle,
          mpi3.WinMode.assert(assert),
          handle))
    }

    def complete() { mpiCall(mpi3.lib.MPI_Win_complete(handle)) }

    def post(group: mpi3.Group, assert: Seq[mpi3.WinMode.WinMode]=List.empty) {
      mpiCall(
        mpi3.lib.MPI_Win_post(group.handle, mpi3.WinMode.assert(assert), handle))
    }

    def await() { mpiCall(mpi3.lib.MPI_Win_wait(handle)) }

    def test: Boolean = withOutVar { result: Pointer[Int] =>
        mpiCall(mpi3.lib.MPI_Win_test(handle, result))
        result(0) != 0
    }

    def lock(
        lockType: mpi3.Lock.Lock,
        rank: Int,
        assert: Seq[mpi3.WinMode.WinMode]=List.empty) {
      mpiCall(
        mpi3.lib.MPI_Win_lock(
          lockType.id,
          rank,
          mpi3.WinMode.assert(assert),
          handle))
    }

    def unlock(rank: Int) { mpiCall(mpi3.lib.MPI_Win_unlock(rank, handle)) }

    def lockAll(assert: Seq[mpi3.WinMode.WinMode]=List.empty) {
      mpiCall(mpi3.lib.MPI_Win_lock_all(mpi3.WinMode.assert(assert), handle))
    }

    def unlockAll() {
      mpiCall(mpi3.lib.MPI_Win_unlock_all(handle))
    }

    def flush(rank: Int) {
      mpiCall(mpi3.lib.MPI_Win_flush(rank, handle))
    }

    def flushAll() {
      mpiCall(mpi3.lib.MPI_Win_flush_all(handle))
    }

    def flushLocal(rank: Int) {
      mpiCall(mpi3.lib.MPI_Win_flush_local(rank, handle))
    }

    def flushLocalAll() {
      mpiCall(mpi3.lib.MPI_Win_flush_local_all(handle))
    }

    def sync() {
      mpiCall(mpi3.lib.MPI_Win_sync(handle))
    }

    private def fencedEval[A](
        assertOpen: Seq[mpi3.WinMode.WinMode],
        assertClose: Seq[mpi3.WinMode.WinMode],
        f: Win => A): A = {
      fence(assertOpen)
      try f(this) finally fence(assertClose)
    }

    def fenceFor[A](
        assertOpen: Seq[mpi3.WinMode.WinMode]=List.empty,
        assertClose: Seq[mpi3.WinMode.WinMode]=List.empty): (Win => A) => A =
      fencedEval(assertOpen, assertClose, _)

    private def accessEval[A](
        group: Group,
        assert: Seq[mpi3.WinMode.WinMode],
        f: Win => A): A = {
      start(group, assert)
      try f(this) finally complete()
    }

    def accessFor[A](
        group: Group,
        assert: Seq[mpi3.WinMode.WinMode]=List.empty): (Win => A) => A =
      accessEval(group, assert, _)

    private def exposeEval[A](
        group: Group,
        assert: Seq[mpi3.WinMode.WinMode],
        f: Win => A): A = {
      post(group, assert)
      try f(this) finally await()
    }

    def exposeFor[A](
        group: Group,
        assert: Seq[mpi3.WinMode.WinMode]=List.empty): (Win => A) => A =
      exposeEval(group, assert, _)

    private def lockEval[A](
        lockType: mpi3.Lock.Lock,
        rank: Int,
        assert: Seq[mpi3.WinMode.WinMode], f: Win => A): A = {
      lock(lockType, rank, assert)
      try f(this) finally unlock(rank)
    }

    def lockFor[A](
        lockType: mpi3.Lock.Lock,
        rank: Int,
        assert: Seq[mpi3.WinMode.WinMode]=List.empty): (Win => A) => A =
      lockEval(lockType, rank, assert, _)

    private def lockAllEval[A](
      assert: Seq[mpi3.WinMode.WinMode], f: Win => A): A = {
      lockAll(assert)
      try f(this) finally unlockAll()
    }

    def lockAllFor[A](
      assert: Seq[mpi3.WinMode.WinMode]=List.empty): (Win => A) => A =
      lockAllEval(assert, _)

    protected def setName(s: Pointer[Byte]) {
      mpiCall(mpi3.lib.MPI_Win_set_name(handle, s))
    }

    protected def getName(buffer: Pointer[Byte]) {
      withOutVar { resultlen: Pointer[Int] =>
        mpiCall(mpi3.lib.MPI_Win_get_name(handle, buffer, resultlen))
      }
    }

    def info: mpi3.Info = {
      val result = new mpi3.Info()
      mpiCall(mpi3.lib.MPI_Win_get_info(handle, result.handlePtr))
      result
    }

    def info_=(newInfo: mpi3.Info) {
      mpiCall(mpi3.lib.MPI_Win_set_info(handle, newInfo.handle))
    }

    type ErrHandlerType = WinErrHandler

    protected var currentErrHandler: WinErrHandler = WinErrHandler.Return

    errHandler = currentErrHandler

    protected def mpiSetErrhandler(errhandler: mpi3.lib.MPI_Errhandler): Int =
      mpi3.lib.MPI_Win_set_errhandler(handle, errhandler)
  }

  final class WinCreate protected[scampi3] (val base: Pointer[_])
      extends Win

  final class WinAllocate protected[scampi3] () extends Win {
    protected[scampi3] val basePtr: Pointer[Pointer[_]] =
      Pointer.allocatePointer()

    def base: Pointer[_] = basePtr(0)
  }

  final class WinShared protected[scampi3] () extends Win {
    protected[scampi3] val basePtr: Pointer[Pointer[_]] =
      Pointer.allocatePointer()

    def base: Pointer[_] = basePtr(0)

    def query(rank: Int): (Long, Int, Pointer[_]) =
      withOutVar { size: Pointer[mpi3.lib.MPI_Aint] =>
        withOutVar { dispUnit: Pointer[Int] =>
          withOutVar { baseptr: Pointer[Pointer[_]] =>
            mpiCall(
              mpi3.lib.MPI_Win_shared_query(handle,
                rank,
                size,
                dispUnit,
                baseptr))
            (size(0), dispUnit(0), baseptr(0))
          }
        }
      }
  }

  final class WinDynamic protected[scampi3] () extends Win {
    private val attached: mutable.Set[Pointer[_]] = mutable.Set()

    def attach(base: Pointer[_], size: Long) {
      mpiCall(mpi3.lib.MPI_Win_attach(handle, base, size))
      attached += base
    }

    def detach(base: Pointer[_]) {
      attached -= base
      mpiCall(mpi3.lib.MPI_Win_detach(handle, base))
    }
  }

  object Win {
    private val wins: mutable.Map[mpi3.lib.MPI_Win, WeakReference[Win]] =
      mutable.Map.empty

    protected[scampi3] def register(win: Win) {
      wins.synchronized {
        require(!win.isNull, "May not register a null Win")
        wins(win.handle) = WeakReference(win)
      }
    }

    protected[scampi3] def remove(win: Win) {
      wins.synchronized { wins -= win.handle }
    }

    protected[scampi3] def lookup(win: mpi3.lib.MPI_Win): Option[Win] =
      wins.synchronized {
        if (wins.contains(win)) {
          wins(win) match {
            case WeakReference(w) if !w.isNull => Some(w)
            case _ => None
          }
        } else None
      }

    object PredefWinIntKeyval
        extends mpi3.RestrictedAttributeKeyval[mpi3.Win,Int]
        with mpi3.KeyvalInt

    object PredefWinPointerKeyval
        extends mpi3.RestrictedAttributeKeyval[mpi3.Win,Pointer[_]]
        with mpi3.KeyvalPointer

    object PredefWinAintKeyval
        extends mpi3.RestrictedAttributeKeyval[mpi3.Win,Long]
        with mpi3.KeyvalAint

    object BaseKey
        extends mpi3.CacheKey(PredefWinPointerKeyval, mpi3.lib.MPI_WIN_BASE)

    object SizeKey
        extends mpi3.CacheKey(PredefWinAintKeyval, mpi3.lib.MPI_WIN_SIZE)

    object DispUnitKey
        extends mpi3.CacheKey(PredefWinIntKeyval, mpi3.lib.MPI_WIN_DISP_UNIT)

    object CreateFlavorKey
        extends mpi3.CacheKey(PredefWinIntKeyval, mpi3.lib.MPI_WIN_CREATE_FLAVOR)

    object ModelKey
        extends mpi3.CacheKey(PredefWinIntKeyval, mpi3.lib.MPI_WIN_MODEL)
  }

  trait WinErrHandler extends mpi3.ErrHandler

  class WinUserErrHandler(fn: Function2[Win, Int, (Win, Int)])
      extends WinErrHandler
      with mpi3.UserErrHandler {
    // The error handler should only be called within the context of a
    // an mpiCall function.
    def handleError(win: Pointer[mpi3.lib.MPI_Win], err: Pointer[Int]) {
      fn(Win.lookup(win(0)).get, err(0)) match {
        case (newwin, code) => {
          win(0) = newwin.handle
          err(0) = code
        }
      }
    }

    private val errhandlerFunction =
      mpi3.lib.MPI_Win_errhandler_function(handleError)

    mpi3.mpiCall(
      mpi3.lib.MPI_Win_create_errhandler(
        Pointer.pointerTo(errhandlerFunction),
        handlePtr))
  }

  object WinErrHandler {
    object Abort extends WinErrHandler {
      handlePtr.set(mpi3.lib.MPI_ERRORS_ARE_FATAL)
    }
    object Return extends WinErrHandler {
      handlePtr.set(mpi3.lib.MPI_ERRORS_RETURN)
    }
  }
}
