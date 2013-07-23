//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.collection.mutable
import scala.util.Random
import org.bridj.{Pointer, TypedPointer, NativeObjectInterface}

trait CacheComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  abstract class Keyval[ObjectType,ValueType] {
    def copyAttribute(obj: ObjectType, ptr: Pointer[_]): Option[Pointer[_]]

    def toPointer(v: ValueType): Pointer[_]

    def fromPointer(p: Pointer[_]): ValueType
  }

  abstract class ValAttributeKeyval[ObjectType,ValueType<:AnyVal]
      extends Keyval[ObjectType,ValueType] {
    def copyAttribute(obj: ObjectType, ptr: Pointer[_]): Option[Pointer[_]] =
      Some(toPointer(fromPointer(ptr)))
  }

  abstract class RefAttributeKeyval[ObjectType,ValueType<:AnyRef]
      extends Keyval[ObjectType,ValueType] {
    def copyAttribute(obj: ObjectType, ptr: Pointer[_]): Option[Pointer[_]] =
      Some(ptr)
  }

  abstract class RestrictedAttributeKeyval[ObjectType,ValueType]
      extends Keyval[ObjectType,ValueType] {
    def copyAttribute(obj: ObjectType, ptr: Pointer[_]): Option[Pointer[_]] =
      None
  }

  trait KeyvalTyped[ValueType] {
    class KeyvalTypedPointer(value: ValueType)
        extends TypedPointer(ValueCache.record(value)) {
      override def release() {
        ValueCache.remove(peer)
        super.release()
      }
    }

    def toPointer(value: ValueType): Pointer[_] =
      new KeyvalTypedPointer(value)

    def fromPointer(ptr: Pointer[_]): ValueType =
      ValueCache.lookup(ptr.getPeer).asInstanceOf[ValueType]
  }

  trait KeyvalInt {
    def toPointer(v: Int): Pointer[_] = Pointer.pointerToInt(v)

    def fromPointer(ptr: Pointer[_]): Int = ptr.as(classOf[Int])(0)
  }

  trait KeyvalAint {
    def toPointer(v: Long): Pointer[_] = mpi3.pointerToAint(mpi3.aintFromLong(v))

    def fromPointer(ptr: Pointer[_]): Long =
      mpi3.aintToLong(mpi3.aintFromPointer(ptr))
  }

  trait KeyvalPointer {
    def toPointer(v: Pointer[_]): Pointer[_] = Pointer.pointerToPointer(v)

    def fromPointer(ptr: Pointer[_]): Pointer[_] = ptr.as(classOf[Pointer[_]])(0)
  }

  trait KeyvalBoolean {
    def toPointer(v: Boolean): Pointer[_] =
      Pointer.pointerToInt(if (v) 1 else 0)

    def fromPointer(ptr: Pointer[_]): Boolean =
      ptr.as(classOf[Int])(0) != 0
  }

  // trait KeyvalValueSeq[V] {
  //   val datatypeVector: mpi3.DatatypeVector[V]

  //   lazy val trueExtent = mpi3.Extent(
  //     datatypeVector.datatype.trueExtent.lowerBound,
  //     datatypeVector.datatype.trueRange(datatypeVector.length))

  //   def toPointer(v: mpi3.ValueBuffer[V]): Pointer[_] = {
  //     require(
  //       v.length == datatypeVector.length,
  //       "Value length must equal datatype vector length")
  //     require(
  //       v.datatype == datatypeVector.datatype,
  //       "Value datatype must be identical to vector datatype")
  //     v.pointer
  //   }

  //   def fromPointer(ptr: Pointer[_]): mpi3.ValueBuffer[V] =
  //     (datatypeVector @:
  //       mpi3.CommBuffer(ptr.offset(-trueExtent.lowerBound), trueExtent.range))
  // }

  class CacheKey[ObjectType,ValueType] protected[scampi3](
    val keyval: Keyval[ObjectType,ValueType],
    initId: Int) {
    def id: Int = initId
  }

  final class UserCacheKey[ObjectType,ValueType] protected[scampi3](
      keyval: Keyval[ObjectType,ValueType],
      initId: Int,
      val mpiFreeKeyval: Pointer[Int] => Int)
      extends CacheKey[ObjectType,ValueType](keyval, initId) {
    private val idPtr: Pointer[Int] = {
      val result = Pointer.allocateInt().as(classOf[Int])
      result(0) = initId
      result
    }

    override def id: Int = idPtr(0)

    def isNull: Boolean = id == mpi3.lib.MPI_KEYVAL_INVALID

    override def finalize() {
      mpi3.lifecycleSync { if (!mpi3.finalized) free() }
    }

    def free() {
      if (!isNull) mpi3.mpiCall(mpiFreeKeyval(idPtr))
    }

    // override def equals(other: Any): Boolean = {
    //   other.isInstanceOf[CacheKey[ObjectType,_]] &&
    //   other.asInstanceOf[CacheKey[ObjectType,_]].id == id
    // }

    // override def hashCode: Int = id.hashCode
  }

  trait Cache[ObjectType] {
    type HandleType

    type CopyFunctionType <: NativeObjectInterface

    type DeleteFunctionType <: NativeObjectInterface

    val attributes = mutable.Map[Int, Pointer[_]]()

    protected[scampi3] def fromMpiHandle(h: HandleType): ObjectType

    trait CopyFn {
      val keyval: Keyval[ObjectType,_]
      def apply(
          handle: HandleType,
          key: Int,
          extraState: Pointer[_],
          attributeValIn: Pointer[_],
          attributeValOut: Pointer[Pointer[_]],
          flag: Pointer[Int]): Int = {
        val attrValOutOpt =
          keyval.copyAttribute(fromMpiHandle(handle), attributeValIn)
        if (attrValOutOpt.isDefined) {
          val attrValOut = attrValOutOpt.get
          attributeValOut.set(attrValOut)
          attributes(key) = attrValOut
          flag.as(classOf[Int]).set(1)
        } else {
          flag.as(classOf[Int]).set(0)
        }
        mpi3.lib.MPI_SUCCESS
      }
    }

    trait DelFn {
      def apply(
          handle: HandleType,
          key: Int,
          attributeVal: Pointer[_],
          extraState: Pointer[_]): Int = {
        attributes -= key
        mpi3.lib.MPI_SUCCESS
      }
    }

    protected val Del: DeleteFunctionType

    protected def mkCopyFn(keyval: Keyval[ObjectType,_]): CopyFunctionType

    def newKey[V](keyval: Keyval[ObjectType,V]): CacheKey[ObjectType,V] = {
      val copyFn = mkCopyFn(keyval)
      cacheKeys.synchronized {
        val keyId = withOutVar { keyPtr: Pointer[Int] =>
          mpi3.mpiCall(
            mpiCreateKeyval(
              Pointer.pointerTo(copyFn),
              Pointer.pointerTo(Del),
              keyPtr,
              null))
          keyPtr(0)
        }
        val result = new UserCacheKey(keyval, keyId, mpiFreeKeyval)
        cacheKeys += result
        result
      }
    }

    protected def mpiCreateKeyval(
      copyFn: Pointer[CopyFunctionType],
      delFn: Pointer[DeleteFunctionType],
      keyval: Pointer[Int],
      extraState: Pointer[_]): Int

    protected def mpiFreeKeyval(keyval: Pointer[Int]): Int

    protected def mpiSetAttr(keyval: Int, attribute: Pointer[_])

    protected def mpiGetAttr(
      keyval: Int,
      attribute: Pointer[Pointer[_]],
      flag: Pointer[Int])

    protected def mpiDeleteAttr(keyval: Int)

    protected val cacheKeys: mutable.Set[CacheKey[ObjectType,_]]

    def update[T](key: CacheKey[ObjectType,T], value: T) = {
      val ptr = key.keyval.toPointer(value)
      mpiSetAttr(key.id, ptr)
      attributes(key.id) = ptr
      this
    }

    def remove(key: CacheKey[ObjectType,_]) = {
      mpiDeleteAttr(key.id)
      attributes -= key.id
      this
    }

    def apply[T](key: CacheKey[ObjectType,T]): Option[T] = {
      withOutVar { flag: Pointer[Int] =>
        withOutVar { attr: Pointer[Pointer[_]] =>
          mpiGetAttr(key.id, attr, flag)
          if (flag(0) != 0)
            Some(key.keyval.fromPointer(attr(0)))
          else
            None
        }
      }
    }
  }

  trait CommCache extends Cache[mpi3.Comm] {
    type HandleType = mpi3.lib.MPI_Comm

    type CopyFunctionType = mpi3.lib.MPI_Comm_copy_attr_function

    type DeleteFunctionType = mpi3.lib.MPI_Comm_delete_attr_function

    protected def mkCopyFn(kv: Keyval[mpi3.Comm,_]): CopyFunctionType =
      mpi3.lib.MPI_Comm_copy_attr_function((
        handle: mpi3.lib.MPI_Comm,
        key: Int,
        extraState: Pointer[_],
        attributeValIn: Pointer[_],
        attributeValOut: Pointer[Pointer[_]],
        flag: Pointer[Int]) => {
        val attrValOutOpt =
          kv.copyAttribute(fromMpiHandle(handle), attributeValIn)
        if (attrValOutOpt.isDefined) {
          val attrValOut = attrValOutOpt.get
          attributeValOut.set(attrValOut)
          attributes(key) = attrValOut
          flag(0) = 1
        } else {
          flag(0) = 0
        }
        mpi3.lib.MPI_SUCCESS
      })

    protected val Del: DeleteFunctionType =
      mpi3.lib.MPI_Comm_delete_attr_function((
        handle: mpi3.lib.MPI_Comm,
        key: Int,
        attributeVal: Pointer[_],
        extraState: Pointer[_]) => {
        attributes -= key
        mpi3.lib.MPI_SUCCESS
      })

    protected val cacheKeys = commCacheKeys

    final def mpiCreateKeyval(
        copyFn: Pointer[CopyFunctionType],
        delFn: Pointer[DeleteFunctionType],
        keyval: Pointer[Int],
        extraState: Pointer[_]) =
      mpi3.lib.MPI_Comm_create_keyval(copyFn, delFn, keyval, extraState)

    final def mpiFreeKeyval(keyval: Pointer[Int]) =
      mpi3.lib.MPI_Comm_free_keyval(keyval)
  }

  protected val commCacheKeys = mutable.Set[CacheKey[mpi3.Comm,_]](
    mpi3.Comm.TagUBKey,
    mpi3.Comm.UniverseSizeKey,
    mpi3.Comm.LastUsedCodeKey,
    mpi3.Comm.AppNumKey,
    mpi3.Comm.HostKey,
    mpi3.Comm.IOKey)

  trait WinCache extends Cache[mpi3.Win] {
    type HandleType = mpi3.lib.MPI_Win

    type CopyFunctionType = mpi3.lib.MPI_Win_copy_attr_function

    type DeleteFunctionType = mpi3.lib.MPI_Win_delete_attr_function

    protected def mkCopyFn(kv: Keyval[mpi3.Win,_]): CopyFunctionType =
      mpi3.lib.MPI_Win_copy_attr_function((
        handle: mpi3.lib.MPI_Win,
        key: Int,
        extraState: Pointer[_],
        attributeValIn: Pointer[_],
        attributeValOut: Pointer[Pointer[_]],
        flag: Pointer[Int]) => {
        val attrValOutOpt =
          kv.copyAttribute(fromMpiHandle(handle), attributeValIn)
        if (attrValOutOpt.isDefined) {
          val attrValOut = attrValOutOpt.get
          attributeValOut.set(attrValOut)
          attributes(key) = attrValOut
          flag(0) = 1
        } else {
          flag(0) = 0
        }
        mpi3.lib.MPI_SUCCESS
      })

    protected val Del: DeleteFunctionType =
      mpi3.lib.MPI_Win_delete_attr_function((
        handle: mpi3.lib.MPI_Win,
        key: Int,
        attributeVal: Pointer[_],
        extraState: Pointer[_]) => {
        attributes -= key
        mpi3.lib.MPI_SUCCESS
      })

    protected val cacheKeys = winCacheKeys

    final def mpiCreateKeyval(
      copyFn: Pointer[CopyFunctionType],
      delFn: Pointer[DeleteFunctionType],
      keyval: Pointer[Int],
      extraState: Pointer[_]) =
        mpi3.lib.MPI_Win_create_keyval(copyFn, delFn, keyval, extraState)

    final def mpiFreeKeyval(keyval: Pointer[Int]) =
      mpi3.lib.MPI_Win_free_keyval(keyval)
  }

  protected val winCacheKeys = mutable.Set[CacheKey[mpi3.Win,_]](
    mpi3.Win.BaseKey,
    mpi3.Win.SizeKey,
    mpi3.Win.DispUnitKey
  )

  trait DatatypeCache extends Cache[mpi3.Datatype[_]] {
    type HandleType = mpi3.lib.MPI_Datatype

    type CopyFunctionType = mpi3.lib.MPI_Type_copy_attr_function

    type DeleteFunctionType = mpi3.lib.MPI_Type_delete_attr_function

    protected def mkCopyFn(kv: Keyval[mpi3.Datatype[_],_]): CopyFunctionType =
      mpi3.lib.MPI_Type_copy_attr_function((
        handle: mpi3.lib.MPI_Datatype,
        key: Int,
        extraState: Pointer[_],
        attributeValIn: Pointer[_],
        attributeValOut: Pointer[Pointer[_]],
        flag: Pointer[Int]) => {
        val attrValOutOpt =
          kv.copyAttribute(fromMpiHandle(handle), attributeValIn)
        if (attrValOutOpt.isDefined) {
          val attrValOut = attrValOutOpt.get
          attributeValOut.set(attrValOut)
          attributes(key) = attrValOut
          flag(0) = 1
        } else {
          flag(0) = 0
        }
        mpi3.lib.MPI_SUCCESS
      })

    protected val Del: DeleteFunctionType =
      mpi3.lib.MPI_Type_delete_attr_function((
        handle: mpi3.lib.MPI_Datatype,
        key: Int,
        attributeVal: Pointer[_],
        extraState: Pointer[_]) => {
        attributes -= key
        mpi3.lib.MPI_SUCCESS
      })

    protected val cacheKeys = datatypeCacheKeys

    final def mpiCreateKeyval(
      copyFn: Pointer[CopyFunctionType],
      delFn: Pointer[DeleteFunctionType],
      keyval: Pointer[Int],
      extraState: Pointer[_]) =
      mpi3.lib.MPI_Type_create_keyval(copyFn, delFn, keyval, extraState)

    final def mpiFreeKeyval(keyval: Pointer[Int]) =
      mpi3.lib.MPI_Type_free_keyval(keyval)
  }

  protected val datatypeCacheKeys =
    mutable.Set[CacheKey[mpi3.Datatype[_],_]]()

  protected object ValueCache {
    private val values = mutable.Map.empty[Long, Any]

    private def nextKey: Long = {
      val result = Random.nextLong
      if (result == 0 || values.contains(result)) nextKey
      else result
    }

    def lookup(key: Long): Any = synchronized {
      values(key)
    }

    def record(value: Any): Long = synchronized {
      val key = nextKey
      values(key) = value
      key
    }

    def remove(key: Long) { synchronized { values -= key } }
  }
}
