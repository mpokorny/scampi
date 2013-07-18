//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable
import scala.ref.WeakReference
import org.bridj.Pointer

trait DatatypeComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  abstract class Datatype[V]
      extends mpi2.Named
      with mpi2.DatatypeCache {

    type Elem = V

    final protected val handlePtr: Pointer[mpi2.lib.MPI_Datatype] = {
      val result = allocateDatatype()
      result.set(mpi2.lib.MPI_DATATYPE_NULL)
      result
    }

    protected[scampi2] def handle = handlePtr(0)

    protected[scampi2] final def fromMpiHandle(h: mpi2.lib.MPI_Datatype):
        Datatype[V] =
      Datatype.lookup(h).asInstanceOf[Datatype[V]]

    protected[scampi2] final def mpiSetAttr(
      keyval: Int,
      attribute: Pointer[_]) {
      mpi2.mpiCall(mpi2.lib.MPI_Type_set_attr(handle, keyval, attribute))
    }

    protected[scampi2] final def mpiGetAttr(
        keyval: Int,
        attribute: Pointer[Pointer[_]],
        flag: Pointer[Int]) {
      mpi2.mpiCall(mpi2.lib.MPI_Type_get_attr(handle, keyval, attribute, flag))
    }

    protected[scampi2] final def mpiDeleteAttr(keyval: Int) {
      mpi2.mpiCall(mpi2.lib.MPI_Type_delete_attr(handle, keyval))
    }

    override def finalize() {
      mpi2.lifecycleSync { if (!mpi2.finalized) free() }
      super.finalize()
    }

    def free()

    final def commit() { mpi2.mpiCall(mpi2.lib.MPI_Type_commit(handlePtr)) }

    lazy val contents: mpi2.Combiner = mpi2.Combiner(handle)

    final def isNull: Boolean = handle == mpi2.lib.MPI_DATATYPE_NULL

    final protected def setName(s: Pointer[Byte]) {
      mpi2.mpiCall(mpi2.lib.MPI_Type_set_name(handle, s))
    }

    final protected def getName(buffer: Pointer[Byte]) {
      withOutVar { resultlen: Pointer[Int] =>
        mpi2.mpiCall(mpi2.lib.MPI_Type_get_name(handle, buffer, resultlen))
      }
    }

    val multiplicity: Int

    val alignment: Int

    final lazy val extent: mpi2.Extent = {
      val args = mpi2.allocateAint(2)
      try {
        mpi2.mpiCall(mpi2.lib.MPI_Type_get_extent(handle, args, args.next))
        mpi2.Extent(lowerBound=args(0), range=args(1))
      } finally args.release()
    }

    final lazy val trueExtent: mpi2.Extent = {
      val args = mpi2.allocateAint(2)
      try {
        mpi2.mpiCall(mpi2.lib.MPI_Type_get_true_extent(handle, args, args.next))
        mpi2.Extent(lowerBound=args(0), range=args(1))
      } finally args.release()
    }

    final lazy val size: Int = withOutVar { result: Pointer[Int] =>
      mpi2.mpiCall(mpi2.lib.MPI_Type_size(handle, result))
      result(0)
    }

    final def trueRange(length: Int) =
      if (length > 0) length * extent.range - (extent.range - trueExtent.range)
      else 0
  }

  abstract class SeqDatatype[V] extends Datatype[V] {

    def load(p: Pointer[_], idx: Int): V

    def store(p: Pointer[_], idx: Int, elem: V): Unit

    def apply(vs: V*): SeqValueBuffer[V] = SeqValueBuffer(vs:_*)(this)

    def alloc(length: Int): SeqValueBuffer[V] =
      SeqValueBuffer.alloc(length)(this)

    def offsetTo(idx: Int): Long

    final def *(length: Int): SeqDatatypeVector[V] =
      new SeqDatatypeVector(this, length)
  }

  object Datatype {
    private val dts:
        mutable.Map[mpi2.lib.MPI_Datatype, WeakReference[Datatype[_]]] =
      mutable.Map.empty

    protected[scampi2] def register(dt: Datatype[_]) {
      dts.synchronized {
        dts(dt.handle) = WeakReference(dt)
      }
    }

    protected[scampi2] def lookup(dt: mpi2.lib.MPI_Datatype): Datatype[_] =
      dts.synchronized {
        if (dts.contains(dt)) {
          dts(dt) match {
            case WeakReference(datatype) if !datatype.isNull => datatype
            case _ => newDatatype(dt)
          }
        } else newDatatype(dt)
      }

    protected[scampi2] def remove(dt: Datatype[_]) {
      dts.synchronized {
        dts -= dt.handle
      }
    }

    private def newDatatype(dt: mpi2.lib.MPI_Datatype): Datatype[_] = dt match {
      case mpi2.MpiChar.handle =>
        mpi2.MpiChar
      case mpi2.MpiShort.handle =>
        mpi2.MpiShort
      case mpi2.MpiInt.handle =>
        mpi2.MpiInt
      case mpi2.MpiLong.handle =>
        mpi2.MpiLong
      case mpi2.MpiLongLong.handle =>
        mpi2.MpiLongLong
      case mpi2.MpiSignedChar.handle =>
        mpi2.MpiSignedChar
      case mpi2.MpiUnsignedChar.handle =>
        mpi2.MpiUnsignedChar
      case mpi2.MpiUnsignedShort.handle =>
        mpi2.MpiUnsignedShort
      case mpi2.MpiUnsigned.handle =>
        mpi2.MpiUnsigned
      case mpi2.MpiUnsignedLong.handle =>
        mpi2.MpiUnsignedLong
      case mpi2.MpiUnsignedLongLong.handle =>
        mpi2.MpiUnsignedLongLong
      case mpi2.MpiFloat.handle =>
        mpi2.MpiFloat
      case mpi2.MpiDouble.handle =>
        mpi2.MpiDouble
      case mpi2.MpiWchar.handle =>
        mpi2.MpiWchar
      case mpi2.MpiByte.handle =>
        mpi2.MpiByte
      case mpi2.MpiBoolean.handle =>
        mpi2.MpiBoolean
      case mpi2.MpiComplex.handle =>
        mpi2.MpiComplex
      case mpi2.MpiFloatComplex.handle =>
        mpi2.MpiFloatComplex
      case mpi2.MpiDoubleComplex.handle =>
        mpi2.MpiDoubleComplex
      case mpi2.MpiFloatInt.handle =>
        mpi2.MpiFloatInt
      case mpi2.MpiDoubleInt.handle =>
        mpi2.MpiDoubleInt
      case mpi2.MpiLongInt.handle =>
        mpi2.MpiLongInt
      case mpi2.MpiIntInt.handle =>
        mpi2.MpiIntInt
      case mpi2.MpiShortInt.handle =>
        mpi2.MpiShortInt
      case mpi2.MpiInt8.handle =>
        mpi2.MpiInt8
      case mpi2.MpiUint8.handle =>
        mpi2.MpiUint8
      case mpi2.MpiInt16.handle =>
        mpi2.MpiInt16
      case mpi2.MpiUint16.handle =>
        mpi2.MpiUint16
      case mpi2.MpiInt32.handle =>
        mpi2.MpiInt32
      case mpi2.MpiUint32.handle =>
        mpi2.MpiUint32
      case mpi2.MpiInt64.handle =>
        mpi2.MpiInt64
      case mpi2.MpiUint64.handle =>
        mpi2.MpiUint64
      case mpi2.MpiPacked.handle =>
        mpi2.MpiPacked
      case _ => {
        if (dt != mpi2.lib.MPI_DATATYPE_NULL) {
          val combiner = mpi2.Combiner(dt)
          val result: Datatype[_] = combiner match {
            case mpi2.NamedCombiner => {
              throw new mpi2.Exception(
                """|MPI datatype with NamedCombiner cannot be instantiated
                   |as a derived datatype""".stripMargin)
            }
            case mpi2.DupCombiner(mpiBasisType) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.DupDatatype(basisType)
            }
            case mpi2.ContiguousCombiner(mpiBasisType, count) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.ContiguousDatatype(basisType, count)
            }
            case mpi2.VectorCombiner(mpiBasisType, count, blocklength, stride) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.VectorDatatype(basisType, count, blocklength, stride)
            }
            case mpi2.HvectorCombiner(mpiBasisType, count, blocklength, stride) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.HvectorDatatype(basisType, count, blocklength, stride)
            }
            case mpi2.IndexedCombiner(mpiBasisType, blocks) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.IndexedDatatype(basisType, blocks)
            }
            case mpi2.HindexedCombiner(mpiBasisType, blocks) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.HindexedDatatype(basisType, blocks)
            }
            case mpi2.IndexedBlockCombiner(
              mpiBasisType,
              blocklength,
              displacements) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.IndexedBlockDatatype(
                basisType,
                blocklength,
                displacements)
            }
            case mpi2.StructCombiner(blocks) =>
              new mpi2.StructDatatype(
                (blocks.map { tb: TypeBlock =>
                  mpi2.StructBlock(
                    lookup(tb.datatype.handle).asInstanceOf[mpi2.SeqDatatype[_]],
                    tb.length,
                    Some(tb.displacement))
                }):_*)
            case mpi2.SubarrayCombiner(mpiBasisType, dims, order) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.SubarrayDatatype(basisType, dims, order)
            }
            case d: DarrayCombiner =>
              ???
            case mpi2.ResizedCombiner(mpiBasisType, extent) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi2.SeqDatatype[_]]
              new mpi2.ResizedDatatype(basisType, extent)
            }
          }
          result.handlePtr.set(dt)
          register(result)
          result
        } else {
          throw new mpi2.Exception("NULL datatype cannot be instantiated")
        }
      }
    }
  }

  final class SeqDatatypeVector[V](
    val datatype: SeqDatatype[V],
    val length: Int) {
    require(length > 0, "Datatype vector length not greater than zero")
    def alloc: SeqValueBuffer[V] = datatype.alloc(length)
  }
}
