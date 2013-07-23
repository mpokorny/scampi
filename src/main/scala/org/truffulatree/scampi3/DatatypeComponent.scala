//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.collection.mutable
import scala.ref.WeakReference
import org.bridj.Pointer

trait DatatypeComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  abstract class Datatype[V]
      extends mpi3.Named
      with mpi3.DatatypeCache {

    type Elem = V

    final protected val handlePtr: Pointer[mpi3.lib.MPI_Datatype] = {
      val result = allocateDatatype()
      result.set(mpi3.lib.MPI_DATATYPE_NULL)
      result
    }

    protected[scampi3] def handle = handlePtr(0)

    protected[scampi3] final def fromMpiHandle(h: mpi3.lib.MPI_Datatype):
        Datatype[V] =
      Datatype.lookup(h).asInstanceOf[Datatype[V]]

    protected[scampi3] final def mpiSetAttr(
      keyval: Int,
      attribute: Pointer[_]) {
      mpi3.mpiCall(mpi3.lib.MPI_Type_set_attr(handle, keyval, attribute))
    }

    protected[scampi3] final def mpiGetAttr(
        keyval: Int,
        attribute: Pointer[Pointer[_]],
        flag: Pointer[Int]) {
      mpi3.mpiCall(mpi3.lib.MPI_Type_get_attr(handle, keyval, attribute, flag))
    }

    protected[scampi3] final def mpiDeleteAttr(keyval: Int) {
      mpi3.mpiCall(mpi3.lib.MPI_Type_delete_attr(handle, keyval))
    }

    override def finalize() {
      mpi3.lifecycleSync { if (!mpi3.finalized) free() }
      super.finalize()
    }

    def free()

    final def commit() { mpi3.mpiCall(mpi3.lib.MPI_Type_commit(handlePtr)) }

    lazy val contents: mpi3.Combiner = mpi3.Combiner(handle)

    final def isNull: Boolean = handle == mpi3.lib.MPI_DATATYPE_NULL

    final protected def setName(s: Pointer[Byte]) {
      mpi3.mpiCall(mpi3.lib.MPI_Type_set_name(handle, s))
    }

    final protected def getName(buffer: Pointer[Byte]) {
      withOutVar { resultlen: Pointer[Int] =>
        mpi3.mpiCall(mpi3.lib.MPI_Type_get_name(handle, buffer, resultlen))
      }
    }

    val multiplicity: Int

    val alignment: Int

    final lazy val extent: mpi3.Extent = {
      val args = mpi3.allocateAint(2)
      try {
        mpi3.mpiCall(mpi3.lib.MPI_Type_get_extent(handle, args, args.next))
        mpi3.Extent(lowerBound=args(0), range=args(1))
      } finally args.release()
    }

    final lazy val trueExtent: mpi3.Extent = {
      val args = mpi3.allocateAint(2)
      try {
        mpi3.mpiCall(mpi3.lib.MPI_Type_get_true_extent(handle, args, args.next))
        mpi3.Extent(lowerBound=args(0), range=args(1))
      } finally args.release()
    }

    final lazy val size: Int = withOutVar { result: Pointer[Int] =>
      mpi3.mpiCall(mpi3.lib.MPI_Type_size(handle, result))
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
        mutable.Map[mpi3.lib.MPI_Datatype, WeakReference[Datatype[_]]] =
      mutable.Map.empty

    protected[scampi3] def register(dt: Datatype[_]) {
      dts.synchronized {
        dts(dt.handle) = WeakReference(dt)
      }
    }

    protected[scampi3] def lookup(dt: mpi3.lib.MPI_Datatype): Datatype[_] =
      dts.synchronized {
        if (dts.contains(dt)) {
          dts(dt) match {
            case WeakReference(datatype) if !datatype.isNull => datatype
            case _ => newDatatype(dt)
          }
        } else newDatatype(dt)
      }

    protected[scampi3] def remove(dt: Datatype[_]) {
      dts.synchronized {
        dts -= dt.handle
      }
    }

    private def newDatatype(dt: mpi3.lib.MPI_Datatype): Datatype[_] = dt match {
      case mpi3.MpiChar.handle =>
        mpi3.MpiChar
      case mpi3.MpiShort.handle =>
        mpi3.MpiShort
      case mpi3.MpiInt.handle =>
        mpi3.MpiInt
      case mpi3.MpiLong.handle =>
        mpi3.MpiLong
      case mpi3.MpiLongLong.handle =>
        mpi3.MpiLongLong
      case mpi3.MpiSignedChar.handle =>
        mpi3.MpiSignedChar
      case mpi3.MpiUnsignedChar.handle =>
        mpi3.MpiUnsignedChar
      case mpi3.MpiUnsignedShort.handle =>
        mpi3.MpiUnsignedShort
      case mpi3.MpiUnsigned.handle =>
        mpi3.MpiUnsigned
      case mpi3.MpiUnsignedLong.handle =>
        mpi3.MpiUnsignedLong
      case mpi3.MpiUnsignedLongLong.handle =>
        mpi3.MpiUnsignedLongLong
      case mpi3.MpiFloat.handle =>
        mpi3.MpiFloat
      case mpi3.MpiDouble.handle =>
        mpi3.MpiDouble
      case mpi3.MpiWchar.handle =>
        mpi3.MpiWchar
      case mpi3.MpiByte.handle =>
        mpi3.MpiByte
      case mpi3.MpiBoolean.handle =>
        mpi3.MpiBoolean
      case mpi3.MpiComplex.handle =>
        mpi3.MpiComplex
      case mpi3.MpiFloatComplex.handle =>
        mpi3.MpiFloatComplex
      case mpi3.MpiDoubleComplex.handle =>
        mpi3.MpiDoubleComplex
      case mpi3.MpiFloatInt.handle =>
        mpi3.MpiFloatInt
      case mpi3.MpiDoubleInt.handle =>
        mpi3.MpiDoubleInt
      case mpi3.MpiLongInt.handle =>
        mpi3.MpiLongInt
      case mpi3.MpiIntInt.handle =>
        mpi3.MpiIntInt
      case mpi3.MpiShortInt.handle =>
        mpi3.MpiShortInt
      case mpi3.MpiInt8.handle =>
        mpi3.MpiInt8
      case mpi3.MpiUint8.handle =>
        mpi3.MpiUint8
      case mpi3.MpiInt16.handle =>
        mpi3.MpiInt16
      case mpi3.MpiUint16.handle =>
        mpi3.MpiUint16
      case mpi3.MpiInt32.handle =>
        mpi3.MpiInt32
      case mpi3.MpiUint32.handle =>
        mpi3.MpiUint32
      case mpi3.MpiInt64.handle =>
        mpi3.MpiInt64
      case mpi3.MpiUint64.handle =>
        mpi3.MpiUint64
      case mpi3.MpiPacked.handle =>
        mpi3.MpiPacked
      case _ => {
        if (dt != mpi3.lib.MPI_DATATYPE_NULL) {
          val combiner = mpi3.Combiner(dt)
          val result: Datatype[_] = combiner match {
            case mpi3.NamedCombiner => {
              throw new mpi3.Exception(
                """|MPI datatype with NamedCombiner cannot be instantiated
                   |as a derived datatype""".stripMargin)
            }
            case mpi3.DupCombiner(mpiBasisType) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.DupDatatype(basisType)
            }
            case mpi3.ContiguousCombiner(mpiBasisType, count) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.ContiguousDatatype(basisType, count)
            }
            case mpi3.VectorCombiner(mpiBasisType, count, blocklength, stride) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.VectorDatatype(basisType, count, blocklength, stride)
            }
            case mpi3.HvectorCombiner(mpiBasisType, count, blocklength, stride) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.HvectorDatatype(basisType, count, blocklength, stride)
            }
            case mpi3.IndexedCombiner(mpiBasisType, blocks) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.IndexedDatatype(basisType, blocks)
            }
            case mpi3.HindexedCombiner(mpiBasisType, blocks) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.HindexedDatatype(basisType, blocks)
            }
            case mpi3.IndexedBlockCombiner(
              mpiBasisType,
              blocklength,
              displacements) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.IndexedBlockDatatype(
                basisType,
                blocklength,
                displacements)
            }
            case mpi3.StructCombiner(blocks) =>
              new mpi3.StructDatatype(
                (blocks.map { tb: TypeBlock =>
                  mpi3.StructBlock(
                    lookup(tb.datatype.handle).asInstanceOf[mpi3.SeqDatatype[_]],
                    tb.length,
                    Some(tb.displacement))
                }):_*)
            case mpi3.SubarrayCombiner(mpiBasisType, dims, order) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.SubarrayDatatype(basisType, dims, order)
            }
            case d: DarrayCombiner =>
              ???
            case mpi3.ResizedCombiner(mpiBasisType, extent) => {
              val basisType =
                lookup(mpiBasisType).asInstanceOf[mpi3.SeqDatatype[_]]
              new mpi3.ResizedDatatype(basisType, extent)
            }
          }
          result.handlePtr.set(dt)
          register(result)
          result
        } else {
          throw new mpi3.Exception("NULL datatype cannot be instantiated")
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
