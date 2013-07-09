//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable
import _root_.org.bridj.{Pointer, CLong}

trait PredefDatatypeComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  sealed abstract class PredefDatatype[V](dt: mpi2.lib.MPI_Datatype)
      extends mpi2.SeqDatatype[V] {
    handlePtr.set(dt)
    mpi2.Datatype.register(this)

    override lazy val handle = super.handle

    val multiplicity = 1

    def free() {}

    def offsetTo(idx: Int): Long = idx * extent.range

    override def toString() = getClass.getSimpleName
  }

  object MpiChar extends PredefDatatype[Char](mpi2.lib.MPI_CHAR) {
    assume(extent.range == AlignHelper.byteByteSize)

    val alignment = AlignHelper.byteAlignment

    def load(p: Pointer[_], idx: Int): Char =
      p.as(classOf[Byte])(idx).toChar

    def store(p: Pointer[_], idx: Int, elem: Char) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[Byte])(idx) = elem.toByte
    }

    def isValid(ch: Char): Boolean = ch.isValidByte
  }

  sealed abstract class Int8(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Byte](dt) {
    assume(extent.range == AlignHelper.byteByteSize)

    val alignment = AlignHelper.byteAlignment

    def load(p: Pointer[_], idx: Int): Byte =
      p.as(classOf[Byte])(idx)

    def store(p: Pointer[_], idx: Int, elem: Byte) {
      p.as(classOf[Byte])(idx) = elem
    }
  }

  object MpiSignedChar extends Int8(mpi2.lib.MPI_SIGNED_CHAR)

  object MpiInt8 extends Int8(mpi2.lib.MPI_INT8_T)

  implicit object MpiByte extends Int8(mpi2.lib.MPI_BYTE)

  sealed abstract class Uint8(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Short](dt) {
    assume(extent.range == AlignHelper.byteByteSize)

    val alignment = AlignHelper.byteAlignment

    val maxVal = (1 << java.lang.Byte.SIZE).toShort

    def load(p: Pointer[_], idx: Int): Short = {
      val b = p.as(classOf[Byte])(idx).toShort
      if (b < 0) (maxVal + b).toShort else b
    }

    def store(p: Pointer[_], idx: Int, elem: Short) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[Byte])(idx) = elem.toByte
    }

    def isValid(sh: Short) = 0 <= sh && sh < maxVal
  }

  object MpiUnsignedChar extends Uint8(mpi2.lib.MPI_UNSIGNED_CHAR)

  object MpiUint8 extends Uint8(mpi2.lib.MPI_UINT8_T)

  sealed abstract class Int16(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Short](dt) {
    assume(extent.range == AlignHelper.shortByteSize)

    val alignment = AlignHelper.shortAlignment

    def load(p: Pointer[_], idx: Int): Short =
      p.as(classOf[Short])(idx)

    def store(p: Pointer[_], idx: Int, elem: Short) {
      p.as(classOf[Short])(idx) = elem
    }
  }

  implicit object MpiShort extends Int16(mpi2.lib.MPI_SHORT)

  object MpiInt16 extends Int16(mpi2.lib.MPI_INT16_T)

  sealed abstract class Uint16(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Int](dt) {
    assume(extent.range == AlignHelper.shortByteSize)

    val alignment = AlignHelper.shortAlignment

    val maxVal = 2 * (Short.MaxValue + 1)

    def load(p: Pointer[_], idx: Int): Int = {
      val v = p.as(classOf[Short])(idx).toInt
      if (v < 0) maxVal + v else v
    }

    def store(p: Pointer[_], idx: Int, elem: Int) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[Short])(idx) = elem.toShort
    }

    def isValid(int: Int): Boolean = 0 <= int && int < maxVal
  }

  object MpiUnsignedShort extends Uint16(mpi2.lib.MPI_UNSIGNED_SHORT)

  object MpiUint16 extends Uint16(mpi2.lib.MPI_UINT16_T)

  sealed abstract class Int32(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Int](dt) {
    assume(extent.range == AlignHelper.intByteSize)

    val alignment = AlignHelper.intAlignment

    def load(p: Pointer[_], idx: Int): Int =
      p.as(classOf[Int])(idx)

    def store(p: Pointer[_], idx: Int, elem: Int) {
      p.as(classOf[Int])(idx) = elem
    }
  }

  implicit object MpiInt extends Int32(mpi2.lib.MPI_INT)

  object MpiInt32 extends Int32(mpi2.lib.MPI_INT32_T)

  sealed abstract class Uint32(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Long](dt) {
    assume(extent.range == AlignHelper.intByteSize)

    val alignment = AlignHelper.intAlignment

    val maxVal = 2 * (Int.MaxValue + 1L)

    def load(p: Pointer[_], idx: Int): Long = {
      val l = p.as(classOf[Int])(idx).toLong
      if (l < 0) maxVal + l else l
    }

    def store(p: Pointer[_], idx: Int, elem: Long) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[Int])(idx) = elem.toInt
    }

    def isValid(lg: Long): Boolean = 0 <= lg && lg < maxVal
  }

  object MpiUnsigned extends Uint32(mpi2.lib.MPI_UNSIGNED)

  object MpiUint32 extends Uint32(mpi2.lib.MPI_UINT32_T)

  object MpiLong extends PredefDatatype[Long](mpi2.lib.MPI_LONG) {
    assume(extent.range == AlignHelper.cLongByteSize)

    val alignment = AlignHelper.cLongAlignment

    val maxVal = {
      val cLongSize = 8 * CLong.SIZE
      if (cLongSize < java.lang.Long.SIZE) (1L << (cLongSize - 1))
      else Long.MaxValue
    }

    val minVal = {
      val cLongSize = 8 * CLong.SIZE
      if (cLongSize < java.lang.Long.SIZE) -(1L << (cLongSize - 1))
      else Long.MinValue
    }

    def load(p: Pointer[_], idx: Int): Long =
      p.as(classOf[CLong])(idx).longValue

    def store(p: Pointer[_], idx: Int, elem: Long) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[CLong])(idx) = CLong.valueOf(elem)
    }

    def isValid(lg: Long): Boolean = minVal <= lg && lg < maxVal
  }

  sealed abstract class Int64(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Long](dt) {
    assume(extent.range == AlignHelper.longByteSize)

    val alignment = AlignHelper.longAlignment

    def load(p: Pointer[_], idx: Int): Long =
      p.as(classOf[Long])(idx)

    def store(p: Pointer[_], idx: Int, elem: Long) {
      p.as(classOf[Long])(idx) = elem
    }
  }

  implicit object MpiLongLong extends Int64(mpi2.lib.MPI_LONG_LONG)

  object MpiInt64 extends Int64(mpi2.lib.MPI_INT64_T)

  object MpiUnsignedLong
      extends PredefDatatype[Long](mpi2.lib.MPI_UNSIGNED_LONG) {
    assume(extent.range == AlignHelper.cLongByteSize)

    val alignment = AlignHelper.cLongAlignment

    val maxVal = {
      val cLongSize = 8 * CLong.SIZE
      if (cLongSize < java.lang.Long.SIZE) (1L << cLongSize)
      else Long.MaxValue
    }

    def load(p: Pointer[_], idx: Int): Long = {
      val l = p.as(classOf[CLong])(idx).longValue
      if (l < 0) l + maxVal else l
    }

    def store(p: Pointer[_], idx: Int, elem: Long) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[CLong])(idx) = CLong.valueOf(elem)
    }

    def isValid(lg: Long): Boolean = 0 <= lg && lg < maxVal
  }

  sealed abstract class Uint64(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[Long](dt) {
    // The Scala representation of this type is clearly insufficiently wide; it
    // should perhaps be replaced with something else.
    assume(extent.range == AlignHelper.longByteSize)

    val alignment = AlignHelper.longAlignment

    val maxVal = Long.MaxValue

    def load(p: Pointer[_], idx: Int): Long = {
      val l = p.as(classOf[Long])(idx)
      if (l < 0) l + maxVal else l
    }

    def store(p: Pointer[_], idx: Int, elem: Long) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      p.as(classOf[Long])(idx) = elem
    }

    def isValid(lg: Long): Boolean = 0 <= lg
  }

  object MpiUnsignedLongLong extends Uint64(mpi2.lib.MPI_UNSIGNED_LONG_LONG)

  object MpiUint64 extends Uint64(mpi2.lib.MPI_UINT64_T)

  implicit object MpiFloat extends PredefDatatype[Float](mpi2.lib.MPI_FLOAT) {
    assume(extent.range == AlignHelper.floatByteSize)

    val alignment = AlignHelper.floatAlignment

    def load(p: Pointer[_], idx: Int): Float =
      p.as(classOf[Float])(idx)

    def store(p: Pointer[_], idx: Int, elem: Float) {
      p.as(classOf[Float])(idx) = elem
    }
  }

  implicit object MpiDouble extends PredefDatatype[Double](mpi2.lib.MPI_DOUBLE) {
    assume(extent.range == AlignHelper.doubleByteSize)

    val alignment = AlignHelper.doubleAlignment

    def load(p: Pointer[_], idx: Int): Double =
      p.as(classOf[Double])(idx)

    def store(p: Pointer[_], idx: Int, elem: Double) {
      p.as(classOf[Double])(idx) = elem
    }
  }

  object MpiWchar extends PredefDatatype[Int](mpi2.lib.MPI_WCHAR) {
    assume(extent.range == AlignHelper.intByteSize)

    val alignment = AlignHelper.intAlignment

    def load(p: Pointer[_], idx: Int): Int =
      p.as(classOf[Int])(idx)

    def store(p: Pointer[_], idx: Int, elem: Int) {
      p.as(classOf[Int])(idx) = elem
    }
  }

  implicit object MpiBoolean extends PredefDatatype[Boolean](mpi2.lib.MPI_C_BOOL) {
    assume(extent.range == AlignHelper.byteByteSize)

    val alignment = AlignHelper.byteAlignment

    def load(p: Pointer[_], idx: Int): Boolean =
      p.as(classOf[Byte])(idx) != 0

    def store(p: Pointer[_], idx: Int, elem: Boolean) {
      p.as(classOf[Byte])(idx) = if (elem) 1 else 0
    }
  }

  sealed abstract class FloatComplex(dt: mpi2.lib.MPI_Datatype)
      extends PredefDatatype[(Float, Float)](dt) {
    assume(extent.range == 2 * AlignHelper.floatByteSize)

    val alignment = AlignHelper.floatAlignment

    def load(p: Pointer[_], idx: Int): (Float,Float) = {
      val f = p.as(classOf[Float])
      (f(2 * idx), f(2 * idx + 1))
    }

    def store(p: Pointer[_], idx: Int, elem: (Float,Float)) {
      val f = p.as(classOf[Float])
      elem match {
        case (re, im) => {
          f(2 * idx) = re
          f(2 * idx + 1) = im
        }
      }
    }
  }

  object MpiComplex extends FloatComplex(mpi2.lib.MPI_C_COMPLEX)

  object MpiFloatComplex extends FloatComplex(mpi2.lib.MPI_C_FLOAT_COMPLEX)

  object MpiDoubleComplex
      extends PredefDatatype[(Double, Double)](mpi2.lib.MPI_C_DOUBLE_COMPLEX) {
    assume(extent.range == 2 * AlignHelper.doubleByteSize)

    val alignment = AlignHelper.doubleAlignment

    def load(p: Pointer[_], idx: Int): (Double,Double) = {
      val f = p.as(classOf[Double])
      (f(2 * idx), f(2 * idx + 1))
    }

    def store(p: Pointer[_], idx: Int, elem: (Double,Double)) {
      val f = p.as(classOf[Double])
      elem match {
        case (re, im) => {
          f(2 * idx) = re
          f(2 * idx + 1) = im
        }
      }
    }
  }

  object MpiFloatInt
      extends PredefDatatype[(Float, Int)](mpi2.lib.MPI_FLOAT_INT) {

    val alignment = AlignHelper.floatAlignment

    private val intOffset =
      AlignHelper.align(AlignHelper.floatByteSize, AlignHelper.intAlignment)

    assume(
      extent.range ==
        AlignHelper.align(intOffset + AlignHelper.intByteSize, alignment))

    def load(p: Pointer[_], idx: Int): (Float,Int) = {
      val pf = p.offset(idx * extent.range)
      val pi = pf.offset(intOffset)
      (pf.as(classOf[Float])(0), pi.as(classOf[Int])(0))
    }

    def store(p: Pointer[_], idx: Int, elem: (Float,Int)) {
      val pf = p.offset(idx * extent.range)
      val pi = pf.offset(intOffset)
      elem match {
        case (flt, int) => {
          pf.as(classOf[Float])(0) = flt
          pi.as(classOf[Int])(0) = int
        }
      }
    }
  }

  object MpiDoubleInt
      extends PredefDatatype[(Double, Int)](mpi2.lib.MPI_DOUBLE_INT) {

    val alignment = AlignHelper.doubleAlignment

    private val intOffset =
      AlignHelper.align(AlignHelper.doubleByteSize, AlignHelper.intAlignment)

    assume(
      extent.range ==
        AlignHelper.align(intOffset + AlignHelper.intByteSize, alignment))

    def load(p: Pointer[_], idx: Int): (Double,Int) = {
      val pf = p.offset(idx * extent.range)
      val pi = pf.offset(intOffset)
      (pf.as(classOf[Double])(0), pi.as(classOf[Int])(0))
    }

    def store(p: Pointer[_], idx: Int, elem: (Double,Int)) {
      val pf = p.offset(idx * extent.range)
      val pi = pf.offset(intOffset)
      elem match {
        case (dbl, int) => {
          pf.as(classOf[Double])(0) = dbl
          pi.as(classOf[Int])(0) = int
        }
      }
    }
  }

  object MpiLongInt
      extends PredefDatatype[(Long, Int)](mpi2.lib.MPI_LONG_INT) {
    val alignment = AlignHelper.cLongAlignment

    private val intOffset =
      AlignHelper.align(AlignHelper.cLongByteSize, AlignHelper.intAlignment)

    assume(
      extent.range ==
        AlignHelper.align(intOffset + AlignHelper.intByteSize, alignment))

    val maxLongVal = {
      val cLongSize = 8 * CLong.SIZE
      if (cLongSize < java.lang.Long.SIZE) (1L << (cLongSize - 1))
      else Long.MaxValue
    }

    val minLongVal = {
      val cLongSize = 8 * CLong.SIZE
      if (cLongSize < java.lang.Long.SIZE) -(1L << (cLongSize - 1))
      else Long.MinValue
    }

    def load(p: Pointer[_], idx: Int): (Long,Int) = {
      val pl = p.offset(idx * extent.range)
      val pi = pl.offset(intOffset)
      (pl.as(classOf[CLong])(0).longValue, pi.as(classOf[Int])(0))
    }

    def store(p: Pointer[_], idx: Int, elem: (Long,Int)) {
      require(isValid(elem), mpi2.invalidValueErrorMsg)
      val pl = p.offset(idx * extent.range)
      val pi = pl.offset(intOffset)
      elem match {
        case (lg, int) => {
          pl.as(classOf[CLong])(0) = CLong.valueOf(lg)
          pi.as(classOf[Int])(0) = int
        }
      }
    }

    def isValid(li: (Long, Int)): Boolean =
      minLongVal <= li._1 && li._1 < maxLongVal
  }

  object MpiIntInt
      extends PredefDatatype[(Int, Int)](mpi2.lib.MPI_2INT) {
    assume(extent.range == 2 * AlignHelper.intByteSize)

    val alignment = AlignHelper.intAlignment

    def load(p: Pointer[_], idx: Int): (Int,Int) = {
      val pi = p.as(classOf[Int])
      (pi(2 * idx), pi(2 * idx + 1))
    }

    def store(p: Pointer[_], idx: Int, elem: (Int,Int)) {
      val pi = p.as(classOf[Int])
      elem match {
        case (i1, i2) => {
          pi(2 * idx) = i1
          pi(2 * idx + 1) = i2
        }
      }
    }
  }

  object MpiShortInt
      extends PredefDatatype[(Short, Int)](mpi2.lib.MPI_SHORT_INT) {
    val alignment = AlignHelper.shortAlignment

    private val intOffset =
      AlignHelper.align(AlignHelper.shortByteSize, AlignHelper.intAlignment)

    assume(
      extent.range ==
        AlignHelper.align(intOffset + AlignHelper.intByteSize, alignment))

    def load(p: Pointer[_], idx: Int): (Short,Int) = {
      val ps = p.offset(idx * extent.range).as(classOf[Short])
      val pi = ps.offset(intOffset).as(classOf[Int])
      (ps(0), pi(0))
    }

    def store(p: Pointer[_], idx: Int, elem: (Short,Int)) {
      val ps = p.offset(idx * extent.range)
      val pi = ps.offset(intOffset)
      elem match {
        case (sh, int) => {
          ps.as(classOf[Short])(0) = sh
          pi.as(classOf[Int])(0) = int
        }
      }
    }
  }

  // implicit object MpiAint
  //     extends PredefDatatype[mpi2.lib.MPI_Aint](mpi2.lib.MPI_AINT) {
  //   val alignment = mpi2.aintAlignment

  //   def load(p: Pointer[_], idx: Int): mpi2.lib.MPI_Aint =
  //     p.as(classOf[mpi2.lib.MPI_Aint])(idx)

  //   def store(p: Pointer[_], idx: Int, elem: mpi2.lib.MPI_Aint) {
  //     p.as(classOf[mpi2.lib.MPI_Aint])(idx) = elem
  //   }
  // }

  // object MpiOffset
  //     extends PredefDatatype[mpi2.lib.MPI_Offset](mpi2.lib.MPI_OFFSET) {
  //   val alignment = mpi2.offsetAlignment

  //   def load(p: Pointer[_], idx: Int): mpi2.lib.MPI_Offset =
  //     p.as(classOf[mpi2.lib.MPI_Offset])(idx)

  //   def store(p: Pointer[_], idx: Int, elem: mpi2.lib.MPI_Offset) {
  //     p.as(classOf[mpi2.lib.MPI_Offset])(idx) = elem
  //   }
  // }

  object MpiPacked extends mpi2.Datatype[Any] {
    handlePtr.set(mpi2.lib.MPI_PACKED)
    mpi2.Datatype.register(this)

    override lazy val handle = super.handle

    def free() {}

    val multiplicity = 1

    val alignment = 1

    override def toString(): String = getClass.getSimpleName
  }
}
