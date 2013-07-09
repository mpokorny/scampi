//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable
import scala.collection.generic.CanBuildFrom
import org.bridj.Pointer

trait ValueBufferComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  abstract class ValueBuffer[Elem]
      extends mutable.Iterable[Elem] {

    val datatype: mpi2.Datatype[Elem]

    val region: mpi2.CommRegion

    val valueCount: Int

    val pointer: Pointer[_]

    def :@(idx: Int): mpi2.CommRegionMarker

    override def stringPrefix = "ValueBuffer"
  }

  class SeqValueBuffer[Elem](
    val datatype: mpi2.SeqDatatype[Elem],
    val region: mpi2.CommRegion)
      extends ValueBuffer[Elem]
      with mutable.IndexedSeq[Elem]
      with mutable.IndexedSeqLike[Elem,SeqValueBuffer[Elem]] {
    // NB: "region" spans true extent
    val valueCount =
      if (region != Bottom) {
        val mpi2.Extent(_, range) = datatype.extent
        val mpi2.Extent(_, trueRange) = datatype.trueExtent
        require(region.size >= trueRange)
        ((region.size + range - trueRange) / range).toInt
      } else 0

    val length = valueCount * datatype.multiplicity

    lazy val pointer: Pointer[_] =
      if (region != Bottom) {
        Pointer.pointerToAddress(
          region.pointer.getPeer - datatype.trueExtent.lowerBound,
          classOf[Byte],
          mpi2.noRelease
        )
      }
      else Pointer.NULL

    require(
      region == Bottom ||
        pointer == mpi2.lib.MPI_IN_PLACE ||
        pointer.getPeer % datatype.alignment == 0,
      s"Incorrect ValueBuffer alignment for datatype ${datatype}")

    protected def blockPointer(idx: Int): Pointer[_] = {
      if (region != Bottom)
        pointer.offset((idx / datatype.multiplicity) * datatype.extent.range)
      else
        Pointer.pointerToAddress(
          (idx / datatype.multiplicity) * datatype.extent.range,
          classOf[Byte],
          mpi2.noRelease)
    }

    def apply(idx: Int): datatype.Elem = {
      require(0 <= idx && idx < length, "Index out of bounds")
      datatype.load(blockPointer(idx), idx % datatype.multiplicity)
    }

    def update(idx: Int, elem: datatype.Elem) {
      require(0 <= idx && idx < length, "Index out of bounds")
      datatype.store(blockPointer(idx), idx % datatype.multiplicity, elem)
    }

    def :@(idx: Int): mpi2.CommRegionMarker = {
      if (region != Bottom)
        (region + (idx / datatype.multiplicity) * datatype.extent.range +
          datatype.offsetTo(idx % datatype.multiplicity))
      else
        Bottom + datatype.offsetTo(idx % datatype.multiplicity)
    }

    def @:[W](dv: mpi2.SeqDatatypeVector[W]): SeqValueBuffer[W] =
      dv @: region

    def @:[W](dt: mpi2.SeqDatatype[W]): SeqValueBuffer[W] =
      dt @: region

    def +(idx: Int): mpi2.CommRegionMarker =
      region + idx * datatype.extent.range

    def copy(s: Seq[datatype.Elem]) {
      require(s.length <= length)
      for (i <- 0 until s.length) this(i) = s(i)
    }

    override protected[this] def newBuilder: SeqValueBufferBuilder[Elem] =
      SeqValueBuffer.newBuilder(datatype)

    override def stringPrefix = "SeqValueBuffer"
  }

  class BottomValueBuffer[Elem](
    dt: mpi2.SeqDatatype[Elem],
    override val valueCount: Int)
      extends SeqValueBuffer(dt, Bottom)

  object SeqValueBuffer {
    def apply[V](vs: V*)(implicit datatype: mpi2.SeqDatatype[V]): SeqValueBuffer[V] =
      (newBuilder(datatype) ++= vs).result()

    def alloc[V](length: Int)(
      implicit datatype: mpi2.SeqDatatype[V]): SeqValueBuffer[V] =
      new SeqValueBuffer(datatype, CommBuffer(datatype.trueRange(length)))

    def newBuilder[V](datatype: mpi2.SeqDatatype[V]): SeqValueBufferBuilder[V] =
      new SeqValueBufferBuilder(datatype)

    def newBuilder[V](
      datatype: mpi2.SeqDatatype[V],
      region: mpi2.CommRegion): OneTimeValueBufferBuilder[V] =
      new OneTimeValueBufferBuilder(datatype, region)

    implicit def canBuildFrom[V](implicit datatype: mpi2.SeqDatatype[V]):
        CanBuildFrom[SeqValueBuffer[V],V,SeqValueBuffer[V]] =
      new CanBuildFrom[SeqValueBuffer[V],V,SeqValueBuffer[V]] {
        def apply() = newBuilder(datatype)
        def apply(from: SeqValueBuffer[V]) = newBuilder(from.datatype)
      }
  }

  class SeqValueBufferBuilder[Elem](datatype: mpi2.SeqDatatype[Elem])
      extends mutable.Builder[Elem,SeqValueBuffer[Elem]] {

    protected val defaultLength = 100 max datatype.multiplicity
    protected var maxLength: Int = defaultLength
    protected var length: Int = 0
    protected var optBuffer: Option[SeqValueBuffer[Elem]] = None

    private def valueCount(n: Int) = (
      (n + (
        (datatype.multiplicity - n % datatype.multiplicity) %
          datatype.multiplicity)) /
        datatype.multiplicity)

    protected[this] def initBuffer() {
      val region = mpi2.CommBuffer(datatype.trueRange(valueCount(maxLength)))
      optBuffer = Some(new SeqValueBuffer(datatype, region))
      length = 0
    }

    protected[this] def resizeBuffer() {
      require (optBuffer.isDefined)
      val region = mpi2.CommBuffer(datatype.trueRange(valueCount(maxLength)))
      length = length min maxLength
      optBuffer.get.region.pointer.copyTo(region.pointer, length)
      optBuffer = Some(new SeqValueBuffer(datatype, region))
    }

    override def sizeHint(size: Int) {
      if (maxLength != size) {
        maxLength = size
        if (optBuffer.isDefined)
          resizeBuffer()
      }
    }

    def +=(elem: Elem) = {
      if (!optBuffer.isDefined) initBuffer()
      optBuffer.get(length) = elem
      length += 1
      this
    }

    def clear() {
      optBuffer = None
      maxLength = defaultLength
   }

    def result(): SeqValueBuffer[Elem] = {
      if (!optBuffer.isDefined) initBuffer()
      val fullRegion = optBuffer.get.region
      val restrictedRegion =
        if (length == maxLength)
          fullRegion
        else
          new CommRegionBuffer(
            Some((fullRegion, 0)),
            datatype.trueRange(valueCount(length)))
      val result = new SeqValueBuffer(datatype, restrictedRegion)
      clear()
      result
    }
  }

  class OneTimeValueBufferBuilder[Elem](
    datatype: mpi2.SeqDatatype[Elem],
    region: mpi2.CommRegion)
      extends SeqValueBufferBuilder[Elem](datatype) {

    private var initialized = false

    override protected[this] def initBuffer() {
      require(!initialized, "FIXME")
      val buffer = new SeqValueBuffer(datatype, region)
      maxLength = buffer.length
      length = 0
      optBuffer = Some(buffer)
      initialized = true
    }

    override protected[this] def resizeBuffer() {
      require(false, "FIXME")
    }

    initBuffer()
  }

  object EmptyBuffer extends ValueBuffer[Byte] {
    val datatype = mpi2.MpiByte
    val region = Bottom
    val pointer = Pointer.NULL
    val valueCount = 0
    def :@(idx: Int) = Bottom + idx
    def iterator = Iterator.empty
  }

  class ValueInPlace[V](v: ValueBuffer[V]) extends ValueBuffer[V] {
    val datatype = v.datatype
    val region = v.region
    val pointer = mpi2.lib.MPI_IN_PLACE
    val valueCount = v.valueCount
    def :@(idx: Int) = v :@ idx
    def iterator = v.iterator
  }

  object ValueInPlace {
    def apply[V](v: ValueBuffer[V]) = new ValueInPlace(v)
  }

  class PackedValueBuffer(
    val comm: mpi2.Comm,
    val region: mpi2.CommRegion,
    val codec: PackedCodec)
      extends ValueBuffer[Any]() {

    val datatype = mpi2.MpiPacked

    val valueCount = 1

    val pointer = region.pointer

    def :@(idx: Int) = region + idx

    def iterator = new Iterator[Any] {

      private var decodedBlocks: Seq[Seq[_]] = Seq.empty

      private var currentBlock: Option[Seq[_]] = None

      private var blockIndex: Int = 0

      private val position = {
        val result = allocateInt()
        result(0) = 0
        result
      }

      private def prepareCurrentBlock() {
        if (!currentBlock.isDefined || blockIndex == currentBlock.get.length) {
          currentBlock = codec.blockSignature(decodedBlocks) match {
            case None =>
              None
            case Some((n: Int, dt: mpi2.SeqDatatype[_])) => {
              val buff = dt.alloc(n)
              mpi2.mpiCall(mpi2.lib.MPI_Unpack(
                pointer,
                pointer.getValidBytes.toInt,
                position,
                buff.pointer,
                n,
                dt.handle,
                comm.handle))
              decodedBlocks = decodedBlocks :+ buff
              Some(buff)
            }
          }
          blockIndex = 0
        }
      }

      def hasNext = {
        prepareCurrentBlock()
        currentBlock.isDefined && blockIndex < currentBlock.get.length
      }

      def next: Any = {
        prepareCurrentBlock()
        val result = currentBlock.get(blockIndex)
        blockIndex += 1
        result
      }
    }

    def newBuiler: PackedValueBufferBuilder =
      new PackedValueBufferBuilder(comm, codec)
  }

  class PackedValueBufferBuilder(comm: mpi2.Comm, codec: PackedCodec)
      extends mutable.Builder[Any, PackedValueBuffer] {

    private val region = mpi2.CommBuffer(codec.maxSize)

    private var encodedBlocks: Seq[Seq[_]] = Seq.empty

    private var currentBlock: Option[SeqValueBuffer[_]] = None

    private var blockIndex: Int = 0

    private val position = {
      val result = allocateInt()
      result(0) = 0
      result
    }

    private def prepareCurrentBlock() {
      if (currentBlock.isDefined && blockIndex == currentBlock.get.length) {
        val block = currentBlock.get
        mpi2.mpiCall(mpi2.lib.MPI_Pack(
          block.pointer,
          block.valueCount,
          block.datatype.handle,
          region.pointer,
          region.size.toInt,
          position,
          comm.handle))
        encodedBlocks = encodedBlocks :+ block
        currentBlock = None
      }
      if (!currentBlock.isDefined) {
        blockIndex = 0
        currentBlock = codec.blockSignature(encodedBlocks) match {
          case None =>
            None
          case Some((n: Int, dt: mpi2.SeqDatatype[_])) =>
            Some(dt.alloc(n))
        }
      }
    }

    def +=(elem: Any): this.type = {
      prepareCurrentBlock()
      currentBlock match {
        case Some(cb) =>
          cb(blockIndex) = elem.asInstanceOf[cb.datatype.Elem]
        case None =>
          throw new PackedLengthException
      }
      blockIndex += 1
      this
    }

    def clear() {
      encodedBlocks = Seq.empty
      currentBlock = None
      blockIndex = 0
      position(0) = 0
    }

    def result(): PackedValueBuffer = {
      prepareCurrentBlock()
      if (!currentBlock.isDefined)
        new PackedValueBuffer(comm, region, codec)
      else
        throw new PackedLengthException
    }
  }

  object PackedValueBuffer {
    def apply(comm: mpi2.Comm, codec: PackedCodec, vs: Any*): PackedValueBuffer =
      (newBuilder(comm, codec) ++= vs).result()

    def alloc(comm: mpi2.Comm, codec: PackedCodec): PackedValueBuffer =
      new PackedValueBuffer(comm, CommBuffer(codec.maxSize), codec)

    def newBuilder(comm: mpi2.Comm, codec: PackedCodec): PackedValueBufferBuilder =
      new PackedValueBufferBuilder(comm, codec)
  }

  abstract class PackedCodec {
    val maxSize: Long

    def blockSignature(blocks: Seq[Seq[_]]): Option[(Int, mpi2.SeqDatatype[_])]
  }

  abstract class FixedPackedCodec extends PackedCodec {
    val datatypeSequence: Seq[(Int, mpi2.SeqDatatype[_])]

    private lazy val liftedDatatypeSequence = datatypeSequence.lift

    lazy val maxSize: Long = (0L /: datatypeSequence) {
      case (size, (len, dt)) =>
        AlignHelper.align(size, dt.alignment) + dt.trueRange(len)
    }

    def blockSignature(blocks: Seq[Seq[_]]) =
      liftedDatatypeSequence(blocks.length)
  }

  def cStringValueBuffer(str: String): SeqValueBuffer[Char] = {
    val buff = mpi2.MpiChar.alloc(str.length + 1)
    buff.pointer.setCString(str)
    buff
  }
}
