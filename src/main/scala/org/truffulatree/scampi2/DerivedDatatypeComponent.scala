//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable
import scala.util.Random
import mutable.{Seq => MSeq}
import org.bridj.Pointer

trait DerivedDatatypeComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  abstract class DerivedDatatype[V] extends mpi2.SeqDatatype[V] {

    def free() {
      if (!isNull) {
        mpi2.Datatype.remove(this)
        mpi2.mpiCall(mpi2.lib.MPI_Type_free(handlePtr))
      }
    }

    override def toString(): String =
      s"${getClass.getSimpleName}(${handle})"

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int

    private def register() {
      mpi2.mpiCall(dtCreate(handlePtr))
      mpi2.Datatype.register(this)
    }
    register()
  }

  //
  // Dup datatypes
  //
  class DupDatatype[V](val basisType: mpi2.SeqDatatype[V])
      extends DerivedDatatype[V] {

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int =
      mpi2.lib.MPI_Type_dup(basisType.handle, dt)

    val multiplicity = basisType.multiplicity

    val alignment = basisType.alignment

    def load(p: Pointer[_], idx: Int): V = basisType.load(p, idx)

    def store(p: Pointer[_], idx: Int, elem: V) {
      basisType.store(p, idx, elem)
    }

    def offsetTo(idx: Int): Long = basisType.offsetTo(idx)
  }

  //
  // Resized datatypes
  //
  class ResizedDatatype[V](
    val basisType: mpi2.SeqDatatype[V],
    newExtent: mpi2.Extent)
      extends DerivedDatatype[V] {

    require(
      newExtent.range % basisType.alignment == 0,
      "Basis datatype alignment must divide resized datatype range")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int =
      mpi2.lib.MPI_Type_create_resized(
        basisType.handle,
        newExtent.lowerBound,
        newExtent.range,
        dt)

    val multiplicity = basisType.multiplicity

    val alignment = basisType.alignment

    def load(p: Pointer[_], idx: Int): V =
      basisType.load(p, idx)

    def store(p: Pointer[_], idx: Int, elem: V) {
      basisType.store(p, idx, elem)
    }

    def offsetTo(idx: Int): Long = basisType.offsetTo(idx)
  }

  //
  // Contiguous datatypes
  //
  class ContiguousDatatype[V](
    val basisType: mpi2.SeqDatatype[V],
    val length: Int)
      extends DerivedDatatype[V] {

    require(length >= 0, "ContiguousDatatype length must be non-negative")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int =
      mpi2.lib.MPI_Type_contiguous(length, basisType.handle, dt)

    val multiplicity = length * basisType.multiplicity

    val alignment = basisType.alignment

    protected def resolveIndex(idx: Int) =
      (idx / basisType.multiplicity, idx % basisType.multiplicity)

    protected def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement = blk * basisType.extent.range
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = resolveIndex(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = resolveIndex(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int) = {
      val (blk, off) = resolveIndex(idx)
      blk * basisType.extent.range + basisType.offsetTo(off)
    }
  }

  // //
  // // We allow access to datatype values constructed from blocks of
  // // elements by (block, offset) pairs as well as linear addressing.
  // //

  // class BlockedBuffer[V](
  //     dt: mpi2.TypedDatatype[V],
  //     cRegion: mpi2.CommRegion,
  //     offset: Long,
  //     length: Int,
  //     linearToBlockIndex: Int => Tuple2[Int, Int])
  //     extends mutable.ArrayBuffer[mpi2.ValueSeq[V]] {
  //   def toBlockedSeq: BlockedSeq[V] =
  //     new BlockedSeq[V](dt, cRegion, offset, this, length, linearToBlockIndex)
  // }

  // class BlockedSeq[V](
  //     val datatype: mpi2.TypedDatatype[V],
  //     val region: mpi2.CommRegion,
  //     val initialOffset: Long,
  //     blocks: mutable.Seq[mpi2.ValueSeq[V]],
  //     val length: Int,
  //     linearToBlockIndex: Int => Tuple2[Int, Int])
  //     extends mutable.IndexedSeq[V] with mpi2.ValueSeq[V] {

  //   def apply(idx: Int): V = apply(linearToBlockIndex(idx))

  //   def update(idx: Int, elem: V) { update(linearToBlockIndex(idx), elem) }

  //   def validateBlockIndex(blk: Int, off: Int): Boolean =
  //     0 <= blk && blk < blocks.length && 0 <= off && off < blocks(blk).length

  //   def apply(blockIndex: Tuple2[Int, Int]): V = blockIndex match {
  //     case (blk, off) => {
  //       require(validateBlockIndex(blk, off), mpi2.indexOutOfRangeErrorMsg)
  //       blocks(blk)(off)
  //     }
  //   }
  //   def update(blockIndex: Tuple2[Int, Int], elem: V) =
  //     blockIndex match {
  //       case (blk, off) => {
  //         require(validateBlockIndex(blk, off), mpi2.indexOutOfRangeErrorMsg)
  //         blocks(blk).update(off, elem)
  //       }
  //     }
  //   override def toString: String = {
  //     "BlockedSeq(" +
  //     blocks.map(b => "(" + b.mkString(",") + ")").mkString(",") +
  //     ")"
  //   }
  // }

  //
  // Vector datatypes
  //
  class VectorDatatype[V](
      val basisType: mpi2.SeqDatatype[V],
      val numBlocks: Int,
      val blocklength: Int,
      val stride: Int)
      extends DerivedDatatype[V] {

    require(numBlocks >= 0, "numBlocks must be non-negative")

    require(blocklength >= 0, "blocklength must be non-negative")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int =
      mpi2.lib.MPI_Type_vector(
        numBlocks,
        blocklength,
        stride,
        basisType.handle,
        dt)

    val multiplicity = numBlocks * blocklength * basisType.multiplicity

    val alignment = basisType.alignment

    private val blockMultiplicity = blocklength * basisType.multiplicity

    private val blockStride = stride * basisType.extent.range

    protected def resolveIndex(idx: Int) =
      (idx / blockMultiplicity, idx % blockMultiplicity)

    protected def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement = blk * blockStride
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = resolveIndex(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = resolveIndex(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int): Long = {
      val (blk, off) = resolveIndex(idx)
      blk * blockStride + basisType.offsetTo(off)
    }
  }

  //
  // Hvector datatypes
  //
  class HvectorDatatype[V](
    val basisType: mpi2.SeqDatatype[V],
    val numBlocks: Int,
    val blocklength: Int,
    val stride: mpi2.lib.MPI_Aint)
      extends DerivedDatatype[V] {

    require(numBlocks >= 0, "numBlocks must be non-negative")

    require(blocklength >= 0, "blocklength must be non-negative")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int =
      mpi2.lib.MPI_Type_create_hvector(
        numBlocks,
        blocklength,
        stride,
        basisType.handle,
        dt)

    val multiplicity = numBlocks * blocklength * basisType.multiplicity

    val alignment = basisType.alignment

    private val blockMultiplicity = blocklength * basisType.multiplicity

    protected def resolveIndex(idx: Int) =
      (idx / blockMultiplicity, idx % blockMultiplicity)

    protected def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement = blk * stride
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = resolveIndex(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = resolveIndex(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int): Long = {
      val (blk, off) = resolveIndex(idx)
      blk * stride + basisType.offsetTo(off)
    }
  }

  //
  // Helper class for vector composed of irregular blocks
  //
  private class BlockIndexer(blocklengths: Seq[Int]) {
    val (length, blockIdxLimits) = {
      ((0, List.empty[(Int, Int, Int)]) /: blocklengths) {
        case ((start, seq), len) => {
          val b = if (seq.isEmpty) 0 else seq.head._1 + 1
          (start + len, (b, start, start + len) :: seq)
        }
      } match {
        case (len, lim) => (len, lim.reverse)
      }
    }
    def apply(idx: Int): (Int, Int) = {
      ((Option.empty[(Int, Int)] /: blockIdxLimits) {
        case (None, (b, lo, hi)) if lo <= idx && idx < hi =>
          Some((b, idx - lo))
        case (acc, _) => acc
      }).get
    }
  }

  //
  // Indexed datatypes
  //
  class IndexedDatatype[V](
      val basisType: mpi2.SeqDatatype[V],
      val blockSeq: Seq[mpi2.Block])
      extends DerivedDatatype[V] {

    require(blockSeq.length >= 0, "Number of blocks must be non-negative")

    require(
      blockSeq.forall(b => 0 <= b.length),
      "Block lengths must all be non-negative")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int = {
      val buffer =
        if (blockSeq.length > 0)
          Pointer.allocateInts(2 * blockSeq.length).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val lengths = buffer
        val displacements =
          if (blockSeq.length > 0) buffer.next(blockSeq.length)
          else buffer
        for (i <- 0 until blockSeq.length) {
          lengths(i) = blockSeq(i).length
          displacements(i) = blockSeq(i).displacement
        }
        mpi2.lib.MPI_Type_indexed(
          blockSeq.length,
          lengths,
          displacements,
          basisType.handle,
          dt)
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    private val indexer =
      new BlockIndexer(blockSeq.map(_.length * basisType.multiplicity))

    val multiplicity = indexer.length

    val alignment = basisType.alignment

    private def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement = blockSeq(blk).displacement * basisType.extent.range
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = indexer(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = indexer(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int): Long = {
      val (blk, off) = indexer(idx)
      (blockSeq(blk).displacement * basisType.extent.range +
        basisType.offsetTo(off))
    }
  }

  //
  // Hindexed datatypes
  //
  class HindexedDatatype[V](
      val basisType: mpi2.SeqDatatype[V],
      val blockSeq: Seq[mpi2.HBlock])
      extends DerivedDatatype[V] {

    require(blockSeq.length >= 0, "Number of blocks must be non-negative")

    require(
      blockSeq.forall(b => 0 <= b.length),
      "Block lengths must all be non-negative")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int = {
      val (lengths, displacements) =
        if (blockSeq.length > 0)
          (Pointer.allocateInts(blockSeq.length).as(classOf[Int]),
            mpi2.allocateAint(blockSeq.length))
        else
          (nullPointer[Int], nullPointer[mpi2.lib.MPI_Aint])
      try {
        for (i <- 0 until blockSeq.length) {
          lengths(i) = blockSeq(i).length
          displacements(i) = blockSeq(i).displacement
        }
        mpi2.lib.MPI_Type_create_hindexed(
          blockSeq.length,
          lengths,
          displacements,
          basisType.handle,
          dt)
      }
      finally {
        if (displacements != Pointer.NULL) displacements.release()
        if (lengths != Pointer.NULL) lengths.release()
      }
    }

    private val indexer =
      new BlockIndexer(blockSeq.map(_.length * basisType.multiplicity))

    val multiplicity = indexer.length

    val alignment = basisType.alignment

    private def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement = blockSeq(blk).displacement
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = indexer(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = indexer(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int): Long = {
      val (blk, off) = indexer(idx)
      blockSeq(blk).displacement + basisType.offsetTo(off)
    }
  }

  //
  // Indexed block datatypes
  //
  class IndexedBlockDatatype[V](
      val basisType: mpi2.SeqDatatype[V],
      val blocklength: Int,
      val displacements: Seq[Int])
      extends DerivedDatatype[V] {

    require(displacements.length >= 0, "Number of blocks must be at least one")

    require(blocklength >= 0, "Block length must be non-negative")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int = {
      val dspls =
        if (displacements.length > 0)
          Pointer.pointerToInts(displacements:_*).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        mpi2.lib.MPI_Type_create_indexed_block(
          displacements.length,
          blocklength,
          dspls,
          basisType.handle,
          dt)
      } finally {
        if (dspls != Pointer.NULL) dspls.release()
      }
    }

    val multiplicity = blocklength * basisType.multiplicity

    val alignment = basisType.alignment

    private val blockMultiplicity = blocklength * basisType.multiplicity

    protected def resolveIndex(idx: Int) =
      (idx / blockMultiplicity, idx % blockMultiplicity)

    protected def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement = displacements(blk)
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = resolveIndex(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = resolveIndex(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int): Long = {
      val (blk, off) = resolveIndex(idx)
      displacements(idx) + basisType.offsetTo(off)
    }
  }

  //
  // Struct datatypes
  //
  class StructDatatype(blks: StructBlock[_]*)
      extends DerivedDatatype[Any] {

    require(blks.forall(_.length >= 0), mpi2.structBlockLengthErrorMsg)

    val blocks = mpi2.StructBlock.withDisplacements(blks)

    protected case class ExpandedBlock[V](
      datatype: mpi2.SeqDatatype[V],
      length: Int,
      displacement: mpi2.lib.MPI_Aint)

    protected val expandedBlocks = blocks.map {
      case StructBlock(dt, len, disp) =>
        ExpandedBlock(dt, len * dt.multiplicity, disp.get)
    }

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int = {
      // class member "blocks" does not exist at the time this function is
      // called
      val blocks = mpi2.StructBlock.withDisplacements(blks)
      require(
        mpi2.StructBlock.displacementsAreValid(blocks),
        mpi2.structBlocksAlignmentErrorMsg)
      val lengths = Pointer.allocateInts(blocks.length).as(classOf[Int])
      val addresses = mpi2.allocateAint(blocks.length)
      val datatypes = mpi2.allocateDatatype(blocks.length)
      try {
        for (i <- 0 until blocks.length) {
          lengths(i) = blocks(i).length
          addresses(i) = blocks(i).displacement.get
          datatypes(i) = blocks(i).datatype.handle
        }
        mpi2.lib.MPI_Type_create_struct(
          blocks.length,
          lengths,
          addresses,
          datatypes,
          dt)
      } finally {
        datatypes.release()
        addresses.release()
        lengths.release()
      }
    }

    val multiplicity = (0 /: expandedBlocks) {
      case (sum, ExpandedBlock(dt, len, _)) => sum + len
    }

    val alignment = (0 /: blocks) {
      case (mx, StructBlock(dt, _, _)) => dt.alignment max mx
    }

    private def resolveIndex(idx: Int): (ExpandedBlock[_], (Int, Int)) =
      ((Option.empty[ExpandedBlock[_]], idx) /: expandedBlocks) {
        case ((None, i), b@ExpandedBlock(_, len, _)) =>
          if (i < len) (Some(b), i)
          else (None, i - len)
        case (res, _) => res
      } match {
        case (Some(b), i) =>
          (b, (i / b.datatype.multiplicity, i % b.datatype.multiplicity))
        case (None, _) =>
          throw new IndexOutOfBoundsException
      }

    private def blockPointer(p: Pointer[_], blk: ExpandedBlock[_], idx: Int):
        Pointer[_] = {
      blk match {
        case ExpandedBlock(dt, len, disp) => {
          if (p != Pointer.NULL)
            p.offset(disp + idx * dt.extent.range)
          else
            Pointer.pointerToAddress(
              disp + idx * dt.extent.range,
              classOf[Byte],
              mpi2.noRelease)
        }
      }
    }

    def load(p: Pointer[_], idx: Int): Any = {
      resolveIndex(idx) match {
        case (b@ExpandedBlock(dt, _, disp), (dtIdx, remIdx)) =>
          dt.load(blockPointer(p, b, dtIdx), remIdx)
      }
    }

    def store(p: Pointer[_], idx: Int, elem: Any) {
      resolveIndex(idx) match {
        case (b@ExpandedBlock(dt, _, disp), (dtIdx, remIdx)) =>
          dt.store(blockPointer(p, b, dtIdx), remIdx, elem)
      }
    }

    def offsetTo(idx: Int): Long = {
      resolveIndex(idx) match {
        case (ExpandedBlock(dt, _, disp), (dtIdx, remIdx)) =>
          disp + dtIdx * dt.extent.range + dt.offsetTo(remIdx)
      }
    }
  }

  // //
  // // We allow access to subarray values by a list of coordinates as well
  // // as linear addressing. The methods in SubarraySeq[V] have been
  // // placed into an abstract class to avoid a problem that would occur
  // // with a structural refinement of mutable.IndexedSeq when we
  // // introduce the update method using coordinates (see SLS 3.2.7).
  // //
  // abstract class SubarraySeq[V](
  //   rowMajorDims: Seq[mpi2.SubarrayDim],
  //   projArraySizes: List[Int],
  //   order: mpi2.ArrayOrder.ArrayOrder)
  //     extends mutable.IndexedSeq[V] {
  //   protected val subarray: mpi2.ValueSeq[V]

  //   // NB: subarrayCoords must be in reverse order for ColumnMajor everywhere

  //   protected val subarrayRootArrayIndex =
  //     subarrayCoords2arrayIndex(List.fill(rowMajorDims.length)(0))

  //   protected def subarrayCoords2arrayIndex(subarrayCoords: List[Int]): Int = {
  //     val arrayCoords = subarrayCoords.zip(rowMajorDims).map(_ match {
  //       case (i, d) => i + d.start
  //     })
  //     arrayCoords.zip(projArraySizes.tail ::: List(1)).foldLeft(0)((acc, z) => {
  //       z match {
  //         case (i, s) => i * s + acc
  //       }
  //     })
  //   }

  //   protected def subarrayCoords2subarrayIndex(
  //       subarrayCoords: List[Int]): Int = {
  //     subarrayCoords2arrayIndex(subarrayCoords) - subarrayRootArrayIndex
  //   }

  //   protected val subsizes = rowMajorDims.map(_.subsize)

  //   private def validateSubarrayCoords(subarrayCoords: List[Int]): Boolean = {
  //     ((subarrayCoords.length == subsizes.length) &&
  //       subarrayCoords.zip(subsizes).forall(_ match {
  //         case (i, d) => 0 <= i && i < d
  //       }))
  //   }

  //   private val reorderedCoords = order match {
  //     case ArrayOrder.RowMajor =>
  //       (c: List[Int]) => c
  //     case ArrayOrder.ColumnMajor =>
  //       (c: List[Int]) => c.reverse
  //   }

  //   def apply(coords: List[Int]): V = {
  //     val c = reorderedCoords(coords)
  //     require(validateSubarrayCoords(c), mpi2.indexOutOfRangeErrorMsg)
  //     subarray(subarrayCoords2subarrayIndex(c))
  //   }

  //   def update(coords: List[Int], elem: V) {
  //     val c = reorderedCoords(coords)
  //     require(validateSubarrayCoords(c), mpi2.indexOutOfRangeErrorMsg)
  //     subarray.update(subarrayCoords2subarrayIndex(c), elem)
  //   }
  // }

  //
  // Subarray datatypes
  //
  class SubarrayDatatype[V](
    val basisType: mpi2.SeqDatatype[V],
    val dims: Seq[mpi2.SubarrayDim],
    val order: mpi2.ArrayOrder.ArrayOrder)
      extends DerivedDatatype[V] {
    require(dims.length >= 0, "Number of dimensions must be non-negative")
    require(
      dims.forall(_ match {
        case mpi2.SubarrayDim(size, subsize, start) =>
          0 <= subsize && subsize <= size && 0 <= start && start <= size - subsize
      }),
      "All subarray dimensions must fit inside parent array")

    protected def dtCreate(dt: Pointer[mpi2.lib.MPI_Datatype]): Int = {
      val buffer =
        if (dims.length > 0)
          Pointer.allocateInts(3 * dims.length).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val sizes = buffer
        val subsizes =
          if (dims.length > 0) buffer.next(dims.length) else buffer
        val starts =
          if (dims.length > 0) buffer.next(2 * dims.length) else buffer
        for (i <- 0 until dims.length) {
          sizes(i) = dims(i).size
          subsizes(i) = dims(i).subsize
          starts(i) = dims(i).start
        }
        mpi2.lib.MPI_Type_create_subarray(
          dims.length,
          sizes,
          subsizes,
          starts,
          order.id,
          basisType.handle,
          dt)
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    private val rowMajorDims = order match {
      case ArrayOrder.RowMajor => dims
      case ArrayOrder.ColumnMajor => dims.reverse
    }

    private val (projArraySizes, projSubarraySizes) =
      (rowMajorDims :\ (List.empty[Int], List.empty[Int])) {
        case (d, (Nil, Nil)) =>
          (d.size :: Nil, d.subsize :: Nil)
        case (d, (sz, ssz)) =>
          (d.size * sz.head :: sz, d.subsize * ssz.head :: ssz)
      }

    private def subarrayCoords2arrayIndex(subarrayCoords: List[Int]): Int = {
      val arrayCoords = subarrayCoords.zip(rowMajorDims).map {
        case (i, d) => i + d.start
      }
      (0 /: arrayCoords.zip(projArraySizes.tail ::: List(1))) {
        case (acc, (i, s)) => i * s + acc
      }
    }

    private val subarrayRootArrayIndex =
      subarrayCoords2arrayIndex(List.fill(rowMajorDims.length)(0))

    protected def subarrayCoords2subarrayIndex(
      subarrayCoords: List[Int]): Int = {
      subarrayCoords2arrayIndex(subarrayCoords) - subarrayRootArrayIndex
    }

    private def idx2arrayIdx(idx: Int): Int = {
      val subarrayCoords = {
        ((idx, List.empty[Int]) /: projSubarraySizes.tail) {
          case ((i, r), d) =>
            (i % d, i / d :: r)
        } match {
          case (i, r) =>
            (i :: r).reverse
        }
      }
      subarrayCoords2arrayIndex(subarrayCoords)
    }

    val alignment = basisType.alignment

    val multiplicity = projSubarraySizes.head * basisType.multiplicity

    protected def resolveIndex(idx: Int) =
      (idx / basisType.multiplicity, idx % basisType.multiplicity)

    protected def blockPointer(p: Pointer[_], blk: Int): Pointer[_] = {
      val displacement =
        (idx2arrayIdx(blk) - subarrayRootArrayIndex) * basisType.extent.range
      if (p != Pointer.NULL)
        p.offset(displacement)
      else
        Pointer.pointerToAddress(displacement, classOf[Byte], mpi2.noRelease)
    }

    def load(p: Pointer[_], idx: Int): V = {
      val (blk, off) = resolveIndex(idx)
      basisType.load(blockPointer(p, blk), off)
    }

    def store(p: Pointer[_], idx: Int, elem: V) {
      val (blk, off) = resolveIndex(idx)
      basisType.store(blockPointer(p, blk), off, elem)
    }

    def offsetTo(idx: Int): Long = {
      val (blk, off) = resolveIndex(idx)
      ((idx2arrayIdx(blk) - subarrayRootArrayIndex) * basisType.extent.range +
        basisType.offsetTo(off))
    }
  }

  //
  // Darray datatypes
  //
  // def createDarray(size: Int, rank: Int, dims: Seq[DarrayDim],
  //                  order: ArrayOrder.ArrayOrder): DarrayDatatype =
  //   Datatype.dtCtorArrays(4 * dims.length, 0, 0) match {
  //     case (buffer, integers, _, _) => try {
  //       val gsizes = integers
  //       val distribs = integers.next(dims.length)
  //       val dargs = integers.next(2 * dims.length)
  //       val psizes = integers.next(3 * dims.length)
  //       var i = 0
  //       dims.foreach((d: DarrayDim) => {
  //         gsizes.set(i, d.gsize)
  //         distribs.set(i, d.distribution.id)
  //         dargs.set(i, d.distArg)
  //         psizes.set(i, d.psize)
  //         i += 1
  //       })
  //       dtCreate(dt =>
  //         MPI_Type_create_darray(
  //           size, rank, dims.length, gsizes, distribs, dargs, psizes,
  //           order.id, handle, dt)).asInstanceOf[DarrayDatatype]
  //     } finally {
  //       buffer.release()
  //     }
  //   }
}
