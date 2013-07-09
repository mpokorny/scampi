//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import java.nio.ByteBuffer
import org.bridj.Pointer

trait CommBufferComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  trait CommAddressable {
    val root: CommAddressable

    val offset: Long

    def pointer: Pointer[_]
  }

  // A CommRegion corresponds with MPI's notion of "sequential storage"
  trait CommRegion extends CommAddressable {
    val root: CommRegion

    val size: Long

    def bind[A](dv: mpi2.SeqDatatypeVector[A], at: Long = 0):
        SeqValueBuffer[A] = {
      if (root == this) {
        val trueRange = dv.datatype.trueRange(dv.length)
        val trueLowerBound =
          if (trueRange > 0) at + dv.datatype.trueExtent.lowerBound
          else (0L max at) min size
        require(
          trueLowerBound >= 0,
          "True lower bound of datatype vector must be non-negative")
        require(trueLowerBound + trueRange <= size,
          "True upper bound of datatype vector must not exceed buffer size")
        if (trueRange > 0) {
          val valRegion =
            if (trueLowerBound == 0 && size == trueRange) this
            else slice(trueLowerBound, trueRange)
          new SeqValueBuffer(dv.datatype, valRegion)
        } else {
          new BottomValueBuffer(dv.datatype, 0)
        }
      } else root.bind(dv, at + offset)
    }

    final def @:[A](dv: mpi2.SeqDatatypeVector[A]): SeqValueBuffer[A] =
      bind(dv, 0)

    final def @:[A](dt: mpi2.SeqDatatype[A]): SeqValueBuffer[A] = {
      val mpi2.Extent(_, range) = dt.extent
      val mpi2.Extent(_, trueRange) = dt.trueExtent
      val length = ((size + range - trueRange) / range).toInt
      require(length > 0, "Region too small to bind datatype")
      (dt * length) @: this
    }

    final def +(delta: Long) = CommRegionMarker(this, delta)

    final def -(delta: Long) = CommRegionMarker(this, -delta)

    protected def slice(at: Long, len: Long) =
      new CommRegionBuffer(Some((this, at)), len)
  }

  case class CommRegionMarker(val root: CommRegion, val offset: Long)
      extends CommAddressable {
    lazy val pointer = root.pointer.offset(offset)

    def @:[A](dv: mpi2.SeqDatatypeVector[A]): SeqValueBuffer[A] =
      root.bind(dv, offset)

    def @:[A](dt: mpi2.SeqDatatype[A]): SeqValueBuffer[A] = {
      val mpi2.Extent(_, range) = dt.extent
      val mpi2.Extent(_, trueRange) = dt.trueExtent
      val length = ((root.size + range - trueRange) / range).toInt
      require(length > 0, "Region too small to bind datatype")
      root.bind(dt * length, offset)
    }

    def +(delta: Long) = CommRegionMarker(root, offset + delta)

    def -(delta: Long) = CommRegionMarker(root, offset - delta)
  }

  class CommBuffer(protected var ptr: Pointer[_], val size: Long)
      extends CommRegion {
    require(0 <= size, "Size must be non-negative")

    def pointer = synchronized { ptr }

    val root: CommRegion = this

    val offset: Long = 0
  }

  class CommRegionBuffer(parent: Option[(CommRegion, Long)], size: Long)
      extends CommBuffer(
        parent map {
          case (cr, off) =>
            Pointer.pointerToAddress(cr.pointer.getPeer + off, size, mpi2.noRelease)
        } getOrElse(Pointer.allocateBytes(size)),
        size) {
    require(
      parent map { case (cb, off) => 0 <= off && off + size <= cb.size }
        getOrElse(true),
      "Buffer must fit inside parent region")

    override def finalize() {
      release()
      super.finalize()
    }

    override val root: CommRegion =
      parent map { case (cb, off) => cb.root } getOrElse(this)

    override val offset: Long =
      parent map { case (cb, off) => off + cb.offset } getOrElse(0)

    def release() {
      synchronized {
        if (ptr != Pointer.NULL) ptr.release()
        ptr = Pointer.NULL
      }
    }
  }

  object CommBuffer {
    def apply(size: Long): CommBuffer =
      new CommRegionBuffer(None, size)

    def apply(ptr: Pointer[_], size: Long): CommBuffer =
      new CommBuffer(ptr, size)
  }

  object Bottom extends CommRegion {
    val pointer = mpi2.lib.MPI_BOTTOM
    val root = this
    val offset = 0L
    val size = Long.MaxValue
    val buffer = None
    override def bind[A](
      dv: mpi2.SeqDatatypeVector[A],
      at: Long = 0): SeqValueBuffer[A] = {
      // val size = dv.datatype.trueRange(dv.length)
      // val region =
      //   new CommBuffer(Pointer.pointerToAddress(at, size, mpi2.noRelease), size)
      new BottomValueBuffer(dv.datatype, dv.length)
    }
  }

  def getAddress(ptr: Pointer[_]): mpi2.lib.MPI_Aint =
    withOutVar { address: Pointer[mpi2.lib.MPI_Aint] =>
      mpi2.mpiCall(mpi2.lib.MPI_Get_address(ptr, address))
      address(0)
    }
}
