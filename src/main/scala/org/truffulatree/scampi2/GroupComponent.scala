//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable
import org.bridj.Pointer

trait GroupComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  sealed class Group protected () {
    protected final val handlePtr: Pointer[mpi2.lib.MPI_Group] = {
      val result = allocateGroup()
      result.set(mpi2.lib.MPI_GROUP_NULL)
      result
    }

    protected[scampi2] final def handle = handlePtr(0)

    override def equals(other: Any): Boolean = {
      other.isInstanceOf[Group] &&
      other.asInstanceOf[Group].handle == handle
    }
    override def hashCode: Int = handle.##

    override def finalize() {
      mpi2.lifecycleSync { if (!mpi2.finalized) free() }
      super.finalize()
    }

    def free() {
      if (!isNull) {
        mpi2.mpiCall(mpi2.lib.MPI_Group_free(handlePtr))
      }
    }

    final lazy val size: Int = withOutVar { size: Pointer[Int] =>
      mpi2.mpiCall(mpi2.lib.MPI_Group_size(handle, size))
      size(0)
    }

    final lazy val rank: Option[Int] = withOutVar { rank: Pointer[Int] =>
      mpi2.mpiCall(mpi2.lib.MPI_Group_rank(handle, rank))
      if (rank(0) != mpi2.lib.MPI_UNDEFINED) Some(rank(0))
      else None
    }

    def translateRanks(ranks: Seq[Int], other: Group): Seq[Int] = {
      val result = Pointer.allocateInts(ranks.size).as(classOf[Int])
      try {
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_translate_ranks(
            handle,
            ranks.size,
            Pointer.pointerToInts(ranks:_*).as(classOf[Int]),
            other.handle,
            result))
        result.getInts
      } finally result.release()
    }

    def compare(other: Group): mpi2.Comparison.Comparison =
      withOutVar { comp: Pointer[Int] =>
        mpi2.mpiCall(mpi2.lib.MPI_Group_compare(handle, other.handle, comp))
        mpi2.Comparison(comp(0))
      }

    def union(other: Group): Group =
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(mpi2.lib.MPI_Group_union(handle, other.handle, newGroup))
        Group(newGroup(0))
      }

    def intersection(other: Group): Group =
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_intersection(handle, other.handle, newGroup))
        Group(newGroup(0))
      }

    def difference(other: Group): Group =
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_difference(handle, other.handle, newGroup))
        Group(newGroup(0))
      }

    def incl(ranks: Seq[Int]): Group = {
      require(
        ranks.forall(r => 0 <= r && r < size),
        "All elements of 'ranks' are not valid ranks in group")
      require(
        ranks.distinct.forall(r => ranks.count(_ == r) == 1),
        "All elements of 'ranks' are not distinct")
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_incl(
            handle,
            ranks.size,
            Pointer.pointerToInts(ranks:_*).as(classOf[Int]),
            newGroup))
        Group(newGroup(0))
      }
    }

    def excl(ranks: Seq[Int]): Group = {
      require(ranks.forall(r => 0 <= r && r < size),
        "All elements of 'ranks' are not valid ranks in group")
      require(ranks.distinct.forall(r => ranks.count(_ == r) == 1),
        "All elements of 'ranks' are not distinct")
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_excl(
            handle,
            ranks.size,
            Pointer.pointerToInts(ranks:_*).as(classOf[Int]),
            newGroup))
        Group(newGroup(0))
      }
    }

    def rangeIncl(ranges: Seq[(Int, Int, Int)]): Group = {
      val ranks = ranges flatMap {
        case (first, last, stride) => first.to(last, stride)
      }
      require(
        ranks.forall(r => 0 <= r && r < size),
        "All computed ranks are not valid ranks in group")
      require(
        ranks.distinct.forall(r => ranks.count(_ == r) == 1),
        "All computed ranks are not distinct")
      val flatRanges = ranges flatMap {
        case (first, last, stride) => Seq(first, last, stride)
      }
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_range_incl(
            handle,
            ranges.size,
            Pointer.pointerToInts(flatRanges:_*).as(classOf[Int]),
            newGroup))
        Group(newGroup(0))
      }
    }

    def rangeExcl(ranges: Seq[(Int, Int, Int)]): Group = {
      val ranks = ranges flatMap {
        case (first, last, stride) => first.to(last, stride)
      }
      require(ranks.forall(r => 0 <= r && r < size),
        "All computed ranks are not valid ranks in group")
      require(ranks.distinct.forall(r => ranks.count(_ == r) == 1),
        "All computed ranks are not distinct")
      val flatRanges = ranges flatMap {
        case (first, last, stride) => Seq(first, last, stride)
      }
      withOutVar { newGroup: Pointer[mpi2.lib.MPI_Group] =>
        mpi2.mpiCall(
          mpi2.lib.MPI_Group_range_excl(
            handle,
            ranges.size,
            Pointer.pointerToInts(flatRanges:_*).as(classOf[Int]),
            newGroup))
        Group(newGroup(0))
      }
    }

    def isNull: Boolean = handle == mpi2.lib.MPI_GROUP_NULL
  }

  object GroupEmpty extends Group {
    handlePtr.set(mpi2.lib.MPI_GROUP_EMPTY)
    override def free() {}
  }

  object Group {
    protected[scampi2] def apply(grp: mpi2.lib.MPI_Group): Group = {
      if (grp == GroupEmpty.handle) GroupEmpty
      else if (grp != mpi2.lib.MPI_GROUP_NULL) {
        val result = new Group
        result.handlePtr.set(grp)
        result
      } else throw new mpi2.Exception("Null group cannot be instantiated")
    }
  }
}
