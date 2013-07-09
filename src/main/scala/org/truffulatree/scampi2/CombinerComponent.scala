//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait CombinerComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  sealed abstract class Combiner

  case object NamedCombiner extends Combiner

  case class DupCombiner(datatype: mpi2.lib.MPI_Datatype) extends Combiner

  case class ContiguousCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    count: Int)
      extends Combiner

  case class VectorCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    count: Int,
    blocklength: Int,
    stride: Int)
      extends Combiner

  case class HvectorCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    count: Int,
    blocklength: Int,
    stride: mpi2.lib.MPI_Aint)
      extends Combiner

  case class IndexedCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    blocks: Seq[mpi2.Block])
      extends Combiner

  case class HindexedCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    blocks: Seq[mpi2.HBlock])
      extends Combiner

  case class IndexedBlockCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    blocklength: Int,
    displacements: Seq[Int])
      extends Combiner

  case class StructCombiner(blocks: Seq[mpi2.TypeBlock]) extends Combiner

  case class SubarrayCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    dims: Seq[SubarrayDim],
    order: mpi2.ArrayOrder.ArrayOrder)
      extends Combiner

  case class DarrayCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    size: Int,
    rank: Int,
    dims: Seq[DarrayDim],
    order: mpi2.ArrayOrder.ArrayOrder)
      extends Combiner

  case class ResizedCombiner(
    datatype: mpi2.lib.MPI_Datatype,
    extent: mpi2.Extent)
      extends Combiner

  object Combiner {
    def apply(dt: mpi2.lib.MPI_Datatype): Combiner = {
      val args = Pointer.allocateInts(4).as(classOf[Int])
      try {
        val numIntegers = args
        val numAddresses = args.next(1)
        val numDatatypes = args.next(2)
        val combiner = args.next(3)
        mpi2.mpiCall(
          mpi2.lib.MPI_Type_get_envelope(
            dt,
            numIntegers,
            numAddresses,
            numDatatypes,
            combiner))
        val result = mpi2.CombinerType(combiner(0)) match {
          case mpi2.CombinerType.Named =>
            NamedCombiner
          case mpi2.CombinerType.Dup =>
            createDup(dt)
          case mpi2.CombinerType.Contiguous =>
            createContiguous(dt)
          case mpi2.CombinerType.Vector =>
            createVector(dt)
          case mpi2.CombinerType.Hvector =>
            createHvector(dt)
          case mpi2.CombinerType.Indexed =>
            createIndexed(dt, numIntegers(0))
          case mpi2.CombinerType.Hindexed =>
            createHindexed(dt, numIntegers(0), numAddresses(0))
          case mpi2.CombinerType.IndexedBlock =>
            createIndexedBlock(dt, numIntegers(0))
          case mpi2.CombinerType.Struct =>
            createStruct(dt, numIntegers(0), numAddresses(0), numDatatypes(0))
          case mpi2.CombinerType.Subarray =>
            createSubarray(dt, numIntegers(0))
          case mpi2.CombinerType.Darray =>
            createDarray(dt, numIntegers(0))
          case mpi2.CombinerType.Resized =>
            createResized(dt)
        }
        result
      } finally args.release()
    }

    private def getContents[A](
      dt: mpi2.lib.MPI_Datatype,
      numIntegers: Int,
      numAddresses: Int,
      numDatatypes: Int)(
      f: (
        Pointer[Int],
        Pointer[mpi2.lib.MPI_Aint],
        Pointer[mpi2.lib.MPI_Datatype]) => A) = {
      val integers = Pointer.allocateInts(numIntegers).as(classOf[Int])
      val addresses = mpi2.allocateAint(numAddresses)
      val datatypes = mpi2.allocateDatatype(numDatatypes)
      mpi2.mpiCall(
        mpi2.lib.MPI_Type_get_contents(
          dt,
          numIntegers,
          numAddresses,
          numDatatypes,
          integers,
          addresses,
          datatypes))
      try {
        f(integers, addresses, datatypes)
      } finally {
        integers.release()
        addresses.release()
        datatypes.release()
      }
    }

    private def createDup(dt: mpi2.lib.MPI_Datatype) =
      getContents(dt, 0, 0, 1) { (ints, addrs, dts) =>
        DupCombiner(dts(0))
      }

    private def createContiguous(dt: mpi2.lib.MPI_Datatype) =
      getContents(dt, 1, 0, 1) { (ints, addrs, dts) =>
        ContiguousCombiner(dts(0), ints(0))
      }

    private def createVector(dt: mpi2.lib.MPI_Datatype) =
      getContents(dt, 3, 0, 1) { (ints, addrs, dts) =>
        val count = ints
        val blocklength = ints.next(1)
        val stride = ints.next(2)
        VectorCombiner(dts(0), count(0), blocklength(0), stride(0))
      }

    private def createHvector(dt: mpi2.lib.MPI_Datatype) =
      getContents(dt, 2, 1, 1) { (ints, addrs, dts) =>
        val count = ints
        val blocklength = ints.next(1)
        HvectorCombiner(dts(0), count(0), blocklength(0), addrs(0))
      }

    private def createIndexed(dt: mpi2.lib.MPI_Datatype, numIntegers: Int) =
      getContents(dt, numIntegers, 0, 1) { (ints, addrs, dts) =>
        val count = ints
        val lengths = ints.next(1)
        val displacements = ints.next(count(0) + 1)
        val blocks = (0 until count(0)) map { i =>
          mpi2.Block(lengths(i), displacements(i))
        }
        IndexedCombiner(dts(0), blocks)
      }

    private def createHindexed(
      dt: mpi2.lib.MPI_Datatype, numIntegers: Int, numAddresses: Int) =
      getContents(dt, numIntegers, numAddresses, 1) { (ints, addrs, dts) =>
        val count = ints
        val lengths = ints.next(1)
        val displacements = addrs
        val hblocks = (0 until count(0)) map { i =>
          mpi2.HBlock(lengths(i), displacements(i))
        }
        HindexedCombiner(dts(0), hblocks)
      }

    private def createIndexedBlock(dt: mpi2.lib.MPI_Datatype, numIntegers: Int) =
      getContents(dt, numIntegers, 0, 1) { (ints, addrs, dts) =>
        val count = ints
        val length = ints.next(1)
        val displacements = ints.next(2)
        IndexedBlockCombiner(
          dts(0),
          length(0),
          displacements.getInts(count(0)))
      }

    private def createStruct(
      dt: mpi2.lib.MPI_Datatype,
      numIntegers: Int,
      numAddresses: Int,
      numDatatypes: Int) =
      getContents(dt, numIntegers, numAddresses, numDatatypes) { (ints, addrs, dts) =>
        val count = ints
        val lengths = ints.next(1)
        val displacements = addrs
        val types = dts
        val blocks = (0 until count(0)) map { i =>
          mpi2.TypeBlock(
            lengths(i),
            displacements(i),
            Datatype.lookup(types(i)).asInstanceOf[mpi2.SeqDatatype[_]])
        }
        StructCombiner(blocks)
      }

    private def createSubarray(dt: mpi2.lib.MPI_Datatype, numIntegers : Int) =
      getContents(dt, numIntegers, 0, 1) { (ints, addrs, dts) =>
        val ndimsVal = ints(0)
        val sizes = ints.next(1)
        val subsizes = ints.next(1 + ndimsVal)
        val starts = ints.next(1 + 2 * ndimsVal)
        val order = ints.next(1 + 3 * ndimsVal)
        val dims = (0 until ndimsVal) map { i =>
          SubarrayDim(sizes(i), subsizes(i), starts(i))
        }
        SubarrayCombiner(dts(0), dims, ArrayOrder(order(0)))
      }

    private def createDarray(dt: mpi2.lib.MPI_Datatype, numIntegers: Int) =
      getContents(dt, numIntegers, 0, 1) { (ints, addrs, dts) =>
        val size = ints
        val rank = ints.next(1)
        val ndimsVal = ints.next(2)(0)
        val gsizes = ints.next(3)
        val distribs = ints.next(3 + ndimsVal)
        val dargs = ints.next(3 + 2 * ndimsVal)
        val psizes = ints.next(3 + 3 * ndimsVal)
        val order = ints.next(3 + 4 * ndimsVal)
        val dims = (0 until ndimsVal) map { i =>
          DarrayDim(
            gsizes(i),
            ArrayDistribution(distribs(i)),
            dargs(i),
            psizes(i))
        }
        DarrayCombiner(
          dts(0),
          size(0),
          rank(0),
          dims,
          mpi2.ArrayOrder(order(0)))
      }

    private def createResized(dt: mpi2.lib.MPI_Datatype) =
      getContents(dt, 0, 2, 1) { (ints, addrs, dts) =>
        val lowerBound = addrs
        val range = addrs.next(1)
        val extent = mpi2.Extent(lowerBound(0), range(0))
        ResizedCombiner(dts(0), extent)
      }
  }
}
