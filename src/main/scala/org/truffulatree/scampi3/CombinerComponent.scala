//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import org.bridj.Pointer

trait CombinerComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  sealed abstract class Combiner

  case object NamedCombiner extends Combiner

  case class DupCombiner(datatype: mpi3.lib.MPI_Datatype) extends Combiner

  case class ContiguousCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    count: Int)
      extends Combiner

  case class VectorCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    count: Int,
    blocklength: Int,
    stride: Int)
      extends Combiner

  case class HvectorCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    count: Int,
    blocklength: Int,
    stride: mpi3.lib.MPI_Aint)
      extends Combiner

  case class IndexedCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    blocks: Seq[mpi3.Block])
      extends Combiner

  case class HindexedCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    blocks: Seq[mpi3.HBlock])
      extends Combiner

  case class IndexedBlockCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    blocklength: Int,
    displacements: Seq[Int])
      extends Combiner

  case class HindexedBlockCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    blocklength: Int,
    displacements: Seq[MPI_Aint])
      extends Combiner

  case class StructCombiner(blocks: Seq[mpi3.TypeBlock]) extends Combiner

  case class SubarrayCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    dims: Seq[SubarrayDim],
    order: mpi3.ArrayOrder.ArrayOrder)
      extends Combiner

  case class DarrayCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    size: Int,
    rank: Int,
    dims: Seq[DarrayDim],
    order: mpi3.ArrayOrder.ArrayOrder)
      extends Combiner

  case class ResizedCombiner(
    datatype: mpi3.lib.MPI_Datatype,
    extent: mpi3.Extent)
      extends Combiner

  object Combiner {
    def apply(dt: mpi3.lib.MPI_Datatype): Combiner = {
      val args = Pointer.allocateInts(4).as(classOf[Int])
      try {
        val numIntegers = args
        val numAddresses = args.next(1)
        val numDatatypes = args.next(2)
        val combiner = args.next(3)
        mpi3.mpiCall(
          mpi3.lib.MPI_Type_get_envelope(
            dt,
            numIntegers,
            numAddresses,
            numDatatypes,
            combiner))
        val result = mpi3.CombinerType(combiner(0)) match {
          case mpi3.CombinerType.Named =>
            NamedCombiner
          case mpi3.CombinerType.Dup =>
            createDup(dt)
          case mpi3.CombinerType.Contiguous =>
            createContiguous(dt)
          case mpi3.CombinerType.Vector =>
            createVector(dt)
          case mpi3.CombinerType.Hvector =>
            createHvector(dt)
          case mpi3.CombinerType.Indexed =>
            createIndexed(dt, numIntegers(0))
          case mpi3.CombinerType.Hindexed =>
            createHindexed(dt, numIntegers(0), numAddresses(0))
          case mpi3.CombinerType.IndexedBlock =>
            createIndexedBlock(dt, numIntegers(0))
          case mpi3.CombinerType.HindexedBlock =>
            createHindexedBlock(dt, numAddresses(0))
          case mpi3.CombinerType.Struct =>
            createStruct(dt, numIntegers(0), numAddresses(0), numDatatypes(0))
          case mpi3.CombinerType.Subarray =>
            createSubarray(dt, numIntegers(0))
          case mpi3.CombinerType.Darray =>
            createDarray(dt, numIntegers(0))
          case mpi3.CombinerType.Resized =>
            createResized(dt)
        }
        result
      } finally args.release()
    }

    private def getContents[A](
      dt: mpi3.lib.MPI_Datatype,
      numIntegers: Int,
      numAddresses: Int,
      numDatatypes: Int)(
      f: (
        Pointer[Int],
        Pointer[mpi3.lib.MPI_Aint],
        Pointer[mpi3.lib.MPI_Datatype]) => A) = {
      val integers = Pointer.allocateInts(numIntegers).as(classOf[Int])
      val addresses = mpi3.allocateAint(numAddresses)
      val datatypes = mpi3.allocateDatatype(numDatatypes)
      mpi3.mpiCall(
        mpi3.lib.MPI_Type_get_contents(
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

    private def createDup(dt: mpi3.lib.MPI_Datatype) =
      getContents(dt, 0, 0, 1) { (ints, addrs, dts) =>
        DupCombiner(dts(0))
      }

    private def createContiguous(dt: mpi3.lib.MPI_Datatype) =
      getContents(dt, 1, 0, 1) { (ints, addrs, dts) =>
        ContiguousCombiner(dts(0), ints(0))
      }

    private def createVector(dt: mpi3.lib.MPI_Datatype) =
      getContents(dt, 3, 0, 1) { (ints, addrs, dts) =>
        val count = ints
        val blocklength = ints.next(1)
        val stride = ints.next(2)
        VectorCombiner(dts(0), count(0), blocklength(0), stride(0))
      }

    private def createHvector(dt: mpi3.lib.MPI_Datatype) =
      getContents(dt, 2, 1, 1) { (ints, addrs, dts) =>
        val count = ints
        val blocklength = ints.next(1)
        HvectorCombiner(dts(0), count(0), blocklength(0), addrs(0))
      }

    private def createIndexed(dt: mpi3.lib.MPI_Datatype, numIntegers: Int) =
      getContents(dt, numIntegers, 0, 1) { (ints, addrs, dts) =>
        val count = ints
        val lengths = ints.next(1)
        val displacements = ints.next(count(0) + 1)
        val blocks = (0 until count(0)) map { i =>
          mpi3.Block(lengths(i), displacements(i))
        }
        IndexedCombiner(dts(0), blocks)
      }

    private def createHindexed(
      dt: mpi3.lib.MPI_Datatype, numIntegers: Int, numAddresses: Int) =
      getContents(dt, numIntegers, numAddresses, 1) { (ints, addrs, dts) =>
        val count = ints
        val lengths = ints.next(1)
        val displacements = addrs
        val hblocks = (0 until count(0)) map { i =>
          mpi3.HBlock(lengths(i), displacements(i))
        }
        HindexedCombiner(dts(0), hblocks)
      }

    private def createIndexedBlock(dt: mpi3.lib.MPI_Datatype, numIntegers: Int) =
      getContents(dt, numIntegers, 0, 1) { (ints, addrs, dts) =>
        val count = ints
        val length = ints.next(1)
        val displacements = ints.next(2)
        IndexedBlockCombiner(
          dts(0),
          length(0),
          displacements.getInts(count(0)))
      }

    private def createHindexedBlock(dt: mpi3.lib.MPI_Datatype, numAddresses: Int) =
      getContents(dt, 2, numAddresses, 1) { (ints, addrs, dts) =>
        val count = ints
        val length = ints.next(1)
        HindexedBlockCombiner(dts(0), length(0), addrs.toArray)
      }

    private def createStruct(
      dt: mpi3.lib.MPI_Datatype,
      numIntegers: Int,
      numAddresses: Int,
      numDatatypes: Int) =
      getContents(dt, numIntegers, numAddresses, numDatatypes) { (ints, addrs, dts) =>
        val count = ints
        val lengths = ints.next(1)
        val displacements = addrs
        val types = dts
        val blocks = (0 until count(0)) map { i =>
          mpi3.TypeBlock(
            lengths(i),
            displacements(i),
            Datatype.lookup(types(i)).asInstanceOf[mpi3.SeqDatatype[_]])
        }
        StructCombiner(blocks)
      }

    private def createSubarray(dt: mpi3.lib.MPI_Datatype, numIntegers : Int) =
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

    private def createDarray(dt: mpi3.lib.MPI_Datatype, numIntegers: Int) =
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
          mpi3.ArrayOrder(order(0)))
      }

    private def createResized(dt: mpi3.lib.MPI_Datatype) =
      getContents(dt, 0, 2, 1) { (ints, addrs, dts) =>
        val lowerBound = addrs
        val range = addrs.next(1)
        val extent = mpi3.Extent(lowerBound(0), range(0))
        ResizedCombiner(dts(0), extent)
      }
  }
}
