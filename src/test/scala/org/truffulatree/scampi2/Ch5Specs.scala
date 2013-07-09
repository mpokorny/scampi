//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.util.Random
import java.lang.System

class Ch5Spec extends ScampiSpecification {
  "ScaMPI" should {
    "pass example 5.1" in {
      runTest("Ex5_1", 4) must beTrue
    }
    "pass example 5.3" in {
      runTest("Ex5_3", 4) must beTrue
    }
    "pass example 5.4" in {
      runTest("Ex5_4", 4) must beTrue
    }
    "pass example 5.5" in {
      runTest("Ex5_5", 4) must beTrue
    }
    "pass example 5.6" in {
      runTest("Ex5_6", 4) must beTrue
    }
    "pass example 5.7" in {
      runTest("Ex5_7", 4) must beTrue
    }
    "pass example 5.8" in {
      runTest("Ex5_8", 4) must beTrue
    }
    "pass example 5.9" in {
      runTest("Ex5_9", 4) must beTrue
    }
    "pass example 5.10" in {
      runTest("Ex5_10", 4) must beTrue
    }
    "pass example 5.11" in {
      runTest("Ex5_11", 4) must beTrue
    }
    "pass example 5.12" in {
      runTest("Ex5_12", 4) must beTrue
    }
    "pass example 5.13" in {
      runTest("Ex5_13", 4) must beTrue
    }
    "pass example 5.14" in {
      runTest("Ex5_14", 4) must beTrue
    }
    "pass example 5.15" in {
      runTest("Ex5_15", 4) must beTrue
    }
    "pass example 5.16" in {
      runTest("Ex5_16", 4) must beTrue
    }
    "pass example 5.17" in {
      runTest("Ex5_17", 4) must beTrue
    }
    "pass example 5.19" in {
      runTest("Ex5_19", 4) must beTrue
    }
    "pass example 5.21" in {
      runTest("Ex5_21", 4) must beTrue
    }
  }
}

object Ex5_1 extends ScampiApp {
  import mpi2._
  val array = MpiInt.alloc(100)
  Comm.world.rank match {
    case 0 => {
      for (i <- 0 until array.length) array(i) = i + 1
      Comm.world.bcast(array, 0)
      result = true
    }
    case _ => {
      Comm.world.bcast(array, 0)
      result = true
      for (i <- 0 until array.length)
        result = result && (array(i) == i + 1)
    }
  }
}

object Ex5_3 extends ScampiApp {
  import mpi2._
  val sendSize = 100
  Comm.world.rank match {
    case 0 => {
      val array = MpiInt.alloc(sendSize * Comm.world.size)
      val a0 = (MpiInt * sendSize) @: array
      for (i <- 0 until sendSize) a0(i) = i
      Comm.world.gather(ValueInPlace(array), array, 0)
      result = true
      for (i <- 0 until array.length)
        result = result && (array(i) == i)
    }
    case _ => {
      val array = MpiInt.alloc(sendSize)
      for (i <- 0 until sendSize) array(i) = i + Comm.world.rank * sendSize
      Comm.world.gather(array, EmptyBuffer, 0)
      result = true
    }
  }
}

object Ex5_4 extends ScampiApp {
  import mpi2._
  val sendSize = 100
  val sendarray = MpiInt.alloc(sendSize)
  for (i <- 0 until sendSize)
    sendarray(i) = i + Comm.world.rank * sendSize
  val rtype = new ContiguousDatatype(MpiInt, sendSize)
  rtype.commit()
  val rbuf = rtype.alloc(Comm.world.size)
  Comm.world.gather(sendarray, rbuf, 0)
  result = true
  if (Comm.world.rank == 0) {
    for (i <- 0 until Comm.world.size) {
      val a = (rtype * 1) @:(rbuf + i)
      for (j <- 0 until a.length)
        result = result && a(j) == j + i * sendSize
    }
  }
}

object Ex5_5 extends ScampiApp {
  import mpi2._
  val sendSize = 100
  val stride = 115
  val sendarray = MpiInt.alloc(sendSize)
  for (i <- 0 until sendSize)
    sendarray(i) = i + Comm.world.rank * sendSize
  val rbuf = MpiInt.alloc(Comm.world.size * stride)
  for (i <- 0 until rbuf.length) rbuf(i) = -1
  Comm.world.gatherv(
    sendarray, rbuf,
    (0 until Comm.world.size).map(i => Block(sendSize, i * stride)), 0
  )
  result = true
  if (Comm.world.rank == 0)
    for (i <- 0 until rbuf.length) {
      val r = i / stride
      val j = i % stride
      result = result && (
        rbuf(i) == (if (j < sendSize) j + r * sendSize else -1))
    }
}

object Ex5_6 extends ScampiApp {
  import mpi2._
  val numRows = 100
  val numCols = 150
  val stride = 115
  val sendarray = MpiInt.alloc(numRows * numCols)
  for (i <- 0 until sendarray.length)
    sendarray(i) =
      if (i % numCols == 0) i / numCols + Comm.world.rank * numRows else -1
  val rbuf = MpiInt.alloc(Comm.world.size * stride)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val stype = new VectorDatatype(MpiInt, numRows, 1, numCols)
  stype.commit()
  Comm.world.gatherv(
    (stype * 1) @: sendarray, rbuf,
    (0 until Comm.world.size).map(i => Block(numRows, i * stride)),
    0)
  result = true
  if (Comm.world.rank == 0)
    for (i <- 0 until rbuf.length) {
      val r = i / stride
      val j = i % stride
      result = result && (
        rbuf(i) == (if (j < numRows) j + r * numRows else -2)
      )
    }
}

object Ex5_7 extends ScampiApp {
  import mpi2._
  val numRows = 100
  val numCols = 150
  val stride = 115
  val sendarray = MpiInt.alloc(numRows * numCols)
  for (i <- 0 until sendarray.length)
    sendarray(i) =
      if (i % numCols == Comm.world.rank &&
          i / numCols < numRows - Comm.world.rank)
        i / numCols + Comm.world.rank * numRows
      else -1
  val rbuf = MpiInt.alloc(Comm.world.size * stride)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val stype =
    new VectorDatatype(MpiInt, numRows - Comm.world.rank, 1, numCols)
  stype.commit()
  Comm.world.gatherv(
    (stype * 1) @: (sendarray + Comm.world.rank),
    rbuf,
    (0 until Comm.world.size).map(i => Block(numRows - i, i * stride)), 0)
  result = true
  if (Comm.world.rank == 0)
    for (i <- 0 until rbuf.length) {
      val r = i / stride
      val j = i % stride
      result = result && (
        rbuf(i) == (if (j < numRows - r) j + r * numRows else -2))
    }
}

object Ex5_8 extends ScampiApp {
  import mpi2._
  val numRows = 100
  val numCols = 150
  val stride = 115
  val sendarray = MpiInt.alloc(numRows * numCols)
  for (i <- 0 until sendarray.length)
    sendarray(i) =
      if (i % numCols == Comm.world.rank &&
          i / numCols < numRows - Comm.world.rank)
        i / numCols + Comm.world.rank * numRows
      else -1
  val rbuf = MpiInt.alloc(Comm.world.size * stride)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val stype = new ResizedDatatype(
    MpiInt,
    Extent(MpiInt.extent.lowerBound, MpiInt.extent.range * numCols))
  stype.commit()
  Comm.world.gatherv(
    (stype * (numRows - Comm.world.rank)) @: (sendarray + Comm.world.rank),
    rbuf,
    (0 until Comm.world.size).map(i => Block(numRows - i, i * stride)), 0)
  result = true
  if (Comm.world.rank == 0)
    for (i <- 0 until rbuf.length) {
      val r = i / stride
      val j = i % stride
      result = result && (
        rbuf(i) == (if (j < numRows - r) j + r * numRows else -2))
    }
}

object Ex5_9 extends ScampiApp {
  import mpi2._
  val numRows = 100
  val numCols = 150
  val gap = 6
  val sendarray = MpiInt.alloc(numRows * numCols)
  for (i <- 0 until sendarray.length)
    sendarray(i) =
      if (i % numCols == Comm.world.rank &&
          i / numCols < numRows - Comm.world.rank)
        i / numCols + Comm.world.rank * numRows
      else -1
  val rbuf = MpiInt.alloc(
    (Comm.world.size * (2 * numRows + 1 - Comm.world.size)) / 2 +
    Comm.world.size * gap)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val stype =
    new VectorDatatype(MpiInt, numRows - Comm.world.rank, 1, numCols)
  stype.commit()
  Comm.world.gatherv(
    (stype * 1) @: (sendarray + Comm.world.rank),
    rbuf,
    (0 until Comm.world.size).map(
      i => Block(numRows - i, (i * (2 * (numRows + gap) - (i - 1))) / 2)),
    0)
  result = true
  if (Comm.world.rank == 0) {
    var r = 0
    var j = 0
    for (i <- 0 until rbuf.length) {
      result = (
        result && (
          rbuf(i) == (if (j < numRows - r) j + r * numRows else -2)))
      j += 1
      if (j == numRows + gap - r) {
        r += 1
        j = 0
      }
    }
  }
}

object Ex5_10 extends ScampiApp {
  import mpi2._
  val numRows = 100
  val numCols = 150
  val sendarray = MpiInt.alloc(numRows * numCols)
  val numVals = (new Random(Comm.world.rank + System.currentTimeMillis)).
                nextInt(numRows) + 1
  for (i <- 0 until sendarray.length)
    sendarray(i) =
      if (i % numCols == Comm.world.rank && i / numCols < numVals)
        i / numCols + Comm.world.rank * numRows
      else -1
  val num = MpiInt.alloc(1)
  num(0) = numVals
  val rcounts = MpiInt.alloc(Comm.world.size)
  Comm.world.gather(num, rcounts, 0)
  val (total, revDispls) =
    ((0, List.empty[Int]) /: rcounts) {
      case ((st, ds), count) => (st + count, st :: ds)
    }
  val rbuf = MpiInt.alloc(total + 1)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val stype = new ResizedDatatype(
    MpiInt,
    Extent(MpiInt.extent.lowerBound, MpiInt.extent.range * numCols))
  stype.commit()
  Comm.world.gatherv(
    (stype * numVals) @: (sendarray + Comm.world.rank),
    rbuf,
    rcounts.zip(revDispls.reverse).map(Block.tupled),
    0)
  result = true
  if (Comm.world.rank == 0) {
    var r = 0
    var j = 0
    for (i <- 0 until total) {
      result = (
        result && (
          rbuf(i) == (if (j < rcounts(r)) j + r * numRows else -2)))
      j += 1
      if (j == rcounts(r)) {
        r += 1
        j = 0
      }
    }
  }
}

object Ex5_11 extends ScampiApp {
  import mpi2._
  val sendSize = 100
  val rbuf = MpiInt.alloc(sendSize)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val sendbuf = MpiInt.alloc(Comm.world.size * sendSize)
  for (i <- 0 until Comm.world.size)
    for (j <- 0 until sendSize)
      sendbuf(i * sendSize + j) = i * sendSize + j
  Comm.world.scatter(sendbuf, sendSize, rbuf, 0)
  result = true
  for (i <- 0 until sendSize)
    result = result && (rbuf(i) == Comm.world.rank * sendSize + i)
}

object Ex5_12 extends ScampiApp {
  import mpi2._
  val sendSize = 100
  val stride = 117
  val rbuf = MpiInt.alloc(sendSize)
  for (i <- 0 until rbuf.length) rbuf(i) = -2
  val sendbuf = MpiInt.alloc(Comm.world.size * stride)
  for (i <- 0 until Comm.world.size) {
    for (j <- 0 until sendSize)
      sendbuf(i * stride + j) = i * stride + j
    for (j <- sendSize until stride)
      sendbuf(i * stride + j) = -1
  }
  Comm.world.scatterv(
    sendbuf, (0 until Comm.world.size).map(i => Block(sendSize, i * stride)),
    rbuf,
    0)
  result = true
  for (i <- 0 until sendSize)
    result = result && (rbuf(i) == Comm.world.rank * stride + i)
}

object Ex5_13 extends ScampiApp {
  import mpi2._
  val numRows = 100
  val numCols = 150
  val gap = 9
  val recvarray = MpiInt.alloc(numRows * numCols)
  for (i <- 0 until recvarray.length) recvarray(i) = -2
  val sendbuf = MpiInt.alloc(
    (Comm.world.size * (2 * numRows + 1 - Comm.world.size)) / 2 +
    Comm.world.size * gap)
  val sendBlocks = (0 until Comm.world.size).map(
    i => Block(numRows - i, (i * (2 * (numRows + gap) - (i - 1))) / 2)
  )
  sendBlocks.zipWithIndex.foreach {
    case (Block(len, disp), i) => {
      for (j <- 0 until len)
        sendbuf(disp + j) = i * numRows + j
      for (j <- len until len + gap)
        sendbuf(disp + j) = -1
    }
  }
  val stype =
    new VectorDatatype(MpiInt, numRows - Comm.world.rank, 1, numCols)
  stype.commit()
  Comm.world.scatterv(
    sendbuf, sendBlocks,
    (stype * 1) @: (recvarray + Comm.world.rank),
    0)
  result = true
  for (i <- 0 until recvarray.length)
    result == result && (
      recvarray(i) == (
        if (i % numCols == Comm.world.rank)
          i / numCols + Comm.world.rank * numRows
        else -2))
}

object Ex5_14 extends ScampiApp {
  import mpi2._
  val sendSize = 100
  val sendarray = MpiInt.alloc(sendSize)
  for (i <- 0 until sendarray.length)
    sendarray(i) = i + Comm.world.rank * sendSize
  val rbuf = MpiInt.alloc(Comm.world.size * sendSize)
  for (i <- 0 until rbuf.length)
    rbuf(i) = -1
  Comm.world.allgather(sendarray, rbuf)
  result = true
  for (i <- 0 until rbuf.length)
    result = result && (rbuf(i) == i)
}

object Ex5_15 extends ScampiApp {
  import mpi2._
  val sliceSize = 1000
  val a = MpiFloat.alloc(sliceSize)
  val b = MpiFloat.alloc(sliceSize)
  for (i <- 0 until sliceSize) a(i) =
    (i + (Comm.world.rank + 1) * sliceSize).toFloat
  for (i <- 0 until sliceSize) b(i) = 1.0f / a(i)
  val localSum = MpiFloat.alloc(1)
  localSum(0) = (0.0f /: a.zip(b)) { case (acc, (p1, p2)) => acc + p1 * p2 }
  val sum = MpiFloat.alloc(1)
  Comm.world.reduce(localSum, sum, Op.sum, 0)
  result = (
    (Comm.world.rank != 0) ||
    (sum(0) == (Comm.world.size * sliceSize).toFloat))
}

object Ex5_16 extends ScampiApp {
  import mpi2._
  val sliceSize = 1000
  val numCols = 500
  val rng = new Random(Comm.world.rank)
  val a = MpiFloat.alloc(sliceSize)
  for (i <- 0 until sliceSize) a(i) = rng.nextFloat
  val b = MpiFloat.alloc(sliceSize * numCols)
  for (i <- 0 until b.length) b(i) = rng.nextFloat
  val sum = MpiFloat.alloc(numCols)
  for (j <- 0 until numCols)
    sum(j) = (0.0f /: (0 until sliceSize)) {
      case (acc, i) => a(i) * b(i * numCols + j) + acc
    }
  val c = MpiFloat.alloc(numCols)
  Comm.world.reduce(sum, c, Op.sum, 0)
  result = true
  if (Comm.world.rank == 0) {
    val rngs = (0 until Comm.world.size).map(new Random(_))
    val aa = (0 until Comm.world.size).flatMap(r => {
      (0 until sliceSize).map(_ => rngs(r).nextFloat)
    })
    val bb = (0 until Comm.world.size).flatMap(r => {
      (0 until numCols * sliceSize).map(_ => rngs(r).nextFloat)
    })
    val cc = (0 until numCols).map(c => {
      (0.0f /: (0 until sliceSize * Comm.world.size)) {
        case (acc, j) => aa(j) * bb(j * numCols + c) + acc
      }
    })
    for (i <- 0 until c.length)
      result = cc.zip(c).forall(p => ((p._1 - p._2) / p._1).abs <= 1.0e-5f)
  }
}

object Ex5_17 extends ScampiApp {
  import mpi2._
  val in = MpiDoubleInt.alloc(30)
  val out = MpiDoubleInt.alloc(30)
  val rng = new Random(Comm.world.rank)
  for (i <- 0 until in.length)
    in(i) = (rng.nextDouble, Comm.world.rank)
  Comm.world.reduce(in, out, Op.maxLoc, 0)
  result = true
  if (Comm.world.rank == 0) {
    val rngs = (0 until Comm.world.size).map(new Random(_))
    for (i <- 0 until out.length) {
      out(i) match {
        case (v, r) =>
          result = result && {
            val ds = rngs.map(_.nextDouble)
            ds(r) == v && ds.forall(_ <= v)
          }
      }
    }
  }
}

object Ex5_19 extends ScampiApp {
  import mpi2._
  val len = 1000
  val vals = MpiFloat.alloc(len)
  val rng = new Random(Comm.world.rank)
  for (i <- 0 until len) vals(i) = rng.nextFloat
  val in = MpiFloatInt.alloc(1)
  in(0) = ((Float.MaxValue, -1) /: vals.zipWithIndex) {
    case ((minVal, minValIndex), (v, i)) => {
      if (v < minVal) (v, i)
      else (minVal, minValIndex)
    }
  }
  in(0) = (in(0)._1, in(0)._2 + Comm.world.rank * len)
  val out = MpiFloatInt.alloc(1)
  Comm.world.reduce(in, out, Op.minLoc, 0)
  result = true
  if (Comm.world.rank == 0) {
    val min = (0 until Comm.world.size).flatMap(r => {
      val rng = new Random(r)
      (0 until 1000).map(i => (rng.nextFloat, r, i))
    }).min
    result = ((min._1 == out(0)._1) && (min._2 == out(0)._2 / len) &&
                 (min._3 == out(0)._2 % len))
  }
}

object Ex5_21 extends ScampiApp {
  import mpi2._
  val sliceSize = 1000
  val numCols = 500
  val rng = new Random(Comm.world.rank)
  val a = MpiFloat.alloc(sliceSize)
  for (i <- 0 until sliceSize) a(i) = rng.nextFloat
  val b = MpiFloat.alloc(sliceSize * numCols)
  for (i <- 0 until b.length) b(i) = rng.nextFloat
  val sum = MpiFloat.alloc(numCols)
  for (j <- 0 until numCols)
    sum(j) = (0.0f /: (0 until sliceSize)) {
      case (acc, i) => a(i) * b(i * numCols + j) + acc
    }
  val c = MpiFloat.alloc(numCols)
  Comm.world.allreduce(sum, c, Op.sum)
  val rngs = (0 until Comm.world.size).map(new Random(_))
  val aa = (0 until Comm.world.size).flatMap(r => {
    (0 until sliceSize).map(_ => rngs(r).nextFloat)
  })
  val bb = (0 until Comm.world.size).flatMap(r => {
    (0 until numCols * sliceSize).map(_ => rngs(r).nextFloat)
  })
  val cc = (0 until numCols).map(c => {
    (0.0f /: (0 until sliceSize * Comm.world.size)) {
      case (acc, j) => aa(j) * bb(j * numCols + c) + acc
    }
  })
  for (i <- 0 until c.length)
    result = cc.zip(c).forall(p => ((p._1 - p._2) / p._1).abs <= 1.0e-5f)
}
