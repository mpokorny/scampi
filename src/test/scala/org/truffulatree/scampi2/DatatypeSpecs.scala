//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class DatatypeSpec extends ScampiSpecification {
  "datatypes" should {
    "support a vector with negative stride as a vector datatype" in {
      runTest("NegVect", 2) must beTrue
    }
    "support a sequence of vectors with negative strides" in {
      runTest("SeqNegVect", 2) must beTrue
    }
  }
}

object NegVect extends ScampiApp {
  import mpi2._
  val numBlocks = 3
  val blockLength = 4
  val stride = -8
  def vValue(block: Int, offset: Int): Int = block * blockLength + offset + 1
  Comm.world.rank match {
    case 0 => {
      val vs = MpiInt((
        for (i <- 0 until numBlocks; j <- 0 until blockLength)
        yield vValue(i, j)):_*)
      Comm.world.send(vs, 1, 0)
      result = true
    }
    case 1 => {
      val vdt = new VectorDatatype(MpiInt, numBlocks, blockLength, stride)
      vdt.commit()
      val vs = vdt.alloc(1)
      Comm.world.doRecv(vs, 0, 0)
      result = true
      var idx = 0
      for (i <- 0 until numBlocks)
        for (j <- 0 until blockLength) {
          result = result && vs(idx) == vValue(i, j)
          idx += 1
        }
      val is = MpiInt @: vs
      if (result) {
        var expected = (numBlocks - 1) * blockLength + 1
        for (i <- 0 until ((numBlocks - 1) * stride.abs + blockLength)) {
          val j = i % stride.abs
          if (j < blockLength) {
            result = result && is(i) == expected
            expected += 1
          } else {
            result = result && is(i) == 0
            if (j == stride.abs - 1) expected -= 2 * blockLength
          }
        }
      }
    }
    case _ => {
      result = false
    }
  }
}

object SeqNegVect extends ScampiApp {
  import mpi2._
  val numVectors = 2
  val numBlocks = 3
  val blockLength = 4
  val stride = -8
  val vdt = new VectorDatatype(MpiInt, numBlocks, blockLength, stride)
  vdt.commit()
  val vs = vdt.alloc(numVectors)
  def vValue(vector: Int, block: Int, offset: Int): Int =
    (vector + 1) * 100 + block * blockLength + offset + 1
  Comm.world.rank match {
    case 0 => {
      vs.copy(
        for (n <- 0 until numVectors;
          i <- 0 until numBlocks;
          j <- 0 until blockLength)
        yield vValue(n, i, j))
      Comm.world.send(vs, 1, 0)
      result = true
    }
    case 1 => {
      Comm.world.doRecv(vs, 0, 0)
      result = true
      var idx = 0
      for (n <- 0 until numVectors;
        i <- 0 until numBlocks;
        j <- 0 until blockLength) {
        result = result && vs(idx) == vValue(n, i, j)
        idx += 1
      }
    }
    case _ => {
      result = false
    }
  }
}
