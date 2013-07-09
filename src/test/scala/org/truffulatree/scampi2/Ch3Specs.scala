//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.collection.mutable.ArraySeq
import org.bridj.Pointer

class Ch3Spec extends ScampiSpecification {
  "ScaMPI" should {
    "pass example 3.1" in {
      runTest("Ex3_1", 2) must beTrue
    }
    "pass example 3.2" in {
      runTest("Ex3_2", 2) must beTrue
    }
    "pass example 3.6" in {
      runTest("Ex3_6", 2) must beTrue
    }
    "pass example 3.7" in {
      runTest("Ex3_7", 2) must beTrue
    }
    "pass example 3.8" in {
      runTest("Ex3_8", 2) must beTrue
    }
    "pass example 3.10 with standard sends" in {
      runTest("Ex3_10", 2) must beTrue
    }
    "pass example 3.13" in {
      runTest("Ex3_13", 2) must beTrue
    }
    "pass example 3.14" in {
      runTest("Ex3_14", 2) must beTrue
    }
    "pass example 3.15" in {
      runTest("Ex3_15", 2) must beTrue
    }
    "pass example 3.17" in {
      runTest("Ex3_17", 5) must beTrue
    }
    "pass example 3.18" in {
      runTest("Ex3_18", 3) must beTrue
    }
  }
}

object Ex3_1 extends ScampiApp {
  import mpi2._
  val message = "Hello, there"
  val messageBuf = MpiChar.alloc(20)
  Comm.world.rank match {
    case 0 => {
      messageBuf.pointer.setCString(message)
      Comm.world.send(messageBuf, 1, 99)
      result = true
    }
    case 1 => {
      Comm.world.doRecv(messageBuf, 0, 99)
      result = messageBuf.pointer.getCString == message
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_2 extends ScampiApp {
  import mpi2._
  val tag = 42
  Comm.world.rank match {
    case 0 => {
      val a = MpiFloat.alloc(10)
      Comm.world.send(a, 1, tag)
      result = true
    }
    case 1 => {
      val b = MpiFloat.alloc(15)
      Comm.world.doRecv(b, 0, tag)
      result = true
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_6 extends ScampiApp {
  import mpi2._
  val tag = 88
  val buf1 = MpiFloat.alloc(1)
  val buf2 = MpiFloat.alloc(1)
  Comm.world.rank match {
    case 0 => {
      attachNewBuffer(400)
      buf1(0) = 1.0f
      buf2(0) = 2.0f
      Comm.world.bsend(buf1, 1, tag)
      Comm.world.bsend(buf2, 1, tag)
      result = true
    }
    case 1 => {
      Comm.world.doRecv(buf1, 0, anyTag)
      Comm.world.doRecv(buf2, 0, tag)
      result = (buf1(0) == 1.0f && buf2(0) == 2.0f)
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_7 extends ScampiApp {
  import mpi2._
  val tag1 = 1
  val tag2 = 2
  val buf1 = MpiFloat.alloc(1)
  val buf2 = MpiFloat.alloc(1)
  Comm.world.rank match {
    case 0 => {
      attachNewBuffer(400)
      buf1(0) = 1.0f
      buf2(0) = 2.0f
      Comm.world.bsend(buf1, 1, tag1)
      Comm.world.ssend(buf2, 1, tag2)
      result = true
    }
    case 1 => {
      Comm.world.doRecv(buf1, 0, tag2)
      Comm.world.doRecv(buf2, 0, tag1)
      result = (buf1(0) == 2.0f && buf2(0) == 1.0f)
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_8 extends ScampiApp {
  import mpi2._
  val tag = 60
  val buf = MpiFloat.alloc(1)
  Comm.world.rank match {
    case 0 => {
      buf(0) = 1.0f
      Comm.world.ssend(buf, 1, tag)
      Comm.world.doRecv(buf, 1, tag)
      result = (buf(0) == 2.0f)
    }
    case 1 => {
      Comm.world.doRecv(buf, 0, tag)
      result = (buf(0) == 1.0f)
      buf(0) = 2.0f
      Comm.world.ssend(buf, 0, tag)
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_10 extends ScampiApp {
  import mpi2._
  val tag = 63
  val sendbuf = MpiFloat.alloc(1)
  val recvbuf = MpiFloat.alloc(1)
  Comm.world.rank match {
    case 0 => {
      sendbuf(0) = 1.0f
      Comm.world.send(sendbuf, 1, tag)
      Comm.world.doRecv(recvbuf, 1, tag)
      result = (recvbuf(0) == 2.0f)
    }
    case 1 => {
      sendbuf(0) = 2.0f
      Comm.world.send(sendbuf, 0, tag)
      Comm.world.doRecv(recvbuf, 0, tag)
      result = (recvbuf(0) == 1.0f)
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_13 extends ScampiApp {
  import mpi2._
  val tag = 0
  val n = 10
  val outval = MpiFloat.alloc(1)
  val inval = MpiFloat.alloc(1)
  def bufval(r: Int, i: Int): Float = r * 100.0f + i * 1.0f
  def myval(i: Int) = bufval(Comm.world.rank, i)
  def otherval(i: Int) = bufval(1 - Comm.world.rank, i)
  Comm.world.rank match {
    case 0 => {
      result = true
      for (i <- 0 until n) {
        outval(0) = myval(i)
        val req1 = Comm.world.isend(outval, 1, tag)
        req1.free()
        val req2 = Comm.world.irecv(inval, 1, tag)
        req2.doAwait()
        result = result && inval(0) == otherval(i)
      }
    }
    case 1 => {
      val req1 = Comm.world.irecv(inval, 0, tag)
      req1.doAwait()
      result = inval(0) == otherval(0)
      for (i <- 1 until n) {
        outval(0) = myval(i - 1)
        val req2 = Comm.world.isend(outval, 0, tag)
        req2.free()
        val req3 = Comm.world.irecv(inval, 0, tag)
        req3.doAwait()
        result = result && inval(0) == otherval(i)
      }
      outval(0) = myval(n - 1)
      val req4 = Comm.world.isend(outval, 0, tag)
      req4.doAwait()
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_14 extends ScampiApp {
  import mpi2._
  val a = MpiFloat.alloc(1)
  val b = MpiFloat.alloc(1)
  val tagA = 2
  val tagB = 3
  Comm.world.rank match {
    case 0 => {
      a(0) = 1.0f
      b(0) = 2.0f
      Comm.world.isend(a, 1, tagA)
      Comm.world.isend(b, 1, tagB)
      result = true
    }
    case 1 => {
      val reqA = Comm.world.irecv(a, 0, anyTag)
      val reqB = Comm.world.irecv(b, 0, tagB)
      reqA.doAwait()
      reqB.doAwait()
      result = a(0) == 1.0f && b(0) == 2.0f
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_15 extends ScampiApp {
  import mpi2._
  val a = MpiFloat.alloc(1)
  val b = MpiFloat.alloc(1)
  Comm.world.rank match {
    case 0 => {
      a(0) = 1.0f
      b(0) = 2.0f
      Comm.world.ssend(a, 1, 0)
      Comm.world.send(b, 1, 1)
      result = true
    }
    case 1 => {
      val r1 = Comm.world.irecv(a, 0, 0)
      Comm.world.doRecv(b, 0, 1)
      r1.doAwait()
      result = a(0) == 1.0f && b(0) == 2.0f
    }
    case _ => {
      result = false
    }
  }
}

object Ex3_17 extends ScampiApp {
  import mpi2._
  val tag = 7
  val numIterations = 10
  val vLength = 4
  def vOff(r: Int) = 100 * r
  if (Comm.world.rank > 0) {
    // client code
    val myOff = vOff(Comm.world.rank)
    val a = MpiInt.alloc(vLength)
    for (i <- 0 until numIterations) {
      (0 until vLength).map(j => (j, j + vLength * i + myOff)).foreach {
        case (j, v) => a(j) = v
      }
      val req = Comm.world.isend(a, 0, tag)
      req.doAwait()
    }
    result = true
  } else {
    // server code
    val f4 = new ContiguousDatatype(MpiInt, vLength)
    f4.commit()
    val onef4 = f4 * 1
    val a = f4.alloc(Comm.world.size - 1)
    val nextIt = new ArraySeq[Int](Comm.world.size)
    result = true
    var reqs =
      (1 until Comm.world.size).map(
        r => Comm.world.irecv(onef4 @: (a + (r - 1)), r, tag))
    while (reqs.length > 0) {
      Request.waitSome(reqs).foreach(_.foreach {
        case (req, status) => {
          reqs = reqs.filterNot(_ == req)
          val rank = status.source
          result = result && {
            val off = vLength * nextIt(rank) + vOff(rank)
            (true /: (onef4 @: (a + (rank - 1))).zipWithIndex) {
              case (acc, (v, i)) => {
                acc && v == i + off
              }
            }
          }
          nextIt(rank) += 1
          if (nextIt(rank) < numIterations)
            reqs = (reqs :+
              Comm.world.irecv(onef4 @: (a + (rank - 1)), rank, tag))
        }
      })
    }
  }
}

object Ex3_18 extends ScampiApp {
  import mpi2._
  val i = MpiInt.alloc(1)
  val x = MpiFloat.alloc(1)
  Comm.world.rank match {
    case 0 => {
      i(0) = 100
      Comm.world.send(i, 2, 0)
      result = true
    }
    case 1 => {
      x(0) = 111.0f
      Comm.world.send(x, 2, 0)
      result = true
    }
    case 2 => {
      result = true
      for (j <- 0 until 2) {
        Comm.world.probe(anySource, 0).source match {
          case 0 => {
            Comm.world.recv(i, 0, 0)
            result = result && i(0) == 100
          }
          case 1 => {
            Comm.world.recv(x, 1, 0)
            result = result && x(0) == 111.0f
          }
          case _ => {
            result = false
          }
        }
      }
    }
    case _ => {
    }
  }
}
