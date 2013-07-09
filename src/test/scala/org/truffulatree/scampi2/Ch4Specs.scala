//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.language.existentials
import scala.language.reflectiveCalls
import org.bridj.PointerIO
import org.bridj.Pointer
import scala.collection.mutable
import mutable.{Seq => MSeq}

class Ch4Spec extends ScampiSpecification {
  import mpi2._
  initThread(ThreadLevel.Multiple)

  // type PTMap = (Datatype[_], Long)

  // def offset(seq: SeqValueBuffer[_], idx: Int): Long =
  //   (seq + idx).offset

  // def typeOffsets(valueBuff: SeqValueBuffer[_]): Seq[PTMap] = {
  //   // FIXME!
  //   valueBuff.datatype match {
  //     case pd: PredefDatatype[_] => {
  //       valueBuff(0) match {
  //         case prod: Product =>
  //           p.productIterator.toSeq.flatMap {
  //             case vb: SeqValueBuffer[_] => typeOffsets(vb)
  //           }
  //         case _ =>
  //           (0 until valueBuff.valueCount).map(
  //             i => (pd,
  //               offset(valueBuff, i) + pd.extent.lowerBound))
  //       }
  //     }
  //     case sd: StructDatatype => {
  //       ???
  //     }
  //     case dd: DerivedDatatype[_] => {
  //       (0 until valueBuff.valueCount).flatMap(
  //         i => typeOffsets((dd * 1) @: (valueBuff + i)).map {
  //           case (d, off) => (d, off + offset(valueBuff, i))
  //         }
  //       )
  //     }
  //   }
  // }

  // def typeOffsets(valueBuff: ValueBuffer[_]): Seq[PTMap] = {
  //   valueBuff.datatype match {
  //     case pd: PredefDatatype[_] =>
  //       (0 until valueBuff.valueCount).map(
  //         i => (pd, absoluteOffset(valueBuff, i)))
  //     case _ => {
  //       valueBuff map {
  //         case
  //       }

  //       valueBuff flatMap ((v: Any) => v match {
  //         case s: ValueBuffer[_] =>
  //           typeOffsets(s)
  //         case p: Product =>
  //           p.productIterator.toSeq.flatMap(
  //             b => typeOffsets(b.asInstanceOf[ValueBuffer[_]]))
  //         case _ =>
  //           assume(false); Seq()
  //       })
  //     }
  //   }
  // }

  // def typeMap(datatype: SeqDatatype[_]): Seq[PTMap] =
  //   typeOffsets(datatype.alloc(1))

  // val double = MpiDouble
  // val char = MpiChar
  // val float = MpiFloat
  // val int = MpiInt

  "ScaMPI" should {
    // "pass example 4.1" in {
    //   val d = new StructDatatype2(
    //     StructBlock(double, 1, Some(0)),
    //     StructBlock(char, 1, Some(8)))
    //   try {
    //     d.extent.range.intValue must_== AlignHelper.align(
    //       9,
    //       PointerIO.getDoubleInstance.getTargetAlignment.toInt)
    //   } finally d.free()
    // }
    // "pass example 4.2" in {
    //   val d = new StructDatatype2(
    //     StructBlock(double, 1, Some(0)),
    //     StructBlock(char, 1, Some(8)))
    //   val c = new ContiguousDatatype(d, 3)
    //   try {
    //     typeMap(c).toList must_== List[PTMap](
    //       (double, 0), (char, 8), (double, 16), (char, 24),
    //       (double, 32), (char, 40)
    //     )
    //   } finally {
    //     c.free()
    //     d.free()
    //   }
    // }
    // "pass example 4.3" in {
    //   val d = new StructDatatype2(
    //     StructBlock(double, 1, Some(0)),
    //     StructBlock(char, 1, Some(8)))
    //   val v = new VectorDatatype(d, 2, 3, 4)
    //   try {
    //     typeMap(v).toList must_== List[PTMap](
    //       (double, 0),
    //       (char, 8),
    //       (double, 16),
    //       (char, 24),
    //       (double, 32),
    //       (char, 40),
    //       (double, 64),
    //       (char, 72),
    //       (double, 80),
    //       (char, 88),
    //       (double, 96),
    //       (char, 104))
    //   } finally {
    //     d.free()
    //     v.free()
    //   }
    // }
    // "pass example 4.4" in {
    //   val d = new StructDatatype2(
    //     StructBlock(double, 1, Some(0)),
    //     StructBlock(char, 1, Some(8)))
    //   val v = new VectorDatatype(d, 3, 1, -2)
    //   try {
    //     typeMap(v).toList must_== List[PTMap](
    //       (double, 0),
    //       (char, 8),
    //       (double, -32),
    //       (char, -24),
    //       (double, -64),
    //       (char, -56))
    //   } finally {
    //     d.free()
    //     v.free()
    //   }
    // }
    // "pass example 4.5" in {
    //   val d = new StructDatatype2(
    //     StructBlock(double, 1, Some(0)),
    //     StructBlock(char, 1, Some(8)))
    //   val i = new IndexedDatatype(d, List(Block(3, 4), Block(1, 0)))
    //   try {
    //     typeMap(i).toList must_== List[PTMap](
    //       (double, 64),
    //       (char, 72),
    //       (double, 80),
    //       (char, 88),
    //       (double, 96),
    //       (char, 104),
    //       (double, 0),
    //       (char, 8))
    //   }
    //   finally {
    //     i.free()
    //     d.free()
    //   }
    // }
    // "pass example 4.6" in {
    //   val d = new StructDatatype2(
    //     StructBlock(double, 1, Some(0)),
    //     StructBlock(char, 1, Some(8)))
    //   val s = new StructDatatype3(
    //     StructBlock(float, 2, Some(0)),
    //     StructBlock(d, 1, Some(16)),
    //     StructBlock(char, 3, Some(26)))
    //   try {
    //     typeMap(s).toList must_== List[PTMap](
    //       (float, 0),
    //       (float, 4),
    //       (double, 16),
    //       (char, 24),
    //       (char, 26),
    //       (char, 27),
    //       (char, 28))
    //   } finally {
    //     s.free()
    //     d.free()
    //   }
    // }
    // "pass example 4.8" in {
    //   val a = Pointer.allocateInts(100 * 100)
    //   try {
    //     val a_9_9 = a.next(100 * 9 + 9)
    //     getAddress(a_9_9) - getAddress(a) must_== 909 * int.extent.range
    //   } finally a.release()
    // }
    // "pass example 4.9" in {
    //   val i = new ResizedDatatype(MpiInt, Extent(-2, 8))
    //   val i2 = new ContiguousDatatype(i, 2)
    //   try {
    //     typeMap(i2).toList must_== List[PTMap]((int, 0), (int, 8))
    //   } finally {
    //     i.free()
    //     i2.free()
    //   }
    // }
    "pass example 4.12" in {
      runTest("Ex4_12", 2) must beTrue
    }
    "pass example 4.13" in {
      runTest("Ex4_13", 1) must beTrue
    }
    "pass example 4.14" in {
      runTest("Ex4_14", 1) must beTrue
    }
    "pass example 4.15" in {
      runTest("Ex4_15", 1) must beTrue
    }
    "pass example 4.16" in {
      runTest("Ex4_16", 1) must beTrue
    }
    "pass example 4.17" in {
      runTest("Ex4_17", 1) must beTrue
    }
    "pass example 4.18" in {
      runTest("Ex4_18", 1) must beTrue
    }
    // "pass example 4.19" in {
    //   runTest("Ex4_19", 1) must beTrue
    // }
    // "pass example 4.21" in {
    //   runTest("Ex4_21", 2) must beTrue
    // }
    // "pass example 4.22" in {
    //   runTest("Ex4_22", 2) must beTrue
    // }
  }
}

object Ex4_12 extends ScampiApp {
  import mpi2._

  Comm.world.rank match {
    case 0 => {
      val a3 = MpiFloat.alloc(3)
      val a2 = (MpiFloat * 2) @: a3
      Comm.world.send(a2, 1, 0)
      Comm.world.send(a3, 1, 0)
      result = true
    }
    case 1 => {
      val type2 = new ContiguousDatatype(MpiFloat, 2)
      type2.commit()
      val a = type2.alloc(2)
      val status1 = Comm.world.recv(a, 0, 0)
      val status2 = Comm.world.recv(a, 0, 0)
      result = (
        status1.count(type2).get == 1 &&
        status1.getElements(type2) == 2 &&
        !status2.count(type2).isDefined &&
        status2.getElements(type2) == 3)
    }
    case _ => {
      result = false
    }
  }
}

object Ex4_13 extends ScampiApp {
  import mpi2._

  // Send and receive a section of a 3D array
  val real = MpiFloat
  val sizeOfReal = real.extent.range

  // extract the section a(1:17:2, 3:11, 2:10)
  // and store it in e(:,:,:).

  val a = real.alloc(100 * 100 * 100)
  val e = real.alloc(9 * 9 * 9)

  // initialize a
  def va(x: Int, y: Int, z: Int): Float = {
    x.toFloat + 100.0f * (y.toFloat + 100.0f * z.toFloat)
  }
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      for (z <- 0 until 100)
        a(x + 100 * (y + 100 * z)) = va(x, y, z)

  // create a datatype for a 1D section
  val oneSlice = new VectorDatatype(real, 9, 1, 2)

  // create a datatype for a 2D section
  val twoSlice = new HvectorDatatype(oneSlice, 9, 1, 100 * sizeOfReal)

  // create a datatype for the entire section
  val threeSlice = new HvectorDatatype(twoSlice, 9, 1, 100 * 100 * sizeOfReal)

  threeSlice.commit()
  val aSlice = (threeSlice * 1) @: (a + (1 + 100 * (3 + 100 * 2)))
  Comm.world.doSendrecv(aSlice, Comm.world.rank, 0, e, Comm.world.rank, 0)

  // expected values of e
  def ve(x: Int, y: Int, z: Int): Float = {
    (2 * x + 1).toFloat + 100.0f * ((y + 3).toFloat + 100.0f * (z + 2).toFloat)
  }

  // validate e
  result = true
  for (x <- 0 until 9)
    for (y <- 0 until 9)
      for (z <- 0 until 9)
        result = result && e(x + 9 * (y + 9 * z)) == ve(x, y, z)
}

object Ex4_14 extends ScampiApp {
  import mpi2._

  // Copy the (strictly) lower triangular part of a matrix
  val real = MpiFloat

  // copy lower triangular part of array a
  // onto lower triangular part of array b
  val a = real.alloc(100 * 100)
  val b = real.alloc(100 * 100)

  // initialize a
  def va(x: Int, y: Int): Float = {
    x.toFloat + 100.0f * y.toFloat
  }
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      a(x + 100 * y) = va(x, y)

  // initialize b
  def vb(x: Int, y: Int) = -1.0f * va(x, y)
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      b(x + 100 * y) = vb(x, y)

  // compute start and size of each column
  val blocks = (1 to 100).map(i => Block(100 - i, 100 * (i - 1) + i))

  // create datatype for lower triangular part
  val lType = new IndexedDatatype(real, blocks)
  lType.commit()

  // bind lower triangular datatype to arrays
  val lowerA = (lType * 1) @: a
  val lowerB = (lType * 1) @: b

  Comm.world.doSendrecv(lowerA, Comm.world.rank, 0, lowerB, Comm.world.rank, 0)

  // validate b
  result = true
  for (x <- 0 until 100) {
    for (y <- 0 until x)
      result = result && b(x + 100 * y) == va(x, y)
    for (y <- x until 100)
      result = result && b(x + 100 * y) == vb(x, y)
  }
}

object Ex4_15 extends ScampiApp {
  import mpi2._

  // Transpose a matrix
  val real = MpiFloat
  val sizeOfReal = real.extent.range

  // transpose matrix a onto b
  val a = real.alloc(100 * 100)
  val b = real.alloc(100 * 100)

  // initialize a
  def va(x: Int, y: Int): Float = {
    x.toFloat + 100.0f * y.toFloat
  }
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      a(x + 100 * y) = va(x, y)

  // initialize b
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      b(x + 100 * y) = va(x, y)

  // create datatype for one row
  val row = new VectorDatatype(real, 100, 1, 100)

  // create datatype for matrix in row-major order
  val xpose = new HvectorDatatype(row, 100, 1, sizeOfReal)
  xpose.commit()

  // bind transpose datatype to matrix a
  val xposeA = (xpose * 1) @: a

  Comm.world.doSendrecv(xposeA, Comm.world.rank, 0, b, Comm.world.rank, 0)

  // validate b
  result = true
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      result = result && b(x + 100 * y) == va(y, x)
}

object Ex4_16 extends ScampiApp {
  import mpi2._

  // Another approach to the transpose problem
  val real = MpiFloat
  val sizeOfReal = real.extent.range

  // transpose matrix a onto b
  val a = real.alloc(100 * 100)
  val b = real.alloc(100 * 100)

  // initialize a
  def va(x: Int, y: Int): Float = {
    x.toFloat + 100.0f * y.toFloat
  }
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      a(x + 100 * y) = va(x, y)

  // initialize b
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      b(x + 100 * y) = va(x, y)

  // create datatype for one row
  val row = new VectorDatatype(real, 100, 1, 100)

  // create datatype for one row, with the extent of one real number
  val row1 = new ResizedDatatype(row, Extent(row.extent.lowerBound, sizeOfReal))
  row1.commit()

  // bind resized row datatype to all rows of a
  val row1A = (row1 * 100) @: a

  Comm.world.doSendrecv(row1A, Comm.world.rank, 0, b, Comm.world.rank, 0)

  // validate b
  result = true
  for (x <- 0 until 100)
    for (y <- 0 until 100)
      result = result && b(x + 100 * y) == va(y, x)
}

object Ex4_17 extends ScampiApp {
  import mpi2._

  // We manipulate an array of structures

  // Build datatype describing structure
  val particleType = new StructDatatype(
    StructBlock(MpiInt, 1),
    StructBlock(MpiDouble, 6),
    StructBlock(MpiByte, 7))

  // 4.1: Send the entire array
  particleType.commit()
  val numParticles = 1000
  val particleA = particleType.alloc(numParticles)
  // Initialize the array
  val numClasses = 4
  def particleValue(idx: Int) = {
    val klass = idx % numClasses
    val ds = (idx until idx + 6).map(_.toDouble)
    val bs = (idx until idx + 7).map(b => (b % Byte.MaxValue).toByte)
    (klass, ds, bs)
  }
  var off = 0
  for (i <- 0 until numParticles) {
    val (klass, ds, bs) = particleValue(i)
    particleA(off) = klass
    off += 1
    for (j <- 0 until ds.length) particleA(off + j) = ds(j)
    off += ds.length
    for (j <- 0 until bs.length) particleA(off + j) = bs(j)
    off += bs.length
  }
  val particleB = particleType.alloc(numParticles)
  Comm.world.doSendrecv(
    particleA,
    Comm.world.rank,
    0,
    particleB,
    Comm.world.rank,
    0)
  // Validate the receiving array
  result = true
  off = 0
  for (i <- 0 until numParticles) {
    val (klass, ds, bs) = particleValue(i)
    result = result && particleB(off) == klass
    off += 1
    for (j <- 0 until ds.length)
      result = result && particleB(off + j) == ds(j)
    off += ds.length
    for (j <- 0 until bs.length)
      result = result && particleB(off + j) == bs(j)
    off += bs.length
  }

  // 4.2: Send only the entries of class zero particles, preceded by
  // the number of such particles
  val zblocks =
    (particleA.grouped(particleA.datatype.multiplicity).zipWithIndex.withFilter {
      case (p, i) => p(0) == 0
    } map {
      case (p, i) => Block(1, i)
    }).toSeq
  // Create datatype for class zero particles
  val zParticleType = new IndexedDatatype(particleType, zblocks)
  // Prepend particle count
  val particleCount = MpiInt.alloc(1)
  particleCount(0) = zblocks.length
  val zType = new StructDatatype(
    StructBlock(MpiInt, 1, Some(getAddress(particleCount.pointer))),
    StructBlock(zParticleType, 1, Some(getAddress(particleA.pointer))))
  zType.commit()
  val zBType = new StructDatatype(
    StructBlock(MpiInt, 1),
    StructBlock(particleType, numParticles))
  zBType.commit()
  val z = (zType * 1) @: Bottom
  val zB = zBType.alloc(1)
  Comm.world.doSendrecv(z, Comm.world.rank, 0, zB, Comm.world.rank, 0)
  // Validate the receiving array
  result = result && zB(0) == zblocks.length
  val zParticles = particleType @: (zB :@ 1)
  off = 0
  for (i <- 0 until zblocks.length) {
    val (klass, ds, bs) = particleValue(numClasses * i)
    assume(klass == 0)
    result = result && zParticles(off) == klass
    off += 1
    for (j <- 0 until ds.length)
      result = result && zParticles(off + j) == ds(j)
    off += ds.length
    for (j <- 0 until bs.length)
      result = result && zParticles(off + j) == bs(j)
    off += bs.length
  }

  // 4.3: Send the first two coordinates of all entries
  val allPairsType = new HvectorDatatype(
    MpiDouble,
    numParticles,
    2,
    particleType.extent.range)
  allPairsType.commit()
  val allPairsA = allPairsType @: (particleA :@ 1)
  val allPairsB = MpiDouble.alloc(numParticles * 2)
  Comm.world.doSendrecv(
    allPairsA,
    Comm.world.rank,
    0,
    allPairsB,
    Comm.world.rank,
    0)
  // Validate the receiving array
  off = 0
  for (i <- 0 until numParticles) {
    val (_, ds, _) = particleValue(i)
    for (j <- 0 until 2)
      result = result && allPairsB(off + j) == ds(j)
    off += 2
  }

  // An alternative solution to 4.3 (slightly different from MPI report)
  val pairType = new ContiguousDatatype(MpiDouble, 2)
  val onePairType = new ResizedDatatype(
    pairType,
    Extent(-particleType.blocks(1).displacement.get, particleType.extent.range)
  )
  onePairType.commit()
  // Zero out "pairs"
  for (i <- 0 until allPairsB.length)
    allPairsB(i) = 0.0
  val onePair = onePairType @: (particleA :@ 1)
  Comm.world.doSendrecv(
    onePair,
    Comm.world.rank,
    0,
    allPairsB,
    Comm.world.rank,
    0)
  // Validate the receiving array
  // Validate the receiving array
  off = 0
  for (i <- 0 until numParticles) {
    val (_, ds, _) = particleValue(i)
    for (j <- 0 until 2)
      result = result && allPairsB(off + j) == ds(j)
    off += 2
  }
}

object Ex4_18 extends ScampiApp {
  import mpi2._

  // The same manipulations as in the previous example, but use
  // absolute addresses in datatypes.

  // A somewhat different treatment for particle type struct.
  class Particle(valueBuffer: SeqValueBuffer[Any]) {
    var off = 0
    val klassSeq = (MpiInt * 1) @: (valueBuffer :@ off)
    off += 1
    val d = (MpiDouble * 6) @: (valueBuffer :@ off)
    off += 6
    val b = (MpiByte * 7) @: (valueBuffer :@ off)
    def klass = klassSeq(0)
    def klass_=(k: Int) = klassSeq(0) = k
    def setTo(other: Particle) {
      klass = other.klass
      for (i <- 0 until d.length) d(i) = other.d(i)
      for (i <- 0 until b.length) b(i) = other.b(i)
    }
    def isEqualTo(other: Particle): Boolean = {
      klass == other.klass &&
      (0 until d.length).forall(i => d(i) == other.d(i)) &&
      (0 until b.length).forall(i => b(i) == other.b(i))
    }
    override def toString: String = s"Particle(${valueBuffer})"
  }
  object Particle {
    val datatype = new StructDatatype(
      StructBlock(MpiInt, 1, None),
      StructBlock(MpiDouble, 6, None),
      StructBlock(MpiByte, 7, None)
    )
    datatype.commit()
    def apply(numElements: Int) = {
      require(numElements > 0)
      new mutable.IndexedSeq[Particle] {
        val valueBuffer = datatype.alloc(numElements)
        val length = valueBuffer.valueCount
        val particles =
          for (i <- 0 until length)
          yield new Particle((datatype * 1) @: (valueBuffer + i))
        def apply(idx: Int) = particles(idx)
        def update(idx: Int, elem: Particle) {
          particles(idx).setTo(elem)
        }
      }
    }
  }

  val numParticles = 1000
  val particles = Particle(numParticles)
  // Initialize the array
  val numClasses = 4
  val pBuff = Particle.datatype.alloc(1)
  val pI = (MpiInt * 1) @: pBuff
  val pD = (MpiDouble * 6) @: (pBuff :@ 1)
  val pB = (MpiByte * 7) @: (pBuff :@ 7)
  def particleValue(idx: Int) = {
    pI(0) = idx % numClasses
    for (i <- 0 until 6) pD(i) = (i + idx).toDouble
    for (i <- 0 until 7) pB(i) = ((i + idx) % Byte.MaxValue).toByte
    new Particle(pBuff)
  }
  for (p <- particles.zipWithIndex) {
    p match {
      case (particle, i) => particle.setTo(particleValue(i))
    }
  }

  // Build datatype describing first array entry using absolute
  // addresses
  val particleType =
    new StructDatatype(
      StructBlock(
        MpiInt,
        1,
        Some(getAddress((particles.valueBuffer :@ 0).pointer))),
      StructBlock(
        MpiDouble,
        6,
        Some(getAddress((particles.valueBuffer :@ 1).pointer))),
      StructBlock(
        MpiByte,
        7,
        Some(getAddress((particles.valueBuffer :@ 7).pointer))))
  particleType.commit()

  // 5.1: Send the entire array
  val particlesA = (particleType * particles.length) @: Bottom
  val particlesB = Particle(numParticles)
  Comm.world.doSendrecv(
    particlesA,
    Comm.world.rank,
    0,
    particlesB.valueBuffer,
    Comm.world.rank,
    0)
  // Validate the receiving array
  result = true
  for (p <- particlesB.zipWithIndex) {
    p match {
      case (particle, i) =>
        result = result && particle.isEqualTo(particles(i))
    }
  }

  // 5.2: Send the entries of class zero, preceded by the number of
  // such entries
  var i = 0
  val zblocks =
    ((List.empty[Block] /: particles) {
      case (acc, p) => {
        val nextAcc =
          if (p.klass == 0) {
            acc match {
              case Nil => List(Block(1, i))
              case Block(n, st) :: tl => {
                if (i == st + n) Block(n + 1, st) :: tl
                else Block(1, i) :: acc
              }
            }
          } else acc
        i += 1
        nextAcc
      }
    }).reverse
  // zParticleType describes particles with class zero, using their
  // relative addresses
  val zParticleType = new IndexedDatatype(particleType, zblocks)
  // Prepend particle count
  val particleCount = MpiInt.alloc(1)
  particleCount(0) = (0 /: zblocks) { case (acc, b) => acc + b.length }
  val zType = new StructDatatype(
    StructBlock(MpiInt, 1, Some(getAddress(particleCount.pointer))),
    StructBlock(zParticleType, 1, Some(getAddress(Bottom.pointer))))
  zType.commit()
  val zBType = new StructDatatype(
    StructBlock(MpiInt, 1, None),
    StructBlock(Particle.datatype, particles.length, None))
  zBType.commit()

  val z = (zType * 1) @: Bottom
  val zB = zBType.alloc(1)
  Comm.world.doSendrecv(z, Comm.world.rank, 0, zB, Comm.world.rank, 0)
  // Validate the receiving array
  val numZParticles = ((MpiInt * 1) @: zB)(0)
  val zParticlesBuffer = Particle.datatype @: (zB :@ 1)
  val zParticles =
    for (i <- 0 until numZParticles)
    yield new Particle((Particle.datatype * 1) @: (zParticlesBuffer + i))
  for (i <- 0 until zParticles.length) {
    result = (
      result &&
      zParticles(i).isEqualTo(particles(i * numClasses))
    )
  }
}

object Ex4_19 extends ScampiApp {
  import mpi2._

  // Handling of unions

  val types: List[SeqDatatype[_]] = List(MpiInt, MpiDouble)
  // Computation of union element size
  val range = AlignHelper.align(types.map(_.extent.range.toLong).max,
                                types.map(_.alignment).max).toInt
  // Simulate a C union by resizing Int and Double datatypes
  val utypes = types.map(t => new ResizedDatatype(t, Extent(0, range)))
  utypes.foreach(_.commit())
  val numElements = 1000
  // Send buffer
  val buffA = MpiByte.alloc(numElements * range)
  val usA = utypes.map(u => (u * numElements) @: buffA)
  val uiA = usA(0).asInstanceOf[SeqValueBuffer[Int]]
  val ufA = usA(1).asInstanceOf[SeqValueBuffer[Double]]
  // Receive buffer
  val buffB = MpiByte.alloc(numElements * range)
  val usB = utypes.map(u => (u * numElements) @: buffB)
  val uiB = usB(0).asInstanceOf[SeqValueBuffer[Int]]
  val ufB = usB(1).asInstanceOf[SeqValueBuffer[Double]]
  // Test with integers
  for (i <- 0 until numElements) uiA(i) = i
  Comm.world.doSendrecv(uiA, Comm.world.rank, 0, uiB, Comm.world.rank, 0)
  result = true
  for (i <- 0 until numElements) result = result && (uiB(i) == i)
  // Test with doubles
  for (i <- 0 until numElements) ufA(i) = i.toDouble
  Comm.world.doSendrecv(ufA, Comm.world.rank, 0, ufB, Comm.world.rank, 0)
  for (i <- 0 until numElements)
    result = result && (ufB(i) == i.toDouble)
}

// object Ex4_21 extends ScampiApp {
//   import mpi2._

//   val i = 16
//   val j = -802

//   if (Comm.world.rank == 0) {
//     // Sender code
//     object codec extends FixedPackedCodec {
//       val datatypeSequence = List((2, MpiInt))
//     }
//     Comm.world.send(Comm.world.encode(codec, MpiInt(i, j)), 1, 0)
//     result = true
//   } else {
//     // Receiver code
//     val ints = MpiInt.alloc(2)
//     Comm.world.recv(ints, 0, 0)
//     result = ints(0) == i && ints(1) == j
//   }
// }

// object Ex4_22 extends ScampiApp {
//   import mpi2._

//   val i = 200

//   if (Comm.world.rank == 0) {
//     // Sender code
//     val iBuff = MpiInt.alloc()
//     iBuff(0) = i
//     val a = MpiFloat.alloc(i)
//     for (j <- 0 until i) a(j) = j * 2.0f

//     val newtype = new StructDatatype(
//       StructBlock(MpiInt, 1, Some(getAddress(iBuff.pointer))),
//       StructBlock(MpiFloat, i, Some(getAddress(a.pointer))))
//     newtype.commit()

//     object codec extends FixedPackedCodec {
//       val datatypeSequence = List((1, newtype))
//     }
//     val buff = Comm.world.encode(codec, Bottom.bind(newtype))
//     Comm.world.send(buff, 1, 0)
//     result = true
//   } else {
//     // Receiver code
//     object codec extends PackedCodec {
//       def blockSignature(
//         blocks: Seq[mpi2.ValueSeq[_]]): Option[(Int, TypedDatatype[_])] = {
//         if (blocks.length == 0)
//           Some((1, MpiInt))
//         else if (blocks.length == 1)
//           blocks(0) match { case MpiInt(i) => Some((i, MpiFloat)) }
//         else
//           None
//       }
//       val maxSize = 1000L
//     }
//     val buff = Comm.world.packedValueBuffer(codec)
//     Comm.world.recv(buff, 0, 0)
//     val decoded = buff.decoded
//     result = (decoded.length == 2 &&
//       (decoded(0) match {
//         case MpiInt(ir) => {
//           (ir == i &&
//             (decoded(1) match {
//               case MpiFloat(ars@_*) if ars.length == i => {
//                 ars.zipWithIndex.forall {
//                   case (a, j) => a == j * 2.0f
//                 }
//               }
//               case _ => false
//             })
//           )
//         }
//         case _ => false
//       }))
//   }
// }
