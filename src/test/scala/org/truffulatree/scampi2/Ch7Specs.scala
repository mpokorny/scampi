//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class Ch7Spec extends ScampiSpecification {
  "ScaMPI" should {
    "pass example 7.1a" in {
      mpi2.dimsCreate(6, Seq(0, 0)) must_== Seq(3, 2)
    }
    "pass example 7.1b" in {
      mpi2.dimsCreate(7, Seq(0, 0)) must_== Seq(7, 1)
    }
    "pass example 7.1c" in {
      mpi2.dimsCreate(6, Seq(0, 3, 0)) must_== Seq(2, 3, 1)
    }
    "pass example 7.1d" in {
      mpi2.dimsCreate(7, Seq(0, 3, 0)) must throwA[IllegalArgumentException]
    }
    "pass example 7.2" in {
      runTest("Ex7_2", 4) must beTrue
    }
    if (libraryName != "openmpi") {
      "pass example 7.3a" in {
        runTest("Ex7_3a", 4) must beTrue
      }
    } else {
      "pass example 7.3a" in {
        skipped
      }
    }
    if (libraryName != "openmpi") {
      "pass example 7.3b" in {
        runTest("Ex7_3c", 4) must beTrue
      }
    } else {
      "pass example 7.3b" in {
        skipped
      }
    }
    if (libraryName != "openmpi") {
      "pass example 7.3c" in {
        runTest("Ex7_3c", 4) must beTrue
      }
    } else {
      "pass example 7.3c" in {
        skipped
      }
    }
    if (libraryName != "openmpi") {
      "pass example 7.4" in {
        runTest("Ex7_4", 12) must beTrue
      }
    } else {
      "pass example 7.4" in {
        skipped
      }
    }
    "pass example 7.5" in {
      runTest("Ex7_5", 4) must beTrue
    }
    "pass example 7.6" in {
      runTest("Ex7_6", 8) must beTrue
    }
    "pass example 7.7" in {
      runTest("Ex7_7", 12) must beTrue
    }
    "pass example 7.8a" in {
      runTest("Ex7_8a", 24) must beTrue
    }
  }
}

object Ex7_2 extends ScampiApp {
  import mpi2._
  val topo = Seq(Seq(1,3), Seq(0), Seq(3), Seq(0,2))
  val graph = Comm.world.graphCreate(topo, false).get
  result = graph.topo == topo
}

object Ex7_3a extends ScampiApp {
  import mpi2._
  val topo = Seq(Seq(1,3), Seq(0), Seq(3), Seq(0,2))
  val rank = Comm.world.rank
  def indicesWhere[A](seq: Seq[A], p: A => Boolean, from: Int = 0): List[Int] = {
    val i = seq.indexWhere(p, from)
    if (i != -1) i :: indicesWhere(seq, p, i + 1)
    else Nil
  }
  val sources = indicesWhere(topo, (s: Seq[Int]) => s.contains(rank))
  val destinations = topo(rank)
  val graph = Comm.world.distGraphCreate(
    Seq((rank, destinations.map(dst => (dst, Some(1))))),
    InfoNull,
    false)
  def symDiff[A](s1: Seq[A], s2: Seq[A]): Seq[A] = s1.diff(s2) ++ s2.diff(s1)
  val neighbors = graph.neighbors
  result = (
    symDiff(neighbors._1, sources).isEmpty &&
    symDiff(neighbors._2, destinations).isEmpty &&
    neighbors._3.isDefined &&
    symDiff(neighbors._3.get._1, neighbors._1.map(_ => 1)).isEmpty &&
    symDiff(neighbors._3.get._2, neighbors._2.map(_ => 1)).isEmpty
  )
}

object Ex7_3b extends ScampiApp {
  import mpi2._
  val topo = Seq(Seq(1,3), Seq(0), Seq(3), Seq(0,2))
  val rank = Comm.world.rank
  def indicesWhere[A](seq: Seq[A], p: A => Boolean, from: Int = 0): List[Int] = {
    val i = seq.indexWhere(p, from)
    if (i != -1) i :: indicesWhere(seq, p, i + 1)
    else Nil
  }
  val sources = indicesWhere(topo, (s: Seq[Int]) => s.contains(rank))
  val destinations = topo(rank)
  val graph =
    if (rank == 0)
      Comm.world.distGraphCreate(
        (0 until topo.length).map(r => ((r, topo(r).map(dst => (dst, None))))),
        InfoNull,
        false)
    else
      Comm.world.distGraphCreate(
        Seq.empty[(Int, Seq[(Int, Option[Int])])],
        InfoNull,
        false)
  def symDiff[A](s1: Seq[A], s2: Seq[A]): Seq[A] = s1.diff(s2) ++ s2.diff(s1)
  val neighbors = graph.neighbors
  result = (
    symDiff(neighbors._1, sources).isEmpty &&
    symDiff(neighbors._2, destinations).isEmpty &&
    !neighbors._3.isDefined
  )
}

object Ex7_3c extends ScampiApp {
  import mpi2._
  val topo = Seq(Seq(1,3), Seq(0), Seq(3), Seq(0,2))
  val rank = Comm.world.rank
  def indicesWhere[A](seq: Seq[A], p: A => Boolean, from: Int = 0): List[Int] = {
    val i = seq.indexWhere(p, from)
    if (i != -1) i :: indicesWhere(seq, p, i + 1)
    else Nil
  }
  val sources = indicesWhere(topo, (s: Seq[Int]) => s.contains(rank))
  val destinations = topo(rank)
  val graph =
    Comm.world.distGraphCreateAdjacent(
      sources. map(s => (s, None)), destinations.map(d => (d, None)),
      InfoNull,
      false)
  def symDiff[A](s1: Seq[A], s2: Seq[A]): Seq[A] = s1.diff(s2) ++ s2.diff(s1)
  val neighbors = graph.neighbors
  result = (
    symDiff(neighbors._1, sources).isEmpty &&
    symDiff(neighbors._2, destinations).isEmpty &&
    !neighbors._3.isDefined
  )
}

object Ex7_4 extends ScampiApp {
  import mpi2._
  val p = 4
  val q = 3
  val rank = Comm.world.rank
  val y = rank / p
  val x = rank % p
  val destinations = Seq(
    (p * y + (x + 1) % p, Some(2)),
    (p * y + (p + x - 1) % p, Some(2)),
    (p * ((y + 1) % q) + x, Some(2)),
    (p * ((q + y - 1) % q) + x, Some(2)),
    (p * ((y + 1) % q) + (x + 1) % p, Some(1)),
    (p * ((q + y - 1) % q) + (x + 1) % p, Some(1)),
    (p * ((y + 1) % q) + (p + x - 1) % p, Some(1)),
    (p * ((q + y - 1) % q) + (p + x - 1) % p, Some(1))
  )
  val graph = Comm.world.distGraphCreate(
    Seq((rank, destinations)),
    InfoNull,
    false)
  def symDiff[A](s1: Seq[A], s2: Seq[A]): Seq[A] = s1.diff(s2) ++ s2.diff(s1)
  val neighbors = graph.neighbors
  result = (
    symDiff(neighbors._1, destinations.map(_._1)).isEmpty &&
    symDiff(neighbors._2, destinations.map(_._1)).isEmpty &&
    neighbors._3.isDefined &&
    (neighbors._2.zip(neighbors._3.get._2.map(w => Some(w))).toMap
     == destinations.toMap) &&
    symDiff(neighbors._3.get._1, neighbors._3.get._2).isEmpty
  )
}

object Ex7_5 extends ScampiApp {
  import mpi2._
  val topo = Seq(Seq(1, 1, 3), Seq(0, 0), Seq(3), Seq(0, 2, 2))
  val graph = Comm.world.graphCreate(topo, false).get
  result = graph.neighbors == topo(Comm.world.rank)
}

object Ex7_6 extends ScampiApp {
  import mpi2._
  val topo = Seq(
    Seq(1, 0, 0),
    Seq(0, 2, 4),
    Seq(3, 4, 1),
    Seq(2, 6, 5),
    Seq(5, 1, 2),
    Seq(4, 3, 6),
    Seq(7, 5, 3),
    Seq(6, 7, 7)
  )
  assume(Comm.world.size >= topo.length)
  val graphOpt = Comm.world.graphCreate(topo, true)
  if (graphOpt.isDefined) {
    val graph = graphOpt.get
    val neighbors = graph.neighbors
    def aVal(r: Int) = 1.0f / (r + 1).toFloat
    val a = MpiFloat.alloc(1)
    a(0) = aVal(graph.rank)
    graph.doSendrecvReplace(a, neighbors(0), 0, neighbors(0), 0)
    graph.doSendrecvReplace(a, neighbors(1), 0, neighbors(2), 0)
    result = a(0) == aVal(topo(topo(graph.rank)(2))(0))
  } else {
    result = true
  }
}

object Ex7_7 extends ScampiApp {
  import mpi2._
  val numRows = 4
  val numCols = 3
  assume(Comm.world.size == numRows * numCols)
  val cart =
    Comm.world.cartCreate(Seq((numRows, true), (numCols, true)), false).get
  val coords = cart.coords
  val a = MpiInt.alloc(1)
  a(0) = coords(0) * numCols + coords(1)
  val (source, dest) = cart.shift(0, coords(1))
  cart.doSendrecvReplace(a, dest.get, 0, source.get, 0)
  result =
    (a(0) == (((coords(0) + numRows - coords(1)) % numRows) * numCols +
              coords(1)))
}

object Ex7_8a extends ScampiApp {
  import mpi2._
  val dims = Seq(2, 3, 4)
  assume(Comm.world.size == dims.reduce(_ * _))
  val cart = Comm.world.cartCreate(dims.map(d => (d, true)), false).get
  val subcart = cart.sub(List(true, false, true))
  result = (
    (subcart.size == Comm.world.size / dims(1)) &&
    (subcart.topo.map(d => (d._1, d._2)).toSeq ==
      Seq((dims(0), true), (dims(2), true)))
  )
}
