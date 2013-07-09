//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.language.existentials

protected[scampi2] trait Utilities {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  def mpiCall(
      fnInv: => Int,
      mkExc: (Int) => mpi2.MpiException = mpi2.MpiException.apply) {
    val rc = fnInv
    if (rc != mpi2.lib.MPI_SUCCESS) throw mkExc(rc)
  }

  implicit def allocGroup() = mpi2.allocateGroup(1)

  implicit def allocFile() = mpi2.allocateFile(1)

  implicit def allocOp() = mpi2.allocateOp(1)

  implicit def allocErrhandler() = mpi2.allocateErrhandler(1)

  implicit def allocRequest() = mpi2.allocateRequest(1)

  implicit def allocInfo() = mpi2.allocateInfo(1)

  implicit def allocAint() = mpi2.allocateAint(1)

  implicit def allocOffset() = mpi2.allocateOffset(1)

  case class Extent(lowerBound: Long, range: Long)

  case class Block(length: Int, displacement: Int)

  case class HBlock(length: Int, displacement: Long)

  case class TypeBlock(
    length: Int,
    displacement: Long,
    datatype: mpi2.SeqDatatype[_])

  case class SubarrayDim(size: Int, subsize: Int, start: Int)

  case class DarrayDim(
    gsize: Int,
    distribution: mpi2.ArrayDistribution.ArrayDistribution,
    distArg: Int = mpi2.distributeDefaultArg,
    psize: Int)
}
