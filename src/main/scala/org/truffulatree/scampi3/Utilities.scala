//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.language.existentials

protected[scampi3] trait Utilities {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  def mpiCall(
      fnInv: => Int,
      mkExc: (Int) => mpi3.MpiException = mpi3.MpiException.apply) {
    val rc = fnInv
    if (rc != mpi3.lib.MPI_SUCCESS) throw mkExc(rc)
  }

  implicit def allocComm() = mpi3.allocateComm(1)

  implicit def allocGroup() = mpi3.allocateGroup(1)

  implicit def allocFile() = mpi3.allocateFile(1)

  implicit def allocOp() = mpi3.allocateOp(1)

  implicit def allocErrhandler() = mpi3.allocateErrhandler(1)

  implicit def allocRequest() = mpi3.allocateRequest(1)

  implicit def allocInfo() = mpi3.allocateInfo(1)

  implicit def allocAint() = mpi3.allocateAint(1)

  implicit def allocOffset() = mpi3.allocateOffset(1)

  case class Extent(lowerBound: Long, range: Long)

  case class Block(length: Int, displacement: Int)

  case class HBlock(length: Int, displacement: Long)

  case class TypeBlock(
    length: Int,
    displacement: Long,
    datatype: mpi3.SeqDatatype[_])

  case class SubarrayDim(size: Int, subsize: Int, start: Int)

  case class DarrayDim(
    gsize: Int,
    distribution: mpi3.ArrayDistribution.ArrayDistribution,
    distArg: Int = mpi3.distributeDefaultArg,
    psize: Int)
}
