//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

trait StructBlockComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  case class StructBlock[V](
    datatype: mpi3.SeqDatatype[V],
    length: Int,
    displacement: Option[mpi3.lib.MPI_Aint] = None)

  object StructBlock {
    def withDisplacements(blocks: Seq[StructBlock[_]]): Seq[StructBlock[_]] = {
      val result =
        (((0L, 0L, List.empty[StructBlock[_]]) /: blocks) {
          case ((disp, size, dBlocks), b) => {
            val newDisp: mpi3.lib.MPI_Aint = b.displacement.getOrElse(
              AlignHelper.align(disp + size, b.datatype.alignment))
            val newSize: mpi3.lib.MPI_Aint = b.datatype.extent.range * b.length
            (newDisp,
              newSize,
              StructBlock(b.datatype, b.length, Some(newDisp)) :: dBlocks)
          }
        })._3.reverse
      assume(displacementsAreValid(result))
      result
    }

    def displacementsAreDefined(blocks: Seq[StructBlock[_]]): Boolean =
      blocks.forall(_.displacement.isDefined)

    def displacementsAreValid(blocks: Seq[StructBlock[_]]): Boolean =
      blocks.forall(
        b => b.displacement.map(_ % b.datatype.alignment == 0).getOrElse(false))
  }
}
