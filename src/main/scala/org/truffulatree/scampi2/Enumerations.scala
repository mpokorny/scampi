//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

trait Enumerations {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  object ThreadLevel extends Enumeration {
    type ThreadLevel = Value
    val Single = Value(mpi2.lib.MPI_THREAD_SINGLE)
    val Funneled = Value(mpi2.lib.MPI_THREAD_FUNNELED)
    val Serialized = Value(mpi2.lib.MPI_THREAD_SERIALIZED)
    val Multiple = Value(mpi2.lib.MPI_THREAD_MULTIPLE)
  }

  object ArrayOrder extends Enumeration {
    type ArrayOrder = Value
    val C = Value(mpi2.lib.MPI_ORDER_C)
    val Fortran = Value(mpi2.lib.MPI_ORDER_FORTRAN)
    val RowMajor = C
    val ColumnMajor = Fortran
  }

  object ArrayDistribution extends Enumeration {
    type ArrayDistribution = Value
    val Block = Value(mpi2.lib.MPI_DISTRIBUTE_BLOCK)
    val Cyclic = Value(mpi2.lib.MPI_DISTRIBUTE_CYCLIC)
    val None = Value(mpi2.lib.MPI_DISTRIBUTE_NONE)
  }

  object Errors extends Enumeration {
    type Errors = Value
    val AreFatal, Return, ThrowExceptions = Value
  }

  object TopoType extends Enumeration {
    type TopoType = Value
    val Graph = Value(mpi2.lib.MPI_GRAPH)
    val Cart = Value(mpi2.lib.MPI_CART)
    val DistGraph = Value(mpi2.lib.MPI_DIST_GRAPH)
  }

  object CombinerType extends Enumeration {
    type Combiner = Value
    val Contiguous = Value(mpi2.lib.MPI_COMBINER_CONTIGUOUS)
    val Darray = Value(mpi2.lib.MPI_COMBINER_DARRAY)
    val Dup = Value(mpi2.lib.MPI_COMBINER_DUP)
    val F90Complex = Value(mpi2.lib.MPI_COMBINER_F90_COMPLEX)
    val F90Integer = Value(mpi2.lib.MPI_COMBINER_F90_INTEGER)
    val F90Real = Value(mpi2.lib.MPI_COMBINER_F90_REAL)
    val HindexedInteger = Value(mpi2.lib.MPI_COMBINER_HINDEXED_INTEGER)
    val Hindexed = Value(mpi2.lib.MPI_COMBINER_HINDEXED)
    val HvectorInteger = Value(mpi2.lib.MPI_COMBINER_HVECTOR_INTEGER)
    val Hvector = Value(mpi2.lib.MPI_COMBINER_HVECTOR)
    val IndexedBlock = Value(mpi2.lib.MPI_COMBINER_INDEXED_BLOCK)
    val Indexed = Value(mpi2.lib.MPI_COMBINER_INDEXED)
    val Named = Value(mpi2.lib.MPI_COMBINER_NAMED)
    val Resized = Value(mpi2.lib.MPI_COMBINER_RESIZED)
    val StructInteger = Value(mpi2.lib.MPI_COMBINER_STRUCT_INTEGER)
    val Struct = Value(mpi2.lib.MPI_COMBINER_STRUCT)
    val Subarray = Value(mpi2.lib.MPI_COMBINER_SUBARRAY)
    val Vector = Value(mpi2.lib.MPI_COMBINER_VECTOR)
  }

  object Comparison extends Enumeration {
    type Comparison = Value
    val Identical = Value(mpi2.lib.MPI_IDENT)
    val Similar = Value(mpi2.lib.MPI_SIMILAR)
    val Congruent = Value(mpi2.lib.MPI_CONGRUENT)
    val Unequal = Value(mpi2.lib.MPI_UNEQUAL)
  }

  object WinMode extends Enumeration {
    type WinMode = Value
    val NoCheck = Value(mpi2.lib.MPI_MODE_NOCHECK)
    val NoStore = Value(mpi2.lib.MPI_MODE_NOSTORE)
    val NoPut = Value(mpi2.lib.MPI_MODE_NOPUT)
    val NoPrecede = Value(mpi2.lib.MPI_MODE_NOPRECEDE)
    val NoSucceed = Value(mpi2.lib.MPI_MODE_NOSUCCEED)
    def assert(modes: Seq[WinMode]): Int = (0 /: modes)(_ | _.id)
  }

  object Lock extends Enumeration {
    type Lock = Value
    val Exclusive = Value(mpi2.lib.MPI_LOCK_EXCLUSIVE)
    val Shared = Value(mpi2.lib.MPI_LOCK_SHARED)
  }

  object FileMode extends Enumeration {
    type FileMode = Value
    val ReadOnly = Value(mpi2.lib.MPI_MODE_RDONLY)
    val ReadWrite = Value(mpi2.lib.MPI_MODE_RDWR)
    val WriteOnly = Value(mpi2.lib.MPI_MODE_WRONLY)
    val Create = Value(mpi2.lib.MPI_MODE_CREATE)
    val Exclusive = Value(mpi2.lib.MPI_MODE_EXCL)
    val DeleteOnClose = Value(mpi2.lib.MPI_MODE_DELETE_ON_CLOSE)
    val UniqueOpen = Value(mpi2.lib.MPI_MODE_UNIQUE_OPEN)
    val Append = Value(mpi2.lib.MPI_MODE_APPEND)
    val Sequential = Value(mpi2.lib.MPI_MODE_SEQUENTIAL)
    def amode(modes: Seq[FileMode]): Int = (0 /: modes)(_ | _.id)
  }

  object Seek extends Enumeration {
    type Seek = Value
    val Set = Value(mpi2.lib.MPI_SEEK_SET)
    val Current = Value(mpi2.lib.MPI_SEEK_CUR)
    val End = Value(mpi2.lib.MPI_SEEK_END)
  }
}
