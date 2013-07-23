//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

trait Exceptions {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  class Exception(str: String) extends RuntimeException(str)

  class MpiException(val errorCode: Int) extends {
    val errorString = mpi3.errorString(errorCode)
  } with Exception(errorString)

  object MpiException {
    def apply(err: Int) = new MpiException(err)
    def unapply(e: MpiException) = Some(e.errorCode)
  }

  case class CommException(comm: mpi3.Comm, override val errorCode: Int)
      extends MpiException(errorCode)

  case class WinException(win: mpi3.Win, override val errorCode: Int)
      extends MpiException(errorCode)

  case class FileException(file: mpi3.File, override val errorCode: Int)
      extends MpiException(errorCode)

  class OutOfRangeException extends Exception("Value out of range for type")

  class AddressArithmeticException
      extends Exception("Base and adjustment addresses have different roots")

  class PackedLengthException
      extends Exception("Packed datatype has incorrect number of values")
}
