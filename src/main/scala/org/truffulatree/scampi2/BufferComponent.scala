//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.concurrent.Lock
import org.bridj.Pointer

trait BufferComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  private val bufferLock = new Lock

  private var currentBuffer: Option[Pointer[Byte]] = None

  def attachBuffer(buff: Pointer[_], size: Int) {
    bufferLock.acquire()
    try {
      if (currentBuffer.isDefined) detachBuffer()
      mpi2.mpiCall(mpi2.lib.MPI_Buffer_attach(buff, size))
      currentBuffer = Some(buff.as(classOf[Byte]))
    } finally bufferLock.release()
  }

  def detachBuffer() {
    bufferLock.acquire()
    try {
      withOutVar { p: Pointer[Pointer[_]] =>
        withOutVar { sz: Pointer[Int] =>
          mpi2.mpiCall(mpi2.lib.MPI_Buffer_detach(p, sz))
          currentBuffer = None
        }
      }
    } finally bufferLock.release()
  }

  def attachNewBuffer(size: Int) {
    attachBuffer(Pointer.allocateBytes(size), size)
  }
}
