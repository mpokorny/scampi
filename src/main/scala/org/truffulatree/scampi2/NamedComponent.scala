//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.bridj.Pointer

trait NamedComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  trait Named {
    protected def setName(name: Pointer[Byte])

    protected def getName(buffer: Pointer[Byte])

    def name: String = getString(lib.MPI_MAX_OBJECT_NAME) { (len, buffer) =>
      getName(buffer)
    }

    def name_=(s: String) {
      val name = Pointer.pointerToCString(s).as(classOf[Byte])
      try setName(name) finally name.release()
    }
  }
}
