//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2.mpich;

import org.bridj.Pointer;
import org.bridj.TypedPointer;

public class TypedPointers {
    public static class MPI_File extends TypedPointer {
	public MPI_File(long address) {
	    super(address);
	}
	public MPI_File(Pointer address) {
	    super(address);
	}
    };
}
