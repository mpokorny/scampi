//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2.openmpi;

import org.bridj.Pointer;
import org.bridj.TypedPointer;

public class TypedPointers {
    public static class MPI_Comm extends TypedPointer {
	public MPI_Comm(long address) {
	    super(address);
	}
	public MPI_Comm(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Group extends TypedPointer {
	public MPI_Group(long address) {
	    super(address);
	}
	public MPI_Group(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Datatype extends TypedPointer {
	public MPI_Datatype(long address) {
	    super(address);
	}
	public MPI_Datatype(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Errhandler extends TypedPointer {
	public MPI_Errhandler(long address) {
	    super(address);
	}
	public MPI_Errhandler(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Info extends TypedPointer {
	public MPI_Info(long address) {
	    super(address);
	}
	public MPI_Info(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Op extends TypedPointer {
	public MPI_Op(long address) {
	    super(address);
	}
	public MPI_Op(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Request extends TypedPointer {
	public MPI_Request(long address) {
	    super(address);
	}
	public MPI_Request(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_Win extends TypedPointer {
	public MPI_Win(long address) {
	    super(address);
	}
	public MPI_Win(Pointer address) {
	    super(address);
	}
    }

    public static class MPI_File extends TypedPointer {
	public MPI_File(long address) {
	    super(address);
	}
	public MPI_File(Pointer address) {
	    super(address);
	}
    };
}
