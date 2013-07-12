//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2.mpich;

import org.bridj.Pointer;
import org.bridj.Callback;
import org.bridj.CLong;

public class Callbacks {
    public static abstract class MPI_Comm_copy_attr_function
	extends Callback<MPI_Comm_copy_attr_function> {
	public abstract int apply(int t,
				  int keyval,
				  Pointer<?> extraState,
				  Pointer<?> attributeValIn,
				  Pointer<Pointer<?>> attributeValOut,
				  Pointer<Integer> flag);
    };

    public static abstract class MPI_Comm_delete_attr_function
	extends Callback<MPI_Comm_delete_attr_function> {
	public abstract int apply(int t,
				  int keyval,
				  Pointer<?> attributeVal,
				  Pointer<?> extraState);
    };

    public static abstract class MPI_Win_copy_attr_function
	extends Callback<MPI_Win_copy_attr_function> {
	public abstract int apply(int t,
				  int keyval,
				  Pointer<?> extraState,
				  Pointer<?> attributeValIn,
				  Pointer<Pointer<?>> attributeValOut,
				  Pointer<Integer> flag);
    };

    public static abstract class MPI_Win_delete_attr_function
	extends Callback<MPI_Win_delete_attr_function> {
	public abstract int apply(int t,
				  int keyval,
				  Pointer<?> attributeVal,
				  Pointer<?> extraState);
    };

    public static abstract class MPI_Type_copy_attr_function
	extends Callback<MPI_Type_copy_attr_function> {
	public abstract int apply(int t,
				  int keyval,
				  Pointer<?> extraState,
				  Pointer<?> attributeValIn,
				  Pointer<Pointer<?>> attributeValOut,
				  Pointer<Integer> flag);
    };

    public static abstract class MPI_Type_delete_attr_function
	extends Callback<MPI_Type_delete_attr_function> {
	public abstract int apply(int t,
				  int keyval,
				  Pointer<?> attributeVal,
				  Pointer<?> extraState);
    };

    public static abstract class MPI_Comm_errhandler_function
	extends Callback<MPI_Comm_errhandler_function> {
	public abstract void apply(Pointer<Integer> t,
				  Pointer<Integer> errcode);
    };

    public static abstract class MPI_Win_errhandler_function
	extends Callback<MPI_Win_errhandler_function> {
	public abstract void apply(Pointer<Integer> t,
				   Pointer<Integer> errcode);
    };

    public static abstract class MPI_File_errhandler_function
	extends Callback<MPI_File_errhandler_function> {
	public abstract void apply(Pointer<Integer> t,
				   Pointer<Integer> errcode);
    };

    public static abstract class MPI_User_function
	extends Callback<MPI_User_function> {
	public abstract void apply(Pointer<?> invec,
				   Pointer<?> inoutvec,
				   Pointer<Integer> len,
				   Pointer<Integer> datatype);
    };

    public static abstract class MPI_Grequest_cancel_function
	extends Callback<MPI_Grequest_cancel_function> {
	public abstract int apply(Pointer<?> extraState,
				  int complete);
    }

    public static abstract class MPI_Grequest_free_function
	extends Callback<MPI_Grequest_free_function> {
	public abstract int apply(Pointer<?> extraState);
    }

    public static abstract class MPI_Grequest_query_function
	extends Callback<MPI_Grequest_query_function> {
	public abstract int apply(Pointer<?> extraState,
				  Pointer<?> status);
    }

    public static abstract class MPI_Datarep_conversion_function
	extends Callback<MPI_Datarep_conversion_function> {
	public abstract int apply(Pointer<?> userbuf,
				  int datatype,
				  int count,
				  Pointer<?> filebuf,
				  long position,
				  Pointer<?> extraState);
    }

    public static abstract class MPI_Datarep_extent_function
	extends Callback<MPI_Datarep_extent_function> {
	public abstract int apply(int datatype,
				  Pointer<CLong> fileExtent,
				  Pointer<?> extraState);
    }
}
