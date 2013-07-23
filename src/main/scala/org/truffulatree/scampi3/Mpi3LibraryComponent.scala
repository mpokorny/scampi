//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.language.implicitConversions
import org.bridj.{Pointer, NativeObject, NativeObjectInterface, CLong, Callback}

trait Mpi3LibraryComponent {
  protected[scampi3] val lib: Mpi3Library

  val aintByteSize: Int

  val aintAlignment: Int

  val offsetByteSize: Int

  val offsetAlignment: Int

  def allocateDatatype(n: Int = 1): Pointer[lib.MPI_Datatype]

  def allocateComm(n: Int = 1): Pointer[lib.MPI_Comm]

  def allocateGroup(n: Int = 1): Pointer[lib.MPI_Group]

  def allocateWin(n: Int = 1): Pointer[lib.MPI_Win]

  def allocateFile(n: Int = 1): Pointer[lib.MPI_File]

  def allocateOp(n: Int = 1): Pointer[lib.MPI_Op]

  def allocateErrhandler(n: Int = 1): Pointer[lib.MPI_Errhandler]

  def allocateRequest(n: Int = 1): Pointer[lib.MPI_Request]

  def allocateMessage(n: Int = 1): Pointer[lib.MPI_Message]

  def allocateInfo(n: Int = 1): Pointer[lib.MPI_Info]

  def allocateAint(n: Int = 1): Pointer[lib.MPI_Aint]

  def allocateOffset(n: Int = 1): Pointer[lib.MPI_Offset]

  // def associateStatus(peer: Pointer[lib.MPI_Status]): lib.MPI_Status

  def pointerToAint(v: mpi3.lib.MPI_Aint): Pointer[mpi3.lib.MPI_Aint]

  def aintFromPointer(v: Pointer[_]): mpi3.lib.MPI_Aint

  def newStatus(n: Int = 1): Seq[lib.MPI_Status]

  trait CStatus extends NativeObject {
    var MPI_SOURCE: Int
    var MPI_TAG: Int
    var MPI_ERROR: Int
  }

  val distributeDefaultArg = lib.MPI_DISTRIBUTE_DFLT_DARG

  protected def _aintToLong(aint: lib.MPI_Aint): Long

  protected def _aintFromLong(long: Long): lib.MPI_Aint

  protected def _offsetToLong(offset: lib.MPI_Offset): Long

  protected def _offsetFromLong(long: Long): lib.MPI_Offset

  implicit def aintToLong(aint: lib.MPI_Aint): Long = _aintToLong(aint)

  implicit def aintFromLong(long: Long): lib.MPI_Aint = _aintFromLong(long)

  implicit def offsetToLong(offset: lib.MPI_Offset): Long = _offsetToLong(offset)

  implicit def offsetFromLong(long: Long): lib.MPI_Offset = _offsetFromLong(long)

  protected val noRelease = new Pointer.Releaser {
    def release(p: Pointer[_]) {}
  }

  type CopyAttrFunction[T] =
    (T, Int, Pointer[_], Pointer[_], Pointer[Pointer[_]], Pointer[Int]) => Int

  type DeleteAttrFunction[T] =
    (T, Int, Pointer[_], Pointer[_]) => Int

  type ErrhandlerFunction[T] =
    (Pointer[T], Pointer[Int]) => Unit

  type UserFunction[T] =
    (Pointer[_], Pointer[_], Pointer[Int], Pointer[T]) => Unit

  type GrequestCancelFunction = (Pointer[_], Int) => Int

  type GrequestFreeFunction = Pointer[_] => Int

  type GrequestQueryFunction[T] = (Pointer[_], T) => Int

  type DatareqConversionFunction[D, O] =
    (Pointer[_], D, Int, Pointer[_], O, Pointer[_]) => Int

  type DatareqExtentFunction[D, A] = (D, Pointer[A], Pointer[_]) => Int

  trait Mpi3Library {
    // Types
    type MPI_Datatype
    type MPI_Comm
    type MPI_Group
    type MPI_Win
    type MPI_File
    type MPI_Op
    type MPI_Status <: CStatus
    type MPI_Errhandler
    type MPI_Request
    type MPI_Message
    type MPI_Info
    type MPI_Aint
    type MPI_Offset
    type MPI_Count

    // Values
    val MPI_IDENT: Int
    val MPI_CONGRUENT: Int
    val MPI_SIMILAR: Int
    val MPI_UNEQUAL: Int

    val MPI_CHAR: MPI_Datatype
    val MPI_SIGNED_CHAR: MPI_Datatype
    val MPI_UNSIGNED_CHAR: MPI_Datatype
    val MPI_BYTE: MPI_Datatype
    val MPI_WCHAR: MPI_Datatype
    val MPI_SHORT: MPI_Datatype
    val MPI_UNSIGNED_SHORT: MPI_Datatype
    val MPI_INT: MPI_Datatype
    val MPI_UNSIGNED: MPI_Datatype
    val MPI_LONG: MPI_Datatype
    val MPI_UNSIGNED_LONG: MPI_Datatype
    val MPI_FLOAT: MPI_Datatype
    val MPI_DOUBLE: MPI_Datatype
    val MPI_LONG_DOUBLE: MPI_Datatype
    val MPI_LONG_LONG_INT: MPI_Datatype
    val MPI_UNSIGNED_LONG_LONG: MPI_Datatype
    val MPI_LONG_LONG: MPI_Datatype
    val MPI_PACKED: MPI_Datatype
    val MPI_FLOAT_INT: MPI_Datatype
    val MPI_DOUBLE_INT: MPI_Datatype
    val MPI_LONG_INT: MPI_Datatype
    val MPI_SHORT_INT: MPI_Datatype
    val MPI_2INT: MPI_Datatype
    val MPI_LONG_DOUBLE_INT: MPI_Datatype
    val MPI_INT8_T: MPI_Datatype
    val MPI_INT16_T: MPI_Datatype
    val MPI_INT32_T: MPI_Datatype
    val MPI_INT64_T: MPI_Datatype
    val MPI_UINT8_T: MPI_Datatype
    val MPI_UINT16_T: MPI_Datatype
    val MPI_UINT32_T: MPI_Datatype
    val MPI_UINT64_T: MPI_Datatype
    val MPI_C_BOOL: MPI_Datatype
    val MPI_C_FLOAT_COMPLEX: MPI_Datatype
    val MPI_C_COMPLEX: MPI_Datatype
    val MPI_C_DOUBLE_COMPLEX: MPI_Datatype
    val MPI_C_LONG_DOUBLE_COMPLEX: MPI_Datatype
    val MPI_AINT: MPI_Datatype
    val MPI_OFFSET: MPI_Datatype

    val MPI_COMM_WORLD: MPI_Comm
    val MPI_COMM_SELF: MPI_Comm

    val MPI_GROUP_EMPTY: MPI_Group

    val MPI_WIN_NULL: MPI_Win

    val MPI_FILE_NULL: MPI_File

    val MPI_COMM_TYPE_SHARED: Int

    val MPI_MAX: MPI_Op
    val MPI_MIN: MPI_Op
    val MPI_SUM: MPI_Op
    val MPI_PROD: MPI_Op
    val MPI_LAND: MPI_Op
    val MPI_BAND: MPI_Op
    val MPI_LOR: MPI_Op
    val MPI_BOR: MPI_Op
    val MPI_LXOR: MPI_Op
    val MPI_BXOR: MPI_Op
    val MPI_MINLOC: MPI_Op
    val MPI_MAXLOC: MPI_Op
    val MPI_REPLACE: MPI_Op

    val MPI_TAG_UB: Int
    val MPI_HOST: Int
    val MPI_IO: Int
    val MPI_WTIME_IS_GLOBAL: Int
    val MPI_UNIVERSE_SIZE: Int
    val MPI_LASTUSEDCODE: Int
    val MPI_APPNUM: Int
    val MPI_WIN_BASE: Int
    val MPI_WIN_SIZE: Int
    val MPI_WIN_DISP_UNIT: Int
    val MPI_WIN_CREATE_FLAVOR: Int
    val MPI_WIN_MODEL: Int

    val MPI_WIN_FLAVOR_CREATE: Int
    val MPI_WIN_FLAVOR_ALLOCATE: Int
    val MPI_WIN_FLAVOR_DYNAMIC: Int
    val MPI_WIN_FLAVOR_SHARED: Int

    val MPI_WIN_SEPARATE: Int
    val MPI_WIN_UNIFIED: Int

    val MPI_COMM_NULL: MPI_Comm
    val MPI_OP_NULL: MPI_Op
    val MPI_GROUP_NULL: MPI_Group
    val MPI_DATATYPE_NULL: MPI_Datatype
    val MPI_REQUEST_NULL: MPI_Request
    val MPI_ERRHANDLER_NULL: MPI_Errhandler
    val MPI_MESSAGE_NULL: MPI_Message
    val MPI_MESSAGE_NO_PROC: MPI_Message

    val MPI_MAX_DATAREP_STRING: Int
    val MPI_MAX_PROCESSOR_NAME: Int
    val MPI_MAX_ERROR_STRING: Int
    val MPI_MAX_PORT_NAME: Int
    val MPI_MAX_OBJECT_NAME: Int
    val MPI_MAX_LIBRARY_VERSION_STRING: Int
    val MPI_UNDEFINED: Int
    val MPI_KEYVAL_INVALID: Int
    val MPI_BSEND_OVERHEAD: Int

    val MPI_BOTTOM: Pointer[_]
    val MPI_UNWEIGHTED: Pointer[Int]
    val MPI_WEIGHTS_EMPTY: Pointer[Int]

    val MPI_PROC_NULL: Int
    val MPI_ANY_SOURCE: Int
    val MPI_ROOT: Int
    val MPI_ANY_TAG: Int

    val MPI_LOCK_EXCLUSIVE: Int
    val MPI_LOCK_SHARED: Int

    val MPI_ERRORS_ARE_FATAL: MPI_Errhandler
    val MPI_ERRORS_RETURN: MPI_Errhandler

    val MPI_VERSION: Int = 2
    val MPI_SUBVERSION: Int = 2

    val MPI_INFO_NULL: MPI_Info
    val MPI_INFO_ENV: MPI_Info
    val MPI_MAX_INFO_KEY: Int
    val MPI_MAX_INFO_VAL: Int

    val MPI_ORDER_C: Int
    val MPI_ORDER_FORTRAN: Int
    val MPI_DISTRIBUTE_BLOCK: Int
    val MPI_DISTRIBUTE_CYCLIC: Int
    val MPI_DISTRIBUTE_NONE: Int
    val MPI_DISTRIBUTE_DFLT_DARG: Int

    val MPI_IN_PLACE: Pointer[_]

    val MPI_MODE_APPEND: Int
    val MPI_MODE_CREATE: Int
    val MPI_MODE_DELETE_ON_CLOSE: Int
    val MPI_MODE_EXCL: Int
    val MPI_MODE_NOCHECK: Int
    val MPI_MODE_NOSTORE: Int
    val MPI_MODE_NOPUT: Int
    val MPI_MODE_NOPRECEDE: Int
    val MPI_MODE_NOSUCCEED: Int
    val MPI_MODE_RDONLY: Int
    val MPI_MODE_RDWR: Int
    val MPI_MODE_SEQUENTIAL: Int
    val MPI_MODE_UNIQUE_OPEN: Int
    val MPI_MODE_WRONLY: Int

    val MPI_DISPLACEMENT_CURRENT: MPI_Offset

    val MPI_SEEK_CUR: Int
    val MPI_SEEK_END: Int
    val MPI_SEEK_SET: Int

    val MPI_STATUS_IGNORE: Pointer[MPI_Status]
    val MPI_STATUSES_IGNORE: Pointer[MPI_Status]
    val MPI_ERRCODES_IGNORE: Pointer[Int]

    val MPI_ARGV_NULL: Pointer[Pointer[Byte]]
    val MPI_ARGVS_NULL: Pointer[Pointer[Pointer[Byte]]]

    val MPI_THREAD_SINGLE: Int
    val MPI_THREAD_FUNNELED: Int
    val MPI_THREAD_SERIALIZED: Int
    val MPI_THREAD_MULTIPLE: Int

    val MPI_GRAPH: Int
    val MPI_CART: Int
    val MPI_DIST_GRAPH: Int

    val MPI_COMBINER_CONTIGUOUS: Int
    val MPI_COMBINER_DARRAY: Int
    val MPI_COMBINER_DUP: Int
    val MPI_COMBINER_F90_COMPLEX: Int
    val MPI_COMBINER_F90_INTEGER: Int
    val MPI_COMBINER_F90_REAL: Int
    val MPI_COMBINER_HINDEXED_BLOCK: Int
    val MPI_COMBINER_HINDEXED: Int
    val MPI_COMBINER_HVECTOR: Int
    val MPI_COMBINER_INDEXED_BLOCK: Int
    val MPI_COMBINER_INDEXED: Int
    val MPI_COMBINER_NAMED: Int
    val MPI_COMBINER_RESIZED: Int
    val MPI_COMBINER_STRUCT: Int
    val MPI_COMBINER_SUBARRAY: Int
    val MPI_COMBINER_VECTOR: Int

    val MPI_SUCCESS: Int
    val MPI_ERR_BUFFER: Int
    val MPI_ERR_COUNT: Int
    val MPI_ERR_TYPE: Int
    val MPI_ERR_TAG: Int
    val MPI_ERR_COMM: Int
    val MPI_ERR_RANK: Int
    val MPI_ERR_ROOT: Int
    val MPI_ERR_TRUNCATE: Int
    val MPI_ERR_GROUP: Int
    val MPI_ERR_OP: Int
    val MPI_ERR_REQUEST: Int
    val MPI_ERR_TOPOLOGY: Int
    val MPI_ERR_DIMS: Int
    val MPI_ERR_ARG: Int
    val MPI_ERR_OTHER: Int
    val MPI_ERR_UNKNOWN: Int
    val MPI_ERR_INTERN: Int
    val MPI_ERR_IN_STATUS: Int
    val MPI_ERR_PENDING: Int
    val MPI_ERR_FILE: Int
    val MPI_ERR_ACCESS: Int
    val MPI_ERR_AMODE: Int
    val MPI_ERR_BAD_FILE: Int
    val MPI_ERR_FILE_EXISTS: Int
    val MPI_ERR_FILE_IN_USE: Int
    val MPI_ERR_NO_SPACE: Int
    val MPI_ERR_NO_SUCH_FILE: Int
    val MPI_ERR_IO: Int
    val MPI_ERR_READ_ONLY: Int
    val MPI_ERR_CONVERSION: Int
    val MPI_ERR_DUP_DATAREP: Int
    val MPI_ERR_UNSUPPORTED_DATAREP: Int
    val MPI_ERR_INFO: Int
    val MPI_ERR_INFO_KEY: Int
    val MPI_ERR_INFO_VALUE: Int
    val MPI_ERR_INFO_NOKEY: Int
    val MPI_ERR_NAME: Int
    val MPI_ERR_NO_MEM: Int
    val MPI_ERR_NOT_SAME: Int
    val MPI_ERR_PORT: Int
    val MPI_ERR_QUOTA: Int
    val MPI_ERR_SERVICE: Int
    val MPI_ERR_SPAWN: Int
    val MPI_ERR_UNSUPPORTED_OPERATION: Int
    val MPI_ERR_WIN: Int
    val MPI_ERR_BASE: Int
    val MPI_ERR_LOCKTYPE: Int
    val MPI_ERR_KEYVAL: Int
    val MPI_ERR_RMA_CONFLICT: Int
    val MPI_ERR_RMA_SYNC: Int
    val MPI_ERR_SIZE: Int
    val MPI_ERR_DISP: Int
    val MPI_ERR_ASSERT: Int
    val MPI_ERR_LASTCODE: Int

    type MPI_Comm_copy_attr_function <: Callback[MPI_Comm_copy_attr_function]
    def MPI_Comm_copy_attr_function(
      fn: CopyAttrFunction[MPI_Comm]): MPI_Comm_copy_attr_function

    type MPI_Type_copy_attr_function <: Callback[MPI_Type_copy_attr_function]
    def MPI_Type_copy_attr_function(
      fn: CopyAttrFunction[MPI_Datatype]): MPI_Type_copy_attr_function

    type MPI_Win_copy_attr_function <: Callback[MPI_Win_copy_attr_function]
    def MPI_Win_copy_attr_function(
      fn: CopyAttrFunction[MPI_Win]): MPI_Win_copy_attr_function

    type MPI_Comm_delete_attr_function <: Callback[MPI_Comm_delete_attr_function]
    def MPI_Comm_delete_attr_function(
      fn: DeleteAttrFunction[MPI_Comm]): MPI_Comm_delete_attr_function

    type MPI_Type_delete_attr_function <: Callback[MPI_Type_delete_attr_function]
    def MPI_Type_delete_attr_function(
      fn: DeleteAttrFunction[MPI_Datatype]): MPI_Type_delete_attr_function

    type MPI_Win_delete_attr_function <: Callback[MPI_Win_delete_attr_function]
    def MPI_Win_delete_attr_function(
      fn: DeleteAttrFunction[MPI_Win]): MPI_Win_delete_attr_function

    type MPI_Comm_errhandler_function <: Callback[MPI_Comm_errhandler_function]
    def MPI_Comm_errhandler_function(
      fn: ErrhandlerFunction[MPI_Comm]): MPI_Comm_errhandler_function

    type MPI_Win_errhandler_function <: Callback[MPI_Win_errhandler_function]
    def MPI_Win_errhandler_function(
      fn: ErrhandlerFunction[MPI_Win]): MPI_Win_errhandler_function

    type MPI_File_errhandler_function <: Callback[MPI_File_errhandler_function]
    def MPI_File_errhandler_function(
      fn: ErrhandlerFunction[MPI_File]): MPI_File_errhandler_function

    type MPI_User_function <: Callback[MPI_User_function]
    def MPI_User_function(fn: UserFunction[MPI_Datatype]): MPI_User_function

    type MPI_Grequest_cancel_function <: Callback[MPI_Grequest_cancel_function]
    def MPI_Grequest_cancel_function(
      fn: GrequestCancelFunction): MPI_Grequest_cancel_function

    type MPI_Grequest_free_function <: Callback[MPI_Grequest_free_function]
    def MPI_Grequest_free_function(
      fn: GrequestFreeFunction): MPI_Grequest_free_function

    type MPI_Grequest_query_function <: Callback[MPI_Grequest_query_function]
    def MPI_Grequest_query_function(
      fn: GrequestQueryFunction[MPI_Status]): MPI_Grequest_query_function

    type MPI_Datarep_conversion_function <: Callback[MPI_Datarep_conversion_function]
    def MPI_Datarep_conversion_function(
      fn: DatareqConversionFunction[MPI_Datatype, MPI_Offset]):
        MPI_Datarep_conversion_function

    type MPI_Datarep_extent_function <: Callback[MPI_Datarep_extent_function]
    def MPI_Datarep_extent_function(
      fn: DatareqExtentFunction[MPI_Datatype, MPI_Aint]): MPI_Datarep_extent_function

    val MPI_COMM_NULL_COPY_FN: Pointer[MPI_Comm_copy_attr_function]
    val MPI_COMM_NULL_DELETE_FN: Pointer[MPI_Comm_delete_attr_function]
    val MPI_COMM_DUP_FN: Pointer[MPI_Comm_copy_attr_function]
    val MPI_WIN_NULL_COPY_FN: Pointer[MPI_Win_copy_attr_function]
    val MPI_WIN_NULL_DELETE_FN: Pointer[MPI_Win_delete_attr_function]
    val MPI_WIN_DUP_FN: Pointer[MPI_Win_copy_attr_function]
    val MPI_TYPE_NULL_COPY_FN: Pointer[MPI_Type_copy_attr_function]
    val MPI_TYPE_NULL_DELETE_FN: Pointer[MPI_Type_delete_attr_function]
    val MPI_TYPE_DUP_FN: Pointer[MPI_Type_copy_attr_function]

    // A.2.1 Point-to-Point Communication C Bindings

    def MPI_Bsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    def MPI_Bsend_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Buffer_attach(buffer: Pointer[_], size: Int): Int
    def MPI_Buffer_detach(buffer_addr: Pointer[Pointer[_]],
      size: Pointer[Int]): Int
    def MPI_Cancel(request: Pointer[MPI_Request]): Int
    def MPI_Get_count(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Pointer[Int]): Int
    def MPI_Ibsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Improbe(source: Int, tag: Int, comm: MPI_Comm,
      flag: Pointer[Int], message: Pointer[MPI_Message],
      status: Pointer[MPI_Status]): Int
    def MPI_Imrecv(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      message: Pointer[MPI_Message], request: Pointer[MPI_Request]): Int
    def MPI_Iprobe(source: Int, tag: Int, comm: MPI_Comm,
      flag: Pointer[Int], status: Pointer[MPI_Status]): Int
    def MPI_Irecv(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      source: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Irsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Isend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Issend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Mprobe(source: Int, tag: Int, comm: MPI_Comm,
      message: Pointer[MPI_Message], status: Pointer[MPI_Status]): Int
    def MPI_Mrecv(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      message: Pointer[MPI_Message], status: Pointer[MPI_Status]): Int
    def MPI_Probe(source: Int, tag: Int, comm: MPI_Comm,
      status: Pointer[MPI_Status]): Int
    def MPI_Recv(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      source: Int, tag: Int, comm: MPI_Comm, status: Pointer[MPI_Status]): Int
    def MPI_Recv_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, source: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Request_free(request: Pointer[MPI_Request]): Int
    def MPI_Request_get_status(request: MPI_Request, flag: Pointer[Int],
      status: Pointer[MPI_Status]): Int
    def MPI_Rsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    def MPI_Rsend_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Send(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    def MPI_Send_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Sendrecv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, dest: Int, sendtag: Int, recvbuf: Pointer[_],
      recvcount: Int, recvtype: MPI_Datatype, source: Int, recvtag: Int,
      comm: MPI_Comm, status: Pointer[MPI_Status]): Int
    def MPI_Sendrecv_replace(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, sendtag: Int, source: Int,
      recvtag: Int, comm: MPI_Comm, status: Pointer[MPI_Status]): Int
    def MPI_Ssend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    def MPI_Ssend_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Start(request: Pointer[MPI_Request]): Int
    def MPI_Startall(count: Int,
      array_of_requests: Pointer[MPI_Request]): Int
    def MPI_Test(request: Pointer[MPI_Request], flag: Pointer[Int],
      status: Pointer[MPI_Status]): Int
    def MPI_Test_cancelled(status: Pointer[MPI_Status],
      flag: Pointer[Int]): Int
    def MPI_Testall(count: Int, array_of_requests: Pointer[MPI_Request],
      flag: Pointer[Int], array_of_statuses: Pointer[MPI_Status]): Int
    def MPI_Testany(count: Int, array_of_requests: Pointer[MPI_Request],
      index: Pointer[Int], flag: Pointer[Int], status: Pointer[MPI_Status]): Int
    def MPI_Testsome(incount: Int,
      array_of_requests: Pointer[MPI_Request], outcount: Pointer[Int],
      array_of_indices: Pointer[Int],
      array_of_statuses: Pointer[MPI_Status]): Int
    def MPI_Wait(request: Pointer[MPI_Request],
      status: Pointer[MPI_Status]): Int
    def MPI_Waitall(count: Int, array_of_requests: Pointer[MPI_Request],
      array_of_statuses: Pointer[MPI_Status]): Int
    def MPI_Waitany(count: Int, array_of_requests: Pointer[MPI_Request],
      index: Pointer[Int], status: Pointer[MPI_Status]): Int
    def MPI_Waitsome(incount: Int,
      array_of_requests: Pointer[MPI_Request], outcount: Pointer[Int],
      array_of_indices: Pointer[Int],
      array_of_statuses: Pointer[MPI_Status]): Int

    // A.2.2 Datatypes C Bindings

    def MPI_Get_address(location: Pointer[_], address: Pointer[MPI_Aint]): Int
    def MPI_Get_elements(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Pointer[Int]): Int
    def MPI_Get_elements_x(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Pointer[MPI_Count]): Int
    def MPI_Pack(inbuf: Pointer[_], incount: Int,
      datatype: MPI_Datatype, outbuf: Pointer[_], outsize: Int,
      position: Pointer[Int], comm: MPI_Comm): Int
    def MPI_Pack_external(datarep: Pointer[Byte], inbuf: Pointer[_],
      incount: Int, datatype: MPI_Datatype, outbuf: Pointer[_],
      outsize: MPI_Aint,  position: Pointer[MPI_Aint]): Int
    def MPI_Pack_external_size(datarep: Pointer[Byte], incount: Int,
      datatype: MPI_Datatype, size: Pointer[MPI_Aint]): Int
    def MPI_Pack_size(incount: Int, datatype: MPI_Datatype,
      comm: MPI_Comm, size: Pointer[Int]): Int
    def MPI_Type_commit(datatype: Pointer[MPI_Datatype]): Int
    def MPI_Type_contiguous(count: Int, oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_darray(size: Int, rank: Int, ndims: Int,
      array_of_gsizes: Pointer[Int], array_of_distribs: Pointer[Int],
      array_of_dargs: Pointer[Int], array_of_psizes: Pointer[Int], order: Int,
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_hindexed(count: Int,
      array_of_blocklengths: Pointer[Int],
      array_of_displacements: Pointer[MPI_Aint], oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_hindexed_block(count: Int, blocklength: Int,
      array_of_displacements: Pointer[MPI_Aint], oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_hvector(count: Int, blocklength: Int, stride: MPI_Aint,
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_indexed_block(count: Int, blocklength: Int,
      array_of_displacements: Pointer[Int], oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_resized(oldtype: MPI_Datatype, lb: MPI_Aint,
      extent: MPI_Aint, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_struct(count: Int, array_of_blocklengths: Pointer[Int],
      array_of_displacements: Pointer[MPI_Aint],
      array_of_types: Pointer[MPI_Datatype],
      newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_create_subarray(ndims: Int, array_of_sizes: Pointer[Int],
      array_of_subsizes: Pointer[Int], array_of_starts: Pointer[Int],
      order: Int, oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_dup(dtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_free(datatype: Pointer[MPI_Datatype]): Int
    def MPI_Type_get_contents(datatype: MPI_Datatype, max_integers: Int,
      max_addresses: Int, max_datatypes: Int, array_of_integers: Pointer[Int],
      array_of_addresses: Pointer[MPI_Aint],
      array_of_datatypes: Pointer[MPI_Datatype]): Int
    def MPI_Type_get_envelope(datatype: MPI_Datatype,
      num_integers: Pointer[Int], num_addresses: Pointer[Int],
      num_datatypes: Pointer[Int], combiner: Pointer[Int]): Int
    def MPI_Type_get_extent(datatype: MPI_Datatype, lb: Pointer[MPI_Aint],
      extent: Pointer[MPI_Aint]): Int
    def MPI_Type_get_extent_x(datatype: MPI_Datatype, lb: Pointer[MPI_Count],
      extent: Pointer[MPI_Count]): Int
    def MPI_Type_get_true_extent(datatype: MPI_Datatype,
      true_lb: Pointer[MPI_Aint], true_extent: Pointer[MPI_Aint]): Int
    def MPI_Type_get_true_extent_x(datatype: MPI_Datatype,
      true_lb: Pointer[MPI_Count], true_extent: Pointer[MPI_Count]): Int
    def MPI_Type_indexed(count: Int,
      array_of_blocklengths: Pointer[Int], array_of_displacements: Pointer[Int],
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Type_size(datatype: MPI_Datatype, size: Pointer[Int]): Int
    def MPI_Type_size_x(datatype: MPI_Datatype, size: Pointer[MPI_Count]): Int
    def MPI_Type_vector(count: Int, blocklength: Int, stride: Int,
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    def MPI_Unpack(inbuf: Pointer[_], insize: Int,
      position: Pointer[Int], outbuf: Pointer[_], outcount: Int,
      datatype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Unpack_external(datarep: Pointer[Byte], inbuf: Pointer[_],
      insize: MPI_Aint, position: Pointer[MPI_Aint], outbuf: Pointer[_],
      outcount: Int, datatype: MPI_Datatype): Int

    // A.2.3 Collective Communication C Bindings

    def MPI_Allgather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Allgatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Allreduce(sendbuf: Pointer[_], recvbuf: Pointer[_],
      count: Int, datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    def MPI_Alltoall(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Alltoallv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcounts: Pointer[Int], rdispls: Pointer[Int], recvtype: MPI_Datatype,
      comm: MPI_Comm): Int
    def MPI_Alltoallw(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtypes: Pointer[MPI_Datatype],
      recvbuf: Pointer[_], recvcounts: Pointer[Int], rdispls: Pointer[Int],
      recvtypes: Pointer[MPI_Datatype], comm: MPI_Comm): Int
    def MPI_Barrier(comm: MPI_Comm): Int
    def MPI_Bcast(buffer: Pointer[_], count: Int,
      datatype: MPI_Datatype, root: Int, comm: MPI_Comm): Int
    def MPI_Exscan(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    def MPI_Gather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, root: Int, comm: MPI_Comm): Int
    def MPI_Gatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, root: Int,
      comm: MPI_Comm): Int
    def MPI_Iallgather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Iallgatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, comm: MPI_Comm,
      request: Poitner[MPI_Request]): Int
    def MPI_Iallreduce(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Ialltoall(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ialltoallv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcounts: Pointer[Int], rdispls: Pointer[Int], recvtype: MPI_Datatype,
      comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ialltoallw(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtypes: Pointer[MPI_Datatype],
      recvbuf: Pointer[_], recvcounts: Pointer[Int], rdispls: Pointer[Int],
      recvtypes: Pointer[MPI_Datatype], comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Ibarrier(comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ibcast(buffer: Pointer[_], count: Int, datatype: MPI_Datatype,
      root: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Iexscan(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Igather(sendbuf: Pointer[_], sendcount: Int, sendtype: MPI_Datatype,
      recvbuf: Pointer[_], recvcount: Int, recvtype: MPI_Datatype, root: Int,
      comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Igatherv(sendbuf: Pointer[_], sendcount: Int, sendtype: MPI_Datatype,
      recvbuf: Pointer[_], recvcounts: Pointer[Int], displs: Pointer[Int],
      recvtype: MPI_Datatype, root: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Ireduce(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, root: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Ireduce_scatter(sendbuf: Pointer[_], recvbuf: Pointer[_],
      recvcounts: Pointer[Int], datatype: MPI_Datatype, op: MPI_Op op,
      comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ireduce_scatter_block(sendbuf: Pointer[_], recvbuf: Pointer[_],
      recvcount: Int, datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Iscan(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Iscatter(sendbuf: Pointer[_], sendcount: Int, sendtype: MPI_Datatype,
      recvbuf: Pointer[_], recvcount: Int, recvtype: MPI_Datatype, root: Int,
      comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Iscatterv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      displs: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcount: Int, recvtype: MPI_Datatype, root: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Op_commutative(op: MPI_Op, commute: Pointer[Int]): Int
    def MPI_Op_create(function: Pointer[MPI_User_function],
      commute: Int, op: Pointer[MPI_Op]): Int
    def MPI_Op_free(op: Pointer[MPI_Op]): Int
    def MPI_Reduce(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, root: Int, comm: MPI_Comm): Int
    def MPI_Reduce_local(inbuf: Pointer[_], inoutbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op): Int
    def MPI_Reduce_scatter(sendbuf: Pointer[_], recvbuf: Pointer[_],
      recvcounts: Pointer[Int], datatype: MPI_Datatype, op: MPI_Op,
      comm: MPI_Comm): Int
    def MPI_Reduce_scatter_block(sendbuf: Pointer[_], recvbuf: Pointer[_],
      recvcount: Int, datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    def MPI_Scan(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    def MPI_Scatter(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, root: Int, comm: MPI_Comm): Int
    def MPI_Scatterv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      displs: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcount: Int, recvtype: MPI_Datatype, root: Int, comm: MPI_Comm): Int

    // A.2.4 Groups, Contexts, Communicators, and Caching C Bindings

    def MPI_Comm_compare(comm1: MPI_Comm, comm2: MPI_Comm,
      result: Pointer[Int]): Int
    def MPI_Comm_create(comm: MPI_Comm, group: MPI_Group,
      newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_create_group(comm: MPI_Comm, group: MPI_Group, tag: Int,
      newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_create_keyval(
      comm_copy_attr_fn: Pointer[MPI_Comm_copy_attr_function],
      comm_delete_attr_fn: Pointer[MPI_Comm_delete_attr_function],
      comm_keyval: Pointer[Int], extra_state: Pointer[_]): Int
    def MPI_Comm_delete_attr(comm: MPI_Comm, comm_keyval: Int): Int
    def MPI_Comm_dup(comm: MPI_Comm, newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_dup_with_info(comm: MPI_Comm, info: MPI_Info,
      newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_free(comm: Pointer[MPI_Comm]): Int
    def MPI_Comm_free_keyval(comm_keyval: Pointer[Int]): Int
    def MPI_Comm_get_attr(comm: MPI_Comm, comm_keyval: Int,
      attribute_val: Pointer[Pointer[_]], flag: Pointer[Int]): Int
    def MPI_Comm_get_info(comm: MPI_Comm comm, info_used: Pointer[MPI_Info]): Int
    def MPI_Comm_get_name(comm: MPI_Comm, comm_name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    def MPI_Comm_group(comm: MPI_Comm, group: Pointer[MPI_Group]): Int
    def MPI_Comm_idup(comm: MPI_Comm, newcomm: Pointer[MPI_Comm],
      request: Pointer[MPI_Request]): Int
    def MPI_Comm_rank(comm: MPI_Comm, rank: Pointer[Int]): Int
    def MPI_Comm_remote_group(comm: MPI_Comm,
      group: Pointer[MPI_Group]): Int
    def MPI_Comm_remote_size(comm: MPI_Comm, size: Pointer[Int]): Int
    def MPI_Comm_set_attr(comm: MPI_Comm, comm_keyval: Int,
      attribute_val: Pointer[_]): Int
    def MPI_Comm_set_info(comm: MPI_Comm, info: MPI_Info): Int
    def MPI_Comm_set_name(comm: MPI_Comm, comm_name: Pointer[Byte]): Int
    def MPI_Comm_size(comm: MPI_Comm, size: Pointer[Int]): Int
    def MPI_Comm_split(comm: MPI_Comm, color: Int, key: Int,
      newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_split_type(comm: MPI_Comm, split_type: Int, key: Int,
      info: MPI_Info, newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_test_inter(comm: MPI_Comm, flag: Pointer[Int]): Int
    def MPI_Group_compare(group1: MPI_Group, group2: MPI_Group,
      result: Pointer[Int]): Int
    def MPI_Group_difference(group1: MPI_Group, group2: MPI_Group,
      newgroup: Pointer[MPI_Group]): Int
    def MPI_Group_excl(group: MPI_Group, n: Int, ranks: Pointer[Int],
      newgroup: Pointer[MPI_Group]): Int
    def MPI_Group_free(group: Pointer[MPI_Group]): Int
    def MPI_Group_incl(group: MPI_Group, n: Int, ranks: Pointer[Int],
      newgroup: Pointer[MPI_Group]): Int
    def MPI_Group_intersection(group1: MPI_Group, group2: MPI_Group,
      newgroup: Pointer[MPI_Group]): Int
    def MPI_Group_range_excl(group: MPI_Group, n: Int,
      ranges: Pointer[Int], newgroup: Pointer[MPI_Group]): Int
    def MPI_Group_range_incl(group: MPI_Group, n: Int,
      ranges: Pointer[Int], newgroup: Pointer[MPI_Group]): Int
    def MPI_Group_rank(group: MPI_Group, rank: Pointer[Int]): Int
    def MPI_Group_size(group: MPI_Group, size: Pointer[Int]): Int
    def MPI_Group_translate_ranks(group1: MPI_Group, n: Int,
      ranks1: Pointer[Int], group2: MPI_Group, ranks2: Pointer[Int]): Int
    def MPI_Group_union(group1: MPI_Group, group2: MPI_Group,
      newgroup: Pointer[MPI_Group]): Int
    def MPI_Intercomm_create(local_comm: MPI_Comm, local_leader: Int,
      peer_comm: MPI_Comm, remote_leader: Int, tag: Int,
      newintercomm: Pointer[MPI_Comm]): Int
    def MPI_Intercomm_merge(intercomm: MPI_Comm, high: Int,
      newintracomm: Pointer[MPI_Comm]): Int
    def MPI_Type_create_keyval(
      type_copy_attr_fn: Pointer[MPI_Type_copy_attr_function],
      type_delete_attr_fn: Pointer[MPI_Type_delete_attr_function],
      type_keyval: Pointer[Int], extra_state: Pointer[_]): Int
    def MPI_Type_delete_attr(datatype: MPI_Datatype, type_keyval: Int): Int
    def MPI_Type_free_keyval(type_keyval: Pointer[Int]): Int
    def MPI_Type_get_attr(datatype: MPI_Datatype, type_keyval: Int,
      attribute_val: Pointer[Pointer[_]], flag: Pointer[Int]): Int
    def MPI_Type_get_name(datatype: MPI_Datatype, type_name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    def MPI_Type_set_attr(datatype: MPI_Datatype, type_keyval: Int,
      attribute_val: Pointer[_]): Int
    def MPI_Type_set_name(datatype: MPI_Datatype, type_name: Pointer[Byte]): Int
    def MPI_Win_create_keyval(
      win_copy_attr_fn: Pointer[MPI_Win_copy_attr_function],
      win_delete_attr_fn: Pointer[MPI_Win_delete_attr_function],
      win_keyval: Pointer[Int], extra_state: Pointer[_]) :Int
    def MPI_Win_delete_attr(win: MPI_Win, win_keyval: Int): Int
    def MPI_Win_free_keyval(win_keyval: Pointer[Int]): Int
    def MPI_Win_get_attr(win: MPI_Win, win_keyval: Int,
      attribute_val: Pointer[Pointer[_]], flag: Pointer[Int]): Int
    def MPI_Win_get_info(win: MPI_Win, info_used: Pointer[MPI_Info]): Int
    def MPI_Win_get_name(win: MPI_Win, win_name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    def MPI_Win_set_attr(win: MPI_Win, win_keyval: Int,
      attribute_val: Pointer[_]): Int
    def MPI_Win_set_info(win: MPI_Win, info: MPI_Info): Int
    def MPI_Win_set_name(win: MPI_Win, win_name: Pointer[Byte]): Int

    // A.2.5 Process Topologies C Bindings

    def MPI_Cart_coords(comm: MPI_Comm, rank: Int, maxdims: Int,
      coords: Pointer[Int]): Int
    def MPI_Cart_create(comm_old: MPI_Comm, ndims: Int, dims: Pointer[Int],
      periods: Pointer[Int], reorder: Int, comm_cart: Pointer[MPI_Comm]): Int
    def MPI_Cart_get(comm: MPI_Comm, maxdims: Int, dims: Pointer[Int],
      periods: Pointer[Int], coords: Pointer[Int]): Int
    def MPI_Cart_map(comm: MPI_Comm, ndims: Int, dims: Pointer[Int],
      periods: Pointer[Int], newrank: Pointer[Int]): Int
    def MPI_Cart_rank(comm: MPI_Comm, coords: Pointer[Int],
      rank: Pointer[Int]): Int
    def MPI_Cart_shift(comm: MPI_Comm, direction: Int, disp: Int,
      rank_source: Pointer[Int], rank_dest: Pointer[Int]): Int
    def MPI_Cart_sub(comm: MPI_Comm, remain_dims: Pointer[Int],
      newcomm: Pointer[MPI_Comm]): Int
    def MPI_Cartdim_get(comm: MPI_Comm, ndims: Pointer[Int]): Int
    def MPI_Dims_create(nnodes: Int, ndims: Int, dims: Pointer[Int]): Int
    def MPI_Dist_graph_create(comm_old: MPI_Comm, n: Int, sources: Pointer[Int],
      degrees: Pointer[Int], destinations: Pointer[Int], weights: Pointer[Int],
      info: MPI_Info, reorder: Int, comm_dist_graph: Pointer[MPI_Comm]): Int
    def MPI_Dist_graph_create_adjacent(comm_old: MPI_Comm, indegree: Int,
      sources: Pointer[Int], sourceweights: Pointer[Int], outdegree: Int,
      destinations: Pointer[Int], destweights: Pointer[Int], info: MPI_Info,
      reorder: Int, comm_dist_graph: Pointer[MPI_Comm]): Int
    def MPI_Dist_graph_neighbors(comm: MPI_Comm, maxindegree: Int,
      sources: Pointer[Int], sourceweights: Pointer[Int], maxoutdegree: Int,
      destinations: Pointer[Int], destweights: Pointer[Int]): Int
    def MPI_Dist_graph_neighbors_count(comm: MPI_Comm, indegree: Pointer[Int],
      outdegree: Pointer[Int], weighted: Pointer[Int]): Int
    def MPI_Graph_create(comm_old: MPI_Comm, nnodes: Int, index: Pointer[Int],
      edges: Pointer[Int], reorder: Int, comm_graph: Pointer[MPI_Comm]): Int
    def MPI_Graph_get(comm: MPI_Comm, maxindex: Int, maxedges: Int,
      index: Pointer[Int], edges: Pointer[Int]): Int
    def MPI_Graph_map(comm: MPI_Comm, nnodes: Int, index: Pointer[Int],
      edges: Pointer[Int], newrank: Pointer[Int]): Int
    def MPI_Graph_neighbors(comm: MPI_Comm, rank: Int, maxneighbors: Int,
      neighbors: Pointer[Int]): Int
    def MPI_Graph_neighbors_count(comm: MPI_Comm, rank: Int,
      nneighbors: Pointer[Int]): Int
    def MPI_Graphdims_get(comm: MPI_Comm, nnodes: Pointer[Int],
      nedges: Pointer[Int]): Int
    def MPI_Ineighbor_allgather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ineighbor_allgatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Ineighbor_alltoall(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ineighbor_alltoallv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcounts: Pointer[Int], rdispls: Pointer[Int], recvtype: MPI_Datatype,
      comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    def MPI_Ineighbor_alltoallw(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[MPI_Aint], sendtypes: Pointer[MPI_Datatype],
      recvbuf: Pointer[_], recvcounts: Pointer[Int], rdispls: Pointer[MPI_Aint],
      recvtypes: Pointer[MPI_Datatype], comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    def MPI_Neighbor_allgather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Neighbor_allgatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Neighbor_alltoall(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm): Int
    def MPI_Neighbor_alltoallv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcounts: Pointer[Int], rdispls: Pointer[Int], recvtype: MPI_Datatype,
      comm: MPI_Comm): Int
    def MPI_Neighbor_alltoallw(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[MPI_Aint], sendtypes: Pointer[MPI_Datatype],
      recvbuf: Pointer[_], recvcounts: Pointer[Int], rdispls: Pointer[MPI_Aint],
      recvtypes: Pointer[MPI_Datatype], comm: MPI_Comm): Int
    def MPI_Topo_test(comm: MPI_Comm, status: Pointer[Int]): Int

    // A.2.6 MPI Environmenta Management C Bindings

    def MPI_Abort(comm: MPI_Comm, errorcode: Int): Int
    def MPI_Add_error_class(errorclass: Pointer[Int]): Int
    def MPI_Add_error_code(errorclass: Int,
      errorcode: Pointer[Int]): Int
    def MPI_Add_error_string(errorcode: Int, string: Pointer[Byte]): Int
    def MPI_Alloc_mem(size: MPI_Aint, info: MPI_Info, baseptr: Pointer[_]): Int
    def MPI_Comm_call_errhandler(comm: MPI_Comm, errorcode: Int): Int
    def MPI_Comm_create_errhandler(
      function: Pointer[MPI_Comm_errhandler_function],
      errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_Comm_get_errhandler(comm: MPI_Comm,
      errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_Comm_set_errhandler(comm: MPI_Comm, errhandler: MPI_Errhandler): Int
    def MPI_Errhandler_free(errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_Error_class(errorcode: Int, errorclass: Pointer[Int]): Int
    def MPI_Error_string(errorcode: Int, string: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    def MPI_File_call_errhandler(fh: MPI_File, errorcode: Int): Int
    def MPI_File_create_errhandler(
      function: Pointer[MPI_File_errhandler_function],
      errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_File_get_errhandler(file: MPI_File,
      errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_File_set_errhandler(file: MPI_File, errhandler: MPI_Errhandler): Int
    def MPI_Finalize(): Int
    def MPI_Finalized(flag: Pointer[Int]): Int
    def MPI_Free_mem(base: Pointer[_]): Int
    def MPI_Get_processor_name(name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    def MPI_Get_version(version: Pointer[Int], subversion: Pointer[Int]): Int
    def MPI_Get_library_version(version: Pointer[Byte], resultlen: Pointer[Int]):
        Int
    def MPI_Init(argc: Pointer[Int], argv: Pointer[Pointer[Pointer[Byte]]]): Int
    def MPI_Initialized(flag: Pointer[Int]): Int
    def MPI_Win_call_errhandler(win: MPI_Win, errorcode: Int): Int
    def MPI_Win_create_errhandler(
      function: Pointer[MPI_Win_errhandler_function],
      errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_Win_get_errhandler(win: MPI_Win,
      errhandler: Pointer[MPI_Errhandler]): Int
    def MPI_Win_set_errhandler(win: MPI_Win, errhandler: MPI_Errhandler): Int
    def MPI_Wtick(): Double
    def MPI_Wtime(): Double

    // A.2.7 The Info Object C Bindings

    def MPI_Info_create(info: Pointer[MPI_Info]): Int
    def MPI_Info_delete(info: MPI_Info, key: Pointer[Byte]): Int
    def MPI_Info_dup(info: MPI_Info, newinfo: Pointer[MPI_Info]): Int
    def MPI_Info_free(info: Pointer[MPI_Info]): Int
    def MPI_Info_get(info: MPI_Info, key: Pointer[Byte], valuelen: Int,
      value: Pointer[Byte], flag: Pointer[Int]): Int
    def MPI_Info_get_nkeys(info: MPI_Info, nkeys: Pointer[Int]): Int
    def MPI_Info_get_nthkey(info: MPI_Info, n: Int, key: Pointer[Byte]): Int
    def MPI_Info_get_valuelen(info: MPI_Info, key: Pointer[Byte],
      valuelen: Pointer[Int], flag: Pointer[Int]): Int
    def MPI_Info_set(info: MPI_Info, key: Pointer[Byte],
      value: Pointer[Byte]): Int

    // A.2.8 Process Creation and Management C Bindings

    def MPI_Close_port(port_name: Pointer[Byte]): Int
    def MPI_Comm_accept(port_name: Pointer[Byte], info: MPI_Info, root: Int,
      comm: MPI_Comm, newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_connect(port_name: Pointer[Byte], info: MPI_Info, root: Int,
      comm: MPI_Comm, newcomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_disconnect(comm: Pointer[MPI_Comm]): Int
    def MPI_Comm_get_parent(parent: Pointer[MPI_Comm]): Int
    def MPI_Comm_join(fd: Int, intercomm: Pointer[MPI_Comm]): Int
    def MPI_Comm_spawn(command: Pointer[Byte], argv: Pointer[Pointer[Byte]],
      maxprocs: Int, info: MPI_Info, root: Int, comm: MPI_Comm,
      intercomm: Pointer[MPI_Comm], array_of_errcodes: Pointer[Int]): Int
    def MPI_Comm_spawn_multiple(count: Int,
      array_of_commands: Pointer[Pointer[Byte]],
      array_of_argv: Pointer[Pointer[Pointer[Byte]]],
      array_of_maxprocs: Pointer[Int], array_of_info: Pointer[MPI_Info],
      root: Int, comm: MPI_Comm, intercomm: Pointer[MPI_Comm],
      array_of_errcodes: Pointer[Int]): Int
    def MPI_Lookup_name(service_name: Pointer[Byte], info: MPI_Info,
      port_name: Pointer[Byte]): Int
    def MPI_Open_port(info: MPI_Info, port_name: Pointer[Byte]): Int
    def MPI_Publish_name(service_name: Pointer[Byte], info: MPI_Info,
      port_name: Pointer[Byte]): Int
    def MPI_Unpublish_name(service_name: Pointer[Byte], info: MPI_Info,
      port_name: Pointer[Byte]): Int

    // A.2.9 One-Sided Communications C Bindings

    def MPI_Accumulate(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, op: MPI_Op,
      win: MPI_Win): Int
    def MPI_Compare_and_swap(origin_addr: Pointer[_], compare_addr: Pointer[_],
      result_addr: Pointer[_], datatype: MPI_Datatype, target_rank: Int,
      target_disp: MPI_Aint, win: MPI_Win): Int
    def MPI_Fetch_and_op(origin_addr: Pointer[_], result_addr: Pointer[_],
      datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      op: MPI_Op, win: MPI_Win): Int
    def MPI_Get(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, win: MPI_Win): Int
    def MPI_Get_accumulate(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, result_addr: Pointer[_], result_count: Int,
      result_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, op: MPI_Op,
      win: MPI_Win): Int
    def MPI_Put(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, win: MPI_Win): Int
    def MPI_Raccumulate(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, op: MPI_Op, win: MPI_Win,
      request: Pointer[MPI_Request]): Int
    def MPI_Rget(origin_addr: Pointer, origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, win: MPI_Win,
      request: Pointer[MPI_Request]): Int
    def MPI_Rget_accumulate(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, result_addr: Pointer[_], result_count: Int,
      result_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, op: MPI_Op, win: MPI_Win,
      request: Pointer[MPI_Request]): Int
    def MPI_Rput(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, win: MPI_Win,
      request: Pointer[MPI_Request]): Int
    def MPI_Win_allocate(size: MPI_Aint, disp_unit: Int, info:  MPI_Info,
      comm: MPI_Comm, baseptr: Pointer[_], win: Pointer[MPI_Win]): Int
    def MPI_Win_allocate_shared(size: MPI_Aint, disp_unit: Int, info:  MPI_Info,
      comm: MPI_Comm, baseptr: Pointer[_], win: Pointer[MPI_Win]): Int
    def MPI_Win_attach(win: MPI_Win, base: Pointer[_], size: MPI_Aint): Int
    def MPI_Win_complete(win: MPI_Win): Int
    def MPI_Win_create(base: Pointer[_], size: MPI_Aint, disp_unit: Int,
      info: MPI_Info, comm: MPI_Comm, win: Pointer[MPI_Win]): Int
    def MPI_Win_create_dynamic(info: MPI_Info, comm: MPI_Comm,
      win: Pointer[MPI_Win]): Int
    def MPI_Win_detach(win: MPI_Win, base: Pointer[_]): Int
    def MPI_Win_fence(assert: Int, win: MPI_Win): Int
    def MPI_Win_flush(rank: Int, win: MPI_Win): Int
    def MPI_Win_flush_all(win: MPI_Win): Int
    def MPI_Win_flush_local(rank: Int, win: MPI_Win): Int
    def MPI_Win_flush_local_all(win: MPI_Win): Int
    def MPI_Win_free(win: Pointer[MPI_Win]): Int
    def MPI_Win_get_group(win: MPI_Win, group: Pointer[MPI_Group]): Int
    def MPI_Win_get_info(win: MPI_Win, info_used: Pointer[MPI_Info]): Int
    def MPI_Win_lock(lock_type: Int, rank: Int, assert: Int, win: MPI_Win): Int
    def MPI_Win_lock_all(assert: Int, win: MPI_Win): Int
    def MPI_Win_post(group: MPI_Group, assert: Int, win: MPI_Win): Int
    def MPI_Win_set_info(win: MPI_Win, info: MPI_Info): Int
    def MPI_Win_shared_query(win: MPI_Win, rank: Int, size: Pointer[MPI_Aint],
      disp_unit: Pointer[Int], baseptr: Pointer[_]): Int
    def MPI_Win_start(group: MPI_Group, assert: Int, win: MPI_Win): Int
    def MPI_Win_sync(win: MPI_Win): Int
    def MPI_Win_test(win: MPI_Win, flag: Pointer[Int]): Int
    def MPI_Win_unlock(rank: Int, win: MPI_Win): Int
    def MPI_Win_unlock_all(win: MPI_Win): Int
    def MPI_Win_wait(win: MPI_Win): Int

    // A.2.10 External Interfaces C Bindings

    def MPI_Grequest_complete(request: MPI_Request): Int
    def MPI_Grequest_start(query_fn: Pointer[MPI_Grequest_query_function],
      free_fn: Pointer[MPI_Grequest_free_function],
      cancel_fn: Pointer[MPI_Grequest_cancel_function],
      extra_state: Pointer[_], request: Pointer[MPI_Request]): Int
    def MPI_Init_thread(argc: Pointer[Int],
      argv: Pointer[Pointer[Pointer[Byte]]], required: Int,
      provided: Pointer[Int]): Int
    def MPI_Is_thread_main(flag: Pointer[Int]): Int
    def MPI_Query_thread(provided: Pointer[Int]): Int
    def MPI_Status_set_cancelled(status: Pointer[MPI_Status],
      flag: Int): Int
    def MPI_Status_set_elements(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Int): Int
    def MPI_Status_set_elements_x(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: MPI_Count): Int

    // A.2.11 I/O C Bindings

    def MPI_File_close(fh: Pointer[MPI_File]): Int
    def MPI_File_delete(filename: Pointer[Byte], info: MPI_Info): Int
    def MPI_File_get_amode(fh: MPI_File, amode: Pointer[Int]): Int
    def MPI_File_get_atomicity(fh: MPI_File, flag: Pointer[Int]): Int
    def MPI_File_get_byte_offset(fh: MPI_File, offset: MPI_Offset,
      disp: Pointer[MPI_Offset]): Int
    def MPI_File_get_group(fh: MPI_File, group: Pointer[MPI_Group]): Int
    def MPI_File_get_info(fh: MPI_File, info_used: Pointer[MPI_Info]): Int
    def MPI_File_get_position(fh: MPI_File, offset: Pointer[MPI_Offset]): Int
    def MPI_File_get_position_shared(fh: MPI_File,
      offset: Pointer[MPI_Offset]): Int
    def MPI_File_get_size(fh: MPI_File, size: Pointer[MPI_Offset]): Int
    def MPI_File_get_type_extent(fh: MPI_File, datatype: MPI_Datatype,
      extent: Pointer[MPI_Aint]): Int
    def MPI_File_get_view(fh: MPI_File, disp: Pointer[MPI_Offset],
      etype: Pointer[MPI_Datatype], filetype: Pointer[MPI_Datatype],
      datarep: Pointer[Byte]): Int
    def MPI_File_iread(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    def MPI_File_iread_at(fh: MPI_File, offset: MPI_Offset, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    def MPI_File_iread_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    def MPI_File_iwrite(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    def MPI_File_iwrite_at(fh: MPI_File, offset: MPI_Offset, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    def MPI_File_iwrite_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    def MPI_File_open(comm: MPI_Comm, filename: Pointer[Byte], amode: Int,
      info: MPI_Info, fh: Pointer[MPI_File]): Int
    def MPI_File_preallocate(fh: MPI_File, size: MPI_Offset): Int
    def MPI_File_read(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_read_all(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_read_all_begin(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype): Int
    def MPI_File_read_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    def MPI_File_read_at(fh: MPI_File, offset: MPI_Offset, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_read_at_all(fh: MPI_File, offset: MPI_Offset, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_read_at_all_begin(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype): Int
    def MPI_File_read_at_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    def MPI_File_read_ordered(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_read_ordered_begin(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype): Int
    def MPI_File_read_ordered_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    def MPI_File_read_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_seek(fh: MPI_File, offset: MPI_Offset, whence: Int): Int
    def MPI_File_seek_shared(fh: MPI_File, offset: MPI_Offset, whence: Int): Int
    def MPI_File_set_atomicity(fh: MPI_File, flag: Int): Int
    def MPI_File_set_info(fh: MPI_File, info: MPI_Info): Int
    def MPI_File_set_size(fh: MPI_File, size: MPI_Offset): Int
    def MPI_File_set_view(fh: MPI_File, disp: MPI_Offset, etype: MPI_Datatype,
      filetype: MPI_Datatype, datarep: Pointer[Byte], info: MPI_Info): Int
    def MPI_File_sync(fh: MPI_File): Int
    def MPI_File_write(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_write_all(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_write_all_begin(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype): Int
    def MPI_File_write_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    def MPI_File_write_at(fh: MPI_File, offset: MPI_Offset, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_write_at_all(fh: MPI_File, offset: MPI_Offset, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_write_at_all_begin(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype): Int
    def MPI_File_write_at_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    def MPI_File_write_ordered(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_File_write_ordered_begin(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype): Int
    def MPI_File_write_ordered_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    def MPI_File_write_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    def MPI_Register_datarep(datarep: Pointer[Byte],
      read_conversion_fn: Pointer[MPI_Datarep_conversion_function],
      write_conversion_fn: Pointer[MPI_Datarep_conversion_function],
      dtype_file_extent_fn: Pointer[MPI_Datarep_extent_function],
      extra_state: Pointer[_]): Int
  }
}
