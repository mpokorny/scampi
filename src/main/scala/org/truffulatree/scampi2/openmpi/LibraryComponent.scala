//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2.openmpi

import org.truffulatree.scampi2._
import org.bridj.{BridJ, Pointer, TypedPointer, CRuntime, StructObject,
  CLong, Callback}
import org.bridj.ann.{Field, Library, Runtime}
import java.nio.ByteBuffer

trait LibraryComponent
    extends Mpi2LibraryComponent {

  val aintByteSize = AlignHelper.cLongByteSize

  val aintAlignment = AlignHelper.cLongAlignment

  val offsetByteSize = AlignHelper.longByteSize

  val offsetAlignment = AlignHelper.longAlignment

  def allocateDatatype(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Datatype], n)

  def allocateComm(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Comm], n)

  def allocateGroup(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Group], n)

  def allocateWin(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Win], n)

  def allocateFile(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_File], n)

  def allocateOp(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Op], n)

  def allocateErrhandler(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Errhandler], n)

  def allocateRequest(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Request], n)

  def allocateInfo(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_Info], n)

  def allocateAint(n: Int = 1) = Pointer.allocateCLongs(n)

  def allocateOffset(n: Int = 1) = Pointer.allocateLongs(n).as(classOf[Long])

  //def associateStatus(peer: Pointer[lib.MPI_Status]) = lib.MpiStatus(peer)

  def newStatus(n: Int = 1) = lib.MpiStatus(n)

  protected def _aintToLong(aint: lib.MPI_Aint): Long = aint.longValue

  protected def _aintFromLong(long: Long): lib.MPI_Aint = CLong.valueOf(long)

  protected def _offsetToLong(offset: lib.MPI_Offset): Long = offset

  protected def _offsetFromLong(long: Long): lib.MPI_Offset = long

  @Library("mpi")
  @Runtime(classOf[CRuntime])
  final object lib extends Mpi2Library {
    BridJ.register()

    private lazy val nativeLibrary = {
      val result = BridJ.getNativeLibrary(this.getClass)
      val file = BridJ.getNativeLibraryFile(BridJ.getNativeLibraryName(this.getClass))
      println(s"${file}")
      result
    }

    private def getSymbolPointer(name: String): Pointer[_] =
      nativeLibrary.getSymbolPointer(name)

    type MPI_Datatype = TypedPointers.MPI_Datatype
    type MPI_Comm = TypedPointers.MPI_Comm
    type MPI_Group = TypedPointers.MPI_Group
    type MPI_Win = TypedPointers.MPI_Win
    type MPI_File = TypedPointers.MPI_File
    type MPI_Op = TypedPointers.MPI_Op
    final class MPI_Status(peer: Pointer[MPI_Status])
        extends StructObject(peer) with CStatus {
      val sizeOf = typeInfo.sizeOf
      @Field(0)
      def MPI_SOURCE: Int = io.getIntField(this, 0)
      @Field(0)
      def MPI_SOURCE_=(x: Int) {
	io.setIntField(this, 0, x)
      }
      @Field(1)
      def MPI_TAG: Int = io.getIntField(this, 1)
      @Field(1)
      def MPI_TAG_=(x: Int) {
	io.setIntField(this, 1, x)
      }
      @Field(2)
      def MPI_ERROR: Int = io.getIntField(this, 2)
      @Field(2)
      def MPI_ERROR_=(x: Int) {
	io.setIntField(this, 2, x)
      }
      @Field(3)
      private def cancelled = io.getIntField(this, 3)
      @Field(3)
      private def cancelled_=(x: Int) {
	io.setIntField(this, 3, x)
      }
      @Field(4)
      private def ucount = io.getCLongField(this, 4)
      @Field(4)
      private def ucount_=(x: Long) {
	io.setCLongField(this, 4, x)
      }
      override def toString =
        s"MPI_Status(${MPI_SOURCE},${MPI_TAG},${MPI_ERROR})"
    }
    object MpiStatus {
      def apply(peer: Pointer[MPI_Status]) = new MPI_Status(peer)
      def apply(n: Int) = {
        require(n > 0, "Number of requested instances is not at least one")
        val structs = Pointer.allocateBytes(n * sizeOf).as(classOf[MPI_Status])
        (0 until n).map(i => new MPI_Status(structs.next(i)))
      }
      val sizeOf = (new MPI_Status(MPI_STATUS_IGNORE)).sizeOf
    }
    type MPI_Request = TypedPointers.MPI_Request
    type MPI_Errhandler = TypedPointers.MPI_Errhandler
    type MPI_Info = TypedPointers.MPI_Info
    type MPI_Aint = CLong
    type MPI_Offset = Long

    // Values
    val MPI_IDENT = 0
    val MPI_CONGRUENT = 1
    val MPI_SIMILAR = 2
    val MPI_UNEQUAL = 3

    lazy val MPI_CHAR =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_char"))
    lazy val MPI_SIGNED_CHAR =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_signed_char"))
    lazy val MPI_UNSIGNED_CHAR =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_unsigned_char"))
    lazy val MPI_BYTE =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_byte"))
    lazy val MPI_WCHAR =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_wchar"))
    lazy val MPI_SHORT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_short"))
    lazy val MPI_UNSIGNED_SHORT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_unsigned_short"))
    lazy val MPI_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_int"))
    lazy val MPI_UNSIGNED =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_unsigned"))
    lazy val MPI_LONG =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_long"))
    lazy val MPI_UNSIGNED_LONG =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_unsigned_long"))
    lazy val MPI_FLOAT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_float"))
    lazy val MPI_DOUBLE =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_double"))
    lazy val MPI_LONG_DOUBLE =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_long_double"))
    lazy val MPI_LONG_LONG_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_long_long_int"))
    lazy val MPI_UNSIGNED_LONG_LONG =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_unsigned_long_long"))
    lazy val MPI_LONG_LONG =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_long_long"))
    lazy val MPI_PACKED =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_packed"))
    lazy val MPI_LB =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_lb"))
    lazy val MPI_UB =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_ub"))
    lazy val MPI_FLOAT_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_float_int"))
    lazy val MPI_DOUBLE_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_double_int"))
    lazy val MPI_LONG_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_long_int"))
    lazy val MPI_SHORT_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_short_int"))
    lazy val MPI_2INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_2int"))
    lazy val MPI_LONG_DOUBLE_INT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_longdbl_int"))
    lazy val MPI_INT8_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_int8_t"))
    lazy val MPI_INT16_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_int16_t"))
    lazy val MPI_INT32_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_int32_t"))
    lazy val MPI_INT64_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_int64_t"))
    lazy val MPI_UINT8_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_uint8_t"))
    lazy val MPI_UINT16_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_uint16_t"))
    lazy val MPI_UINT32_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_uint32_t"))
    lazy val MPI_UINT64_T =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_uint64_t"))
    lazy val MPI_C_BOOL =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_c_bool"))
    lazy val MPI_C_FLOAT_COMPLEX =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_c_float_complex"))
    lazy val MPI_C_COMPLEX =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_c_complex"))
    lazy val MPI_C_DOUBLE_COMPLEX =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_c_double_complex"))
    lazy val MPI_C_LONG_DOUBLE_COMPLEX =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_c_long_double_complex"))
    lazy val MPI_AINT =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_aint"))
    lazy val MPI_OFFSET =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_offset"))

    lazy val MPI_COMM_WORLD =
      new TypedPointers.MPI_Comm(getSymbolPointer("ompi_mpi_comm_world"))
    lazy val MPI_COMM_SELF =
      new TypedPointers.MPI_Comm(getSymbolPointer("ompi_mpi_comm_self"))

    lazy val MPI_GROUP_EMPTY =
      new TypedPointers.MPI_Group(getSymbolPointer("ompi_mpi_group_empty"))

    lazy val MPI_WIN_NULL =
      new TypedPointers.MPI_Win(getSymbolPointer("ompi_mpi_win_null"))

    lazy val MPI_FILE_NULL =
      new TypedPointers.MPI_File(getSymbolPointer("ompi_mpi_file_null"))

    lazy val MPI_MAX =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_max"))
    lazy val MPI_MIN =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_min"))
    lazy val MPI_SUM =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_sum"))
    lazy val MPI_PROD =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_prod"))
    lazy val MPI_LAND =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_land"))
    lazy val MPI_BAND =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_band"))
    lazy val MPI_LOR =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_lor"))
    lazy val MPI_BOR =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_bor"))
    lazy val MPI_LXOR =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_lxor"))
    lazy val MPI_BXOR =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_bxor"))
    lazy val MPI_MINLOC =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_minloc"))
    lazy val MPI_MAXLOC =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_maxloc"))
    lazy val MPI_REPLACE =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_replace"))

    val MPI_TAG_UB = 0
    val MPI_HOST = 1
    val MPI_IO = 2
    val MPI_WTIME_IS_GLOBAL = 3
    val MPI_UNIVERSE_SIZE = 6
    val MPI_LASTUSEDCODE = 5
    val MPI_APPNUM = 4
    val MPI_WIN_BASE = 7
    val MPI_WIN_SIZE = 8
    val MPI_WIN_DISP_UNIT = 9

    lazy val MPI_COMM_NULL =
      new TypedPointers.MPI_Comm(getSymbolPointer("ompi_mpi_comm_null"))
    lazy val MPI_OP_NULL =
      new TypedPointers.MPI_Op(getSymbolPointer("ompi_mpi_op_null"))
    lazy val MPI_GROUP_NULL =
      new TypedPointers.MPI_Group(getSymbolPointer("ompi_mpi_group_null"))
    lazy val MPI_DATATYPE_NULL =
      new TypedPointers.MPI_Datatype(getSymbolPointer("ompi_mpi_datatype_null"))
    lazy val MPI_REQUEST_NULL =
      new TypedPointers.MPI_Request(getSymbolPointer("ompi_mpi_request_null"))
    lazy val MPI_ERRHANDLER_NULL =
      new TypedPointers.MPI_Errhandler(getSymbolPointer("ompi_mpi_errhandler_null"))

    val MPI_MAX_DATAREP_STRING = 128
    val MPI_MAX_PROCESSOR_NAME = 256
    val MPI_MAX_ERROR_STRING = 256
    val MPI_MAX_PORT_NAME = 1024
    val MPI_MAX_OBJECT_NAME = 64
    val MPI_UNDEFINED = -32766
    val MPI_KEYVAL_INVALID = -1
    val MPI_BSEND_OVERHEAD = 128

    val MPI_BOTTOM = Pointer.pointerToAddress(0, 0, noRelease)
    lazy val MPI_UNWEIGHTED = ???

    val MPI_PROC_NULL = -2
    val MPI_ANY_SOURCE = -1
    val MPI_ROOT = -4
    val MPI_ANY_TAG = -1

    val MPI_LOCK_EXCLUSIVE = 1
    val MPI_LOCK_SHARED = 2

    val MPI_ERRORS_ARE_FATAL =
      new TypedPointers.MPI_Errhandler(getSymbolPointer("ompi_mpi_errors_are_fatal"))
    val MPI_ERRORS_RETURN =
      new TypedPointers.MPI_Errhandler(getSymbolPointer("ompi_mpi_errors_return"))

    lazy val MPI_INFO_NULL =
      new TypedPointers.MPI_Info(getSymbolPointer("ompi_mpi_info_null"))
    val MPI_MAX_INFO_KEY = 36
    val MPI_MAX_INFO_VAL = 256

    val MPI_ORDER_C = 0
    val MPI_ORDER_FORTRAN = 1
    val MPI_DISTRIBUTE_BLOCK = 0
    val MPI_DISTRIBUTE_CYCLIC = 1
    val MPI_DISTRIBUTE_NONE = 2
    val MPI_DISTRIBUTE_DFLT_DARG = -1

    val MPI_IN_PLACE = Pointer.pointerToAddress(1, 0, noRelease)

    val MPI_MODE_APPEND = 128
    val MPI_MODE_CREATE = 1
    val MPI_MODE_DELETE_ON_CLOSE = 16
    val MPI_MODE_EXCL = 64
    val MPI_MODE_RDONLY = 2
    val MPI_MODE_RDWR = 8
    val MPI_MODE_SEQUENTIAL = 256
    val MPI_MODE_UNIQUE_OPEN = 32
    val MPI_MODE_WRONLY = 4

    val MPI_MODE_NOCHECK = 1
    val MPI_MODE_NOSTORE = 8
    val MPI_MODE_NOPUT = 4
    val MPI_MODE_NOPRECEDE = 2
    val MPI_MODE_NOSUCCEED = 16

    val MPI_DISPLACEMENT_CURRENT = -54278278L

    val MPI_SEEK_SET = 600
    val MPI_SEEK_CUR = 602
    val MPI_SEEK_END = 604

    val MPI_STATUS_IGNORE =
      Pointer.pointerToAddress(0, classOf[MPI_Status], noRelease)
    val MPI_STATUSES_IGNORE =
      Pointer.pointerToAddress(0, classOf[MPI_Status], noRelease)
    val MPI_ERRCODES_IGNORE =
      Pointer.pointerToAddress(0, classOf[Int], noRelease)

    val MPI_ARGV_NULL =
      Pointer.pointerToAddress(0, classOf[Pointer[Byte]], noRelease)
    val MPI_ARGVS_NULL =
      Pointer.pointerToAddress(0, classOf[Pointer[Pointer[Byte]]], noRelease)

    val MPI_THREAD_SINGLE = 0
    val MPI_THREAD_FUNNELED = 1
    val MPI_THREAD_SERIALIZED = 2
    val MPI_THREAD_MULTIPLE = 3

    val MPI_GRAPH = 2
    val MPI_CART = 1
    val MPI_DIST_GRAPH = 3

    val MPI_COMBINER_NAMED = 0
    val MPI_COMBINER_DUP = 1
    val MPI_COMBINER_CONTIGUOUS = 2
    val MPI_COMBINER_VECTOR = 3
    val MPI_COMBINER_HVECTOR_INTEGER = 4
    val MPI_COMBINER_HVECTOR = 5
    val MPI_COMBINER_INDEXED = 6
    val MPI_COMBINER_HINDEXED_INTEGER = 7
    val MPI_COMBINER_HINDEXED = 8
    val MPI_COMBINER_INDEXED_BLOCK = 9
    val MPI_COMBINER_STRUCT_INTEGER = 10
    val MPI_COMBINER_STRUCT = 11
    val MPI_COMBINER_SUBARRAY = 12
    val MPI_COMBINER_DARRAY = 13
    val MPI_COMBINER_F90_REAL = 14
    val MPI_COMBINER_F90_COMPLEX = 15
    val MPI_COMBINER_F90_INTEGER = 16
    val MPI_COMBINER_RESIZED = 17

    val MPI_SUCCESS = 0
    val MPI_ERR_BUFFER = 1
    val MPI_ERR_COUNT = 2
    val MPI_ERR_TYPE = 3
    val MPI_ERR_TAG = 4
    val MPI_ERR_COMM = 5
    val MPI_ERR_RANK = 6
    val MPI_ERR_REQUEST = 7
    val MPI_ERR_ROOT = 8
    val MPI_ERR_GROUP = 9
    val MPI_ERR_OP = 10
    val MPI_ERR_TOPOLOGY = 11
    val MPI_ERR_DIMS = 12
    val MPI_ERR_ARG = 13
    val MPI_ERR_UNKNOWN = 14
    val MPI_ERR_TRUNCATE = 15
    val MPI_ERR_OTHER = 16
    val MPI_ERR_INTERN = 17
    val MPI_ERR_IN_STATUS = 18
    val MPI_ERR_PENDING = 19
    val MPI_ERR_ACCESS = 20
    val MPI_ERR_AMODE = 21
    val MPI_ERR_ASSERT = 22
    val MPI_ERR_BAD_FILE = 23
    val MPI_ERR_BASE = 24
    val MPI_ERR_CONVERSION = 25
    val MPI_ERR_DISP = 26
    val MPI_ERR_DUP_DATAREP = 27
    val MPI_ERR_FILE_EXISTS = 28
    val MPI_ERR_FILE_IN_USE = 29
    val MPI_ERR_FILE = 30
    val MPI_ERR_INFO_KEY = 31
    val MPI_ERR_INFO_NOKEY = 32
    val MPI_ERR_INFO_VALUE = 33
    val MPI_ERR_INFO = 34
    val MPI_ERR_IO = 35
    val MPI_ERR_KEYVAL = 36
    val MPI_ERR_LOCKTYPE = 37
    val MPI_ERR_NAME = 38
    val MPI_ERR_NO_MEM = 39
    val MPI_ERR_NOT_SAME = 40
    val MPI_ERR_NO_SPACE = 41
    val MPI_ERR_NO_SUCH_FILE = 42
    val MPI_ERR_PORT = 43
    val MPI_ERR_QUOTA = 44
    val MPI_ERR_READ_ONLY = 45
    val MPI_ERR_RMA_CONFLICT = 46
    val MPI_ERR_RMA_SYNC = 47
    val MPI_ERR_SERVICE = 48
    val MPI_ERR_SIZE = 49
    val MPI_ERR_SPAWN = 50
    val MPI_ERR_UNSUPPORTED_DATAREP = 51
    val MPI_ERR_UNSUPPORTED_OPERATION = 52
    val MPI_ERR_WIN = 53
    val MPI_ERR_LASTCODE = 54

    // lazy val MPI_COMM_NULL_COPY_FN =
    //   getSymbolPointer("OMPI_C_MPI_COMM_NULL_COPY_FN").
    //     as(classOf[MPI_Comm_copy_attr_function])
    // lazy val MPI_COMM_NULL_DELETE_FN =
    //   getSymbolPointer("OMPI_C_MPI_COMM_NULL_DELETE_FN").
    //     as(classOf[MPI_Comm_delete_attr_function])
    // lazy val MPI_COMM_DUP_FN =
    //   getSymbolPointer("OMPI_C_MPI_COMM_DUP_FN").
    //     as(classOf[MPI_Comm_copy_attr_function])
    // lazy val MPI_WIN_NULL_COPY_FN =
    //   getSymbolPointer("OMPI_C_MPI_WIN_NULL_COPY_FN").
    //     as(classOf[MPI_Win_copy_attr_function])
    // lazy val MPI_WIN_NULL_DELETE_FN =
    //   getSymbolPointer("OMPI_C_MPI_WIN_NULL_DELETE_FN").
    //     as(classOf[MPI_Win_delete_attr_function])
    // lazy val MPI_WIN_DUP_FN =
    //   getSymbolPointer("OMPI_C_MPI_WIN_DUP_FN").
    //     as(classOf[MPI_Win_copy_attr_function])
    // lazy val MPI_TYPE_NULL_COPY_FN =
    //   getSymbolPointer("OMPI_C_MPI_TYPE_NULL_COPY_FN").
    //     as(classOf[MPI_Type_copy_attr_function])
    // lazy val MPI_TYPE_NULL_DELETE_FN =
    //   getSymbolPointer("OMPI_C_MPI_TYPE_NULL_DELETE_FN").
    //     as(classOf[MPI_Type_delete_attr_function])
    // lazy val MPI_TYPE_DUP_FN =
    //   getSymbolPointer("OMPI_C_MPI_TYPE_DUP_FN").
    //     as(classOf[MPI_Type_copy_attr_function])

    // A.2.1 Point-to-Point Communication C Bindings

    @native def MPI_Bsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    @native def MPI_Bsend_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    @native def MPI_Buffer_attach(buffer: Pointer[_], size: Int): Int
    @native def MPI_Buffer_detach(buffer_addr: Pointer[Pointer[_]],
      size: Pointer[Int]): Int
    @native def MPI_Cancel(request: Pointer[MPI_Request]): Int
    @native def MPI_Get_count(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Pointer[Int]): Int
    @native def MPI_Ibsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    @native def MPI_Iprobe(source: Int, tag: Int, comm: MPI_Comm,
      flag: Pointer[Int], status: Pointer[MPI_Status]): Int
    @native def MPI_Irecv(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      source: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    @native def MPI_Irsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    @native def MPI_Isend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    @native def MPI_Issend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm, request: Pointer[MPI_Request]): Int
    @native def MPI_Probe(source: Int, tag: Int, comm: MPI_Comm,
      status: Pointer[MPI_Status]): Int
    @native def MPI_Recv(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      source: Int, tag: Int, comm: MPI_Comm, status: Pointer[MPI_Status]): Int
    @native def MPI_Recv_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, source: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    @native def MPI_Request_free(request: Pointer[MPI_Request]): Int
    @native def MPI_Request_get_status(request: MPI_Request, flag: Pointer[Int],
      status: Pointer[MPI_Status]): Int
    @native def MPI_Rsend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    @native def MPI_Rsend_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    @native def MPI_Send(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    @native def MPI_Send_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    @native def MPI_Sendrecv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, dest: Int, sendtag: Int, recvbuf: Pointer[_],
      recvcount: Int, recvtype: MPI_Datatype, source: Int, recvtag: Int,
      comm: MPI_Comm, status: Pointer[MPI_Status]): Int
    @native def MPI_Sendrecv_replace(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, sendtag: Int, source: Int,
      recvtag: Int, comm: MPI_Comm, status: Pointer[MPI_Status]): Int
    @native def MPI_Ssend(buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      dest: Int, tag: Int, comm: MPI_Comm): Int
    @native def MPI_Ssend_init(buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, dest: Int, tag: Int, comm: MPI_Comm,
      request: Pointer[MPI_Request]): Int
    @native def MPI_Start(request: Pointer[MPI_Request]): Int
    @native def MPI_Startall(count: Int,
      array_of_requests: Pointer[MPI_Request]): Int
    @native def MPI_Test(request: Pointer[MPI_Request], flag: Pointer[Int],
      status: Pointer[MPI_Status]): Int
    @native def MPI_Test_cancelled(status: Pointer[MPI_Status],
      flag: Pointer[Int]): Int
    @native def MPI_Testall(count: Int, array_of_requests: Pointer[MPI_Request],
      flag: Pointer[Int], array_of_statuses: Pointer[MPI_Status]): Int
    @native def MPI_Testany(count: Int, array_of_requests: Pointer[MPI_Request],
      index: Pointer[Int], flag: Pointer[Int], status: Pointer[MPI_Status]): Int
    @native def MPI_Testsome(incount: Int,
      array_of_requests: Pointer[MPI_Request], outcount: Pointer[Int],
      array_of_indices: Pointer[Int],
      array_of_statuses: Pointer[MPI_Status]): Int
    @native def MPI_Wait(request: Pointer[MPI_Request],
      status: Pointer[MPI_Status]): Int
    @native def MPI_Waitall(count: Int, array_of_requests: Pointer[MPI_Request],
      array_of_statuses: Pointer[MPI_Status]): Int
    @native def MPI_Waitany(count: Int, array_of_requests: Pointer[MPI_Request],
      index: Pointer[Int], status: Pointer[MPI_Status]): Int
    @native def MPI_Waitsome(incount: Int,
      array_of_requests: Pointer[MPI_Request], outcount: Pointer[Int],
      array_of_indices: Pointer[Int],
      array_of_statuses: Pointer[MPI_Status]): Int

    // A.2.2 Datatypes C Bindings

    @native def MPI_Get_address(location: Pointer[_],
      address: Pointer[MPI_Aint]): Int
    @native def MPI_Get_elements(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Pointer[Int]): Int
    @native def MPI_Pack(inbuf: Pointer[_], incount: Int,
      datatype: MPI_Datatype, outbuf: Pointer[_], outsize: Int,
      position: Pointer[Int], comm: MPI_Comm): Int
    @native def MPI_Pack_external(datarep: Pointer[Byte], inbuf: Pointer[_],
      incount: Int, datatype: MPI_Datatype, outbuf: Pointer[_],
      outsize: MPI_Aint,  position: Pointer[MPI_Aint]): Int
    @native def MPI_Pack_external_size(datarep: Pointer[Byte], incount: Int,
      datatype: MPI_Datatype, size: Pointer[MPI_Aint]): Int
    @native def MPI_Pack_size(incount: Int, datatype: MPI_Datatype,
      comm: MPI_Comm, size: Pointer[Int]): Int
    @native def MPI_Type_commit(datatype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_contiguous(count: Int, oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_darray(size: Int, rank: Int, ndims: Int,
      array_of_gsizes: Pointer[Int], array_of_distribs: Pointer[Int],
      array_of_dargs: Pointer[Int], array_of_psizes: Pointer[Int], order: Int,
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_hindexed(count: Int,
      array_of_blocklengths: Pointer[Int],
      array_of_displacements: Pointer[MPI_Aint], oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_hvector(count: Int, blocklength: Int,
      stride: MPI_Aint, oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_indexed_block(count: Int, blocklength: Int,
      array_of_displacements: Pointer[Int], oldtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_resized(oldtype: MPI_Datatype, lb: MPI_Aint,
      extent: MPI_Aint, newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_struct(count: Int,
      array_of_blocklengths: Pointer[Int],
      array_of_displacements: Pointer[MPI_Aint],
      array_of_types: Pointer[MPI_Datatype],
      newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_create_subarray(ndims: Int,
      array_of_sizes: Pointer[Int],
      array_of_subsizes: Pointer[Int], array_of_starts: Pointer[Int],
      order: Int, oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_dup(dtype: MPI_Datatype,
      newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_free(datatype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_get_contents(datatype: MPI_Datatype, max_integers: Int,
      max_addresses: Int, max_datatypes: Int, array_of_integers: Pointer[Int],
      array_of_addresses: Pointer[MPI_Aint],
      array_of_datatypes: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_get_envelope(datatype: MPI_Datatype,
      num_integers: Pointer[Int], num_addresses: Pointer[Int],
      num_datatypes: Pointer[Int], combiner: Pointer[Int]): Int
    @native def MPI_Type_get_extent(datatype: MPI_Datatype,
      lb: Pointer[MPI_Aint], extent: Pointer[MPI_Aint]): Int
    @native def MPI_Type_get_true_extent(datatype: MPI_Datatype,
      true_lb: Pointer[MPI_Aint], true_extent: Pointer[MPI_Aint]): Int
    @native def MPI_Type_indexed(count: Int,
      array_of_blocklengths: Pointer[Int], array_of_displacements: Pointer[Int],
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Type_size(datatype: MPI_Datatype, size: Pointer[Int]): Int
    @native def MPI_Type_vector(count: Int, blocklength: Int, stride: Int,
      oldtype: MPI_Datatype, newtype: Pointer[MPI_Datatype]): Int
    @native def MPI_Unpack(inbuf: Pointer[_], insize: Int,
      position: Pointer[Int], outbuf: Pointer[_], outcount: Int,
      datatype: MPI_Datatype, comm: MPI_Comm): Int
    @native def MPI_Unpack_external(datarep: Pointer[Byte], inbuf: Pointer[_],
      insize: MPI_Aint, position: Pointer[MPI_Aint], outbuf: Pointer[_],
      outcount: Int, datatype: MPI_Datatype): Int

    // A.2.3 Collective Communication C Bindings

    @native def MPI_Allgather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm): Int
    @native def MPI_Allgatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, comm: MPI_Comm): Int
    @native def MPI_Allreduce(sendbuf: Pointer[_], recvbuf: Pointer[_],
      count: Int, datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    @native def MPI_Alltoall(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, comm: MPI_Comm): Int
    @native def MPI_Alltoallv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcounts: Pointer[Int], rdispls: Pointer[Int], recvtype: MPI_Datatype,
      comm: MPI_Comm): Int
    @native def MPI_Alltoallw(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      sdispls: Pointer[Int], sendtypes: Pointer[MPI_Datatype],
      recvbuf: Pointer[_], recvcounts: Pointer[Int], rdispls: Pointer[Int],
      recvtypes: Pointer[MPI_Datatype], comm: MPI_Comm): Int
    @native def MPI_Barrier(comm: MPI_Comm): Int
    @native def MPI_Bcast(buffer: Pointer[_], count: Int,
      datatype: MPI_Datatype, root: Int, comm: MPI_Comm): Int
    @native def MPI_Exscan(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    @native def MPI_Gather(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, root: Int, comm: MPI_Comm): Int
    @native def MPI_Gatherv(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcounts: Pointer[Int],
      displs: Pointer[Int], recvtype: MPI_Datatype, root: Int,
      comm: MPI_Comm): Int
    @native def MPI_Op_commutative(op: MPI_Op, commute: Pointer[Int]): Int
    @native def MPI_Op_create(function: Pointer[MPI_User_function],
      commute: Int, op: Pointer[MPI_Op]): Int
    @native def MPI_Op_free(op: Pointer[MPI_Op]): Int
    @native def MPI_Reduce(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, root: Int, comm: MPI_Comm): Int
    @native def MPI_Reduce_local(inbuf: Pointer[_], inoutbuf: Pointer[_],
      count: Int, datatype: MPI_Datatype, op: MPI_Op): Int
    @native def MPI_Reduce_scatter(sendbuf: Pointer[_], recvbuf: Pointer[_],
      recvcounts: Pointer[Int], datatype: MPI_Datatype, op: MPI_Op,
      comm: MPI_Comm): Int
    def MPI_Reduce_scatter_block(sendbuf: Pointer[_],
      recvbuf: Pointer[_], recvcount: Int, datatype: MPI_Datatype, op: MPI_Op,
      comm: MPI_Comm): Int =
      ???
    @native def MPI_Scan(sendbuf: Pointer[_], recvbuf: Pointer[_], count: Int,
      datatype: MPI_Datatype, op: MPI_Op, comm: MPI_Comm): Int
    @native def MPI_Scatter(sendbuf: Pointer[_], sendcount: Int,
      sendtype: MPI_Datatype, recvbuf: Pointer[_], recvcount: Int,
      recvtype: MPI_Datatype, root: Int, comm: MPI_Comm): Int
    @native def MPI_Scatterv(sendbuf: Pointer[_], sendcounts: Pointer[Int],
      displs: Pointer[Int], sendtype: MPI_Datatype, recvbuf: Pointer[_],
      recvcount: Int, recvtype: MPI_Datatype, root: Int, comm: MPI_Comm): Int

    // A.2.4 Groups, Contexts, Communicators, and Caching C Bindings

    @native def MPI_Comm_compare(comm1: MPI_Comm, comm2: MPI_Comm,
      result: Pointer[Int]): Int
    @native def MPI_Comm_create(comm: MPI_Comm, group: MPI_Group,
      newcomm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_create_keyval(
      comm_copy_attr_fn: Pointer[MPI_Comm_copy_attr_function],
      comm_delete_attr_fn: Pointer[MPI_Comm_delete_attr_function],
      comm_keyval: Pointer[Int], extra_state: Pointer[_]): Int
    @native def MPI_Comm_delete_attr(comm: MPI_Comm, comm_keyval: Int): Int
    @native def MPI_Comm_dup(comm: MPI_Comm, newcomm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_free(comm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_free_keyval(comm_keyval: Pointer[Int]): Int
    @native def MPI_Comm_get_attr(comm: MPI_Comm, comm_keyval: Int,
      attribute_val: Pointer[Pointer[_]], flag: Pointer[Int]): Int
    @native def MPI_Comm_get_name(comm: MPI_Comm, comm_name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    @native def MPI_Comm_group(comm: MPI_Comm, group: Pointer[MPI_Group]): Int
    @native def MPI_Comm_rank(comm: MPI_Comm, rank: Pointer[Int]): Int
    @native def MPI_Comm_remote_group(comm: MPI_Comm,
      group: Pointer[MPI_Group]): Int
    @native def MPI_Comm_remote_size(comm: MPI_Comm, size: Pointer[Int]): Int
    @native def MPI_Comm_set_attr(comm: MPI_Comm, comm_keyval: Int,
      attribute_val: Pointer[_]): Int
    @native def MPI_Comm_set_name(comm: MPI_Comm, comm_name: Pointer[Byte]): Int
    @native def MPI_Comm_size(comm: MPI_Comm, size: Pointer[Int]): Int
    @native def MPI_Comm_split(comm: MPI_Comm, color: Int, key: Int,
      newcomm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_test_inter(comm: MPI_Comm, flag: Pointer[Int]): Int
    @native def MPI_Group_compare(group1: MPI_Group, group2: MPI_Group,
      result: Pointer[Int]): Int
    @native def MPI_Group_difference(group1: MPI_Group, group2: MPI_Group,
      newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Group_excl(group: MPI_Group, n: Int, ranks: Pointer[Int],
      newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Group_free(group: Pointer[MPI_Group]): Int
    @native def MPI_Group_incl(group: MPI_Group, n: Int, ranks: Pointer[Int],
      newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Group_intersection(group1: MPI_Group, group2: MPI_Group,
      newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Group_range_excl(group: MPI_Group, n: Int,
      ranges: Pointer[Int], newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Group_range_incl(group: MPI_Group, n: Int,
      ranges: Pointer[Int], newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Group_rank(group: MPI_Group, rank: Pointer[Int]): Int
    @native def MPI_Group_size(group: MPI_Group, size: Pointer[Int]): Int
    @native def MPI_Group_translate_ranks(group1: MPI_Group, n: Int,
      ranks1: Pointer[Int], group2: MPI_Group, ranks2: Pointer[Int]): Int
    @native def MPI_Group_union(group1: MPI_Group, group2: MPI_Group,
      newgroup: Pointer[MPI_Group]): Int
    @native def MPI_Intercomm_create(local_comm: MPI_Comm, local_leader: Int,
      peer_comm: MPI_Comm, remote_leader: Int, tag: Int,
      newintercomm: Pointer[MPI_Comm]): Int
    @native def MPI_Intercomm_merge(intercomm: MPI_Comm, high: Int,
      newintracomm: Pointer[MPI_Comm]): Int
    @native def MPI_Type_create_keyval(
      type_copy_attr_fn: Pointer[MPI_Type_copy_attr_function],
      type_delete_attr_fn: Pointer[MPI_Type_delete_attr_function],
      type_keyval: Pointer[Int], extra_state: Pointer[_]): Int
    @native def MPI_Type_delete_attr(datatype: MPI_Datatype,
      type_keyval: Int): Int
    @native def MPI_Type_free_keyval(type_keyval: Pointer[Int]): Int
    @native def MPI_Type_get_attr(datatype: MPI_Datatype, type_keyval: Int,
      attribute_val: Pointer[Pointer[_]], flag: Pointer[Int]): Int
    @native def MPI_Type_get_name(datatype: MPI_Datatype,
      type_name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    @native def MPI_Type_set_attr(datatype: MPI_Datatype, type_keyval: Int,
      attribute_val: Pointer[_]): Int
    @native def MPI_Type_set_name(datatype: MPI_Datatype,
      type_name: Pointer[Byte]): Int
    @native def MPI_Win_create_keyval(
      win_copy_attr_fn: Pointer[MPI_Win_copy_attr_function],
      win_delete_attr_fn: Pointer[MPI_Win_delete_attr_function],
      win_keyval: Pointer[Int], extra_state: Pointer[_]) :Int
    @native def MPI_Win_delete_attr(win: MPI_Win, win_keyval: Int): Int
    @native def MPI_Win_free_keyval(win_keyval: Pointer[Int]): Int
    @native def MPI_Win_get_attr(win: MPI_Win, win_keyval: Int,
      attribute_val: Pointer[Pointer[_]], flag: Pointer[Int]): Int
    @native def MPI_Win_get_name(win: MPI_Win, win_name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    @native def MPI_Win_set_attr(win: MPI_Win, win_keyval: Int,
      attribute_val: Pointer[_]): Int
    @native def MPI_Win_set_name(win: MPI_Win, win_name: Pointer[Byte]): Int

    // A.2.5 Process Topologies C Bindings

    @native def MPI_Cart_coords(comm: MPI_Comm, rank: Int, maxdims: Int,
      coords: Pointer[Int]): Int
    @native def MPI_Cart_create(comm_old: MPI_Comm, ndims: Int,
      dims: Pointer[Int], periods: Pointer[Int], reorder: Int,
      comm_cart: Pointer[MPI_Comm]): Int
    @native def MPI_Cart_get(comm: MPI_Comm, maxdims: Int, dims: Pointer[Int],
      periods: Pointer[Int], coords: Pointer[Int]): Int
    @native def MPI_Cart_map(comm: MPI_Comm, ndims: Int, dims: Pointer[Int],
      periods: Pointer[Int], newrank: Pointer[Int]): Int
    @native def MPI_Cart_rank(comm: MPI_Comm, coords: Pointer[Int],
      rank: Pointer[Int]): Int
    @native def MPI_Cart_shift(comm: MPI_Comm, direction: Int, disp: Int,
      rank_source: Pointer[Int], rank_dest: Pointer[Int]): Int
    @native def MPI_Cart_sub(comm: MPI_Comm, remain_dims: Pointer[Int],
      newcomm: Pointer[MPI_Comm]): Int
    @native def MPI_Cartdim_get(comm: MPI_Comm, ndims: Pointer[Int]): Int
    @native def MPI_Dims_create(nnodes: Int, ndims: Int,
      dims: Pointer[Int]): Int
    def MPI_Dist_graph_create(comm_old: MPI_Comm, n: Int,
      sources: Pointer[Int], degrees: Pointer[Int], destinations: Pointer[Int],
      weights: Pointer[Int], info: MPI_Info, reorder: Int,
      comm_dist_graph: Pointer[MPI_Comm]): Int =
      ???
    def MPI_Dist_graph_create_adjacent(comm_old: MPI_Comm,
      indegree: Int, sources: Pointer[Int], sourceweights: Pointer[Int],
      outdegree: Int, destinations: Pointer[Int], destweights: Pointer[Int],
      info: MPI_Info, reorder: Int, comm_dist_graph: Pointer[MPI_Comm]): Int =
      ???
    def MPI_Dist_graph_neighbors(comm: MPI_Comm, maxindegree: Int,
      sources: Pointer[Int], sourceweights: Pointer[Int], maxoutdegree: Int,
      destinations: Pointer[Int], destweights: Pointer[Int]): Int =
      ???
    def MPI_Dist_graph_neighbors_count(comm: MPI_Comm,
      indegree: Pointer[Int], outdegree: Pointer[Int],
      weighted: Pointer[Int]): Int =
      ???
    @native def MPI_Graph_create(comm_old: MPI_Comm, nnodes: Int,
      index: Pointer[Int],
      edges: Pointer[Int], reorder: Int, comm_graph: Pointer[MPI_Comm]): Int
    @native def MPI_Graph_get(comm: MPI_Comm, maxindex: Int, maxedges: Int,
      index: Pointer[Int], edges: Pointer[Int]): Int
    @native def MPI_Graph_map(comm: MPI_Comm, nnodes: Int, index: Pointer[Int],
      edges: Pointer[Int], newrank: Pointer[Int]): Int
    @native def MPI_Graph_neighbors(comm: MPI_Comm, rank: Int,
      maxneighbors: Int, neighbors: Pointer[Int]): Int
    @native def MPI_Graph_neighbors_count(comm: MPI_Comm, rank: Int,
      nneighbors: Pointer[Int]): Int
    @native def MPI_Graphdims_get(comm: MPI_Comm, nnodes: Pointer[Int],
      nedges: Pointer[Int]): Int
    @native def MPI_Topo_test(comm: MPI_Comm, status: Pointer[Int]): Int

    // A.2.6 MPI Environmenta Management C Bindings

    @native def MPI_Abort(comm: MPI_Comm, errorcode: Int): Int
    @native def MPI_Add_error_class(errorclass: Pointer[Int]): Int
    @native def MPI_Add_error_code(errorclass: Int,
      errorcode: Pointer[Int]): Int
    @native def MPI_Add_error_string(errorcode: Int, string: Pointer[Byte]): Int
    @native def MPI_Alloc_mem(size: MPI_Aint, info: MPI_Info,
      baseptr: Pointer[_]): Int
    @native def MPI_Comm_call_errhandler(comm: MPI_Comm, errorcode: Int): Int
    @native def MPI_Comm_create_errhandler(
      function: Pointer[MPI_Comm_errhandler_function],
      errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_Comm_get_errhandler(comm: MPI_Comm,
      errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_Comm_set_errhandler(comm: MPI_Comm,
      errhandler: MPI_Errhandler): Int
    @native def MPI_Errhandler_free(errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_Error_class(errorcode: Int, errorclass: Pointer[Int]): Int
    @native def MPI_Error_string(errorcode: Int, string: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    @native def MPI_File_call_errhandler(fh: MPI_File, errorcode: Int): Int
    @native def MPI_File_create_errhandler(
      function: Pointer[MPI_File_errhandler_function],
      errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_File_get_errhandler(file: MPI_File,
      errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_File_set_errhandler(file: MPI_File,
      errhandler: MPI_Errhandler): Int
    @native def MPI_Finalize(): Int
    @native def MPI_Finalized(flag: Pointer[Int]): Int
    @native def MPI_Free_mem(base: Pointer[_]): Int
    @native def MPI_Get_processor_name(name: Pointer[Byte],
      resultlen: Pointer[Int]): Int
    @native def MPI_Get_version(version: Pointer[Int],
      subversion: Pointer[Int]): Int
    @native def MPI_Init(argc: Pointer[Int],
      argv: Pointer[Pointer[Pointer[Byte]]]): Int
    @native def MPI_Initialized(flag: Pointer[Int]): Int
    @native def MPI_Win_call_errhandler(win: MPI_Win, errorcode: Int): Int
    @native def MPI_Win_create_errhandler(
      function: Pointer[MPI_Win_errhandler_function],
      errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_Win_get_errhandler(win: MPI_Win,
      errhandler: Pointer[MPI_Errhandler]): Int
    @native def MPI_Win_set_errhandler(win: MPI_Win,
      errhandler: MPI_Errhandler): Int
    @native def MPI_Wtick(): Double
    @native def MPI_Wtime(): Double

    // A.2.7 The Info Object C Bindings

    @native def MPI_Info_create(info: Pointer[MPI_Info]): Int
    @native def MPI_Info_delete(info: MPI_Info, key: Pointer[Byte]): Int
    @native def MPI_Info_dup(info: MPI_Info, newinfo: Pointer[MPI_Info]): Int
    @native def MPI_Info_free(info: Pointer[MPI_Info]): Int
    @native def MPI_Info_get(info: MPI_Info, key: Pointer[Byte], valuelen: Int,
      value: Pointer[Byte], flag: Pointer[Int]): Int
    @native def MPI_Info_get_nkeys(info: MPI_Info, nkeys: Pointer[Int]): Int
    @native def MPI_Info_get_nthkey(info: MPI_Info, n: Int,
      key: Pointer[Byte]): Int
    @native def MPI_Info_get_valuelen(info: MPI_Info, key: Pointer[Byte],
      valuelen: Pointer[Int], flag: Pointer[Int]): Int
    @native def MPI_Info_set(info: MPI_Info, key: Pointer[Byte],
      value: Pointer[Byte]): Int

    // A.2.8 Process Creation and Management C Bindings

    @native def MPI_Close_port(port_name: Pointer[Byte]): Int
    @native def MPI_Comm_accept(port_name: Pointer[Byte], info: MPI_Info,
      root: Int, comm: MPI_Comm, newcomm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_connect(port_name: Pointer[Byte], info: MPI_Info,
      root: Int, comm: MPI_Comm, newcomm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_disconnect(comm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_get_parent(parent: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_join(fd: Int, intercomm: Pointer[MPI_Comm]): Int
    @native def MPI_Comm_spawn(command: Pointer[Byte],
      argv: Pointer[Pointer[Byte]], maxprocs: Int, info: MPI_Info, root: Int,
      comm: MPI_Comm, intercomm: Pointer[MPI_Comm],
      array_of_errcodes: Pointer[Int]): Int
    @native def MPI_Comm_spawn_multiple(count: Int,
      array_of_commands: Pointer[Pointer[Byte]],
      array_of_argv: Pointer[Pointer[Pointer[Byte]]],
      array_of_maxprocs: Pointer[Int], array_of_info: Pointer[MPI_Info],
      root: Int, comm: MPI_Comm, intercomm: Pointer[MPI_Comm],
      array_of_errcodes: Pointer[Int]): Int
    @native def MPI_Lookup_name(service_name: Pointer[Byte], info: MPI_Info,
      port_name: Pointer[Byte]): Int
    @native def MPI_Open_port(info: MPI_Info, port_name: Pointer[Byte]): Int
    @native def MPI_Publish_name(service_name: Pointer[Byte], info: MPI_Info,
      port_name: Pointer[Byte]): Int
    @native def MPI_Unpublish_name(service_name: Pointer[Byte], info: MPI_Info,
      port_name: Pointer[Byte]): Int

    // A.2.9 One-Sided Communications C Bindings

    @native def MPI_Accumulate(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, op: MPI_Op,
      win: MPI_Win): Int
    @native def MPI_Get(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, win: MPI_Win): Int
    @native def MPI_Put(origin_addr: Pointer[_], origin_count: Int,
      origin_datatype: MPI_Datatype, target_rank: Int, target_disp: MPI_Aint,
      target_count: Int, target_datatype: MPI_Datatype, win: MPI_Win): Int
    @native def MPI_Win_complete(win: MPI_Win): Int
    @native def MPI_Win_create(base: Pointer[_], size: MPI_Aint, disp_unit: Int,
      info: MPI_Info, comm: MPI_Comm, win: Pointer[MPI_Win]): Int
    @native def MPI_Win_fence(assert: Int, win: MPI_Win): Int
    @native def MPI_Win_free(win: Pointer[MPI_Win]): Int
    @native def MPI_Win_get_group(win: MPI_Win, group: Pointer[MPI_Group]): Int
    @native def MPI_Win_lock(lock_type: Int, rank: Int, assert: Int,
      win: MPI_Win): Int
    @native def MPI_Win_post(group: MPI_Group, assert: Int, win: MPI_Win): Int
    @native def MPI_Win_start(group: MPI_Group, assert: Int, win: MPI_Win): Int
    @native def MPI_Win_test(win: MPI_Win, flag: Pointer[Int]): Int
    @native def MPI_Win_unlock(rank: Int, win: MPI_Win): Int
    @native def MPI_Win_wait(win: MPI_Win): Int

    // A.2.10 External Interfaces C Bindings

    @native def MPI_Grequest_complete(request: MPI_Request): Int
    @native def MPI_Grequest_start(
      query_fn: Pointer[MPI_Grequest_query_function],
      free_fn: Pointer[MPI_Grequest_free_function],
      cancel_fn: Pointer[MPI_Grequest_cancel_function],
      extra_state: Pointer[_], request: Pointer[MPI_Request]): Int
    @native def MPI_Init_thread(argc: Pointer[Int],
      argv: Pointer[Pointer[Pointer[Byte]]], required: Int,
      provided: Pointer[Int]): Int
    @native def MPI_Is_thread_main(flag: Pointer[Int]): Int
    @native def MPI_Query_thread(provided: Pointer[Int]): Int
    @native def MPI_Status_set_cancelled(status: Pointer[MPI_Status],
      flag: Int): Int
    @native def MPI_Status_set_elements(status: Pointer[MPI_Status],
      datatype: MPI_Datatype, count: Int): Int

    // A.2.11 I/O C Bindings

    @native def MPI_File_close(fh: Pointer[MPI_File]): Int
    @native def MPI_File_delete(filename: Pointer[Byte], info: MPI_Info): Int
    @native def MPI_File_get_amode(fh: MPI_File, amode: Pointer[Int]): Int
    @native def MPI_File_get_atomicity(fh: MPI_File, flag: Pointer[Int]): Int
    @native def MPI_File_get_byte_offset(fh: MPI_File, offset: MPI_Offset,
      disp: Pointer[MPI_Offset]): Int
    @native def MPI_File_get_group(fh: MPI_File,
      group: Pointer[MPI_Group]): Int
    @native def MPI_File_get_info(fh: MPI_File,
      info_used: Pointer[MPI_Info]): Int
    @native def MPI_File_get_position(fh: MPI_File,
      offset: Pointer[MPI_Offset]): Int
    @native def MPI_File_get_position_shared(fh: MPI_File,
      offset: Pointer[MPI_Offset]): Int
    @native def MPI_File_get_size(fh: MPI_File, size: Pointer[MPI_Offset]): Int
    @native def MPI_File_get_type_extent(fh: MPI_File, datatype: MPI_Datatype,
      extent: Pointer[MPI_Aint]): Int
    @native def MPI_File_get_view(fh: MPI_File, disp: Pointer[MPI_Offset],
      etype: Pointer[MPI_Datatype], filetype: Pointer[MPI_Datatype],
      datarep: Pointer[Byte]): Int
    @native def MPI_File_iread(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    @native def MPI_File_iread_at(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    @native def MPI_File_iread_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    @native def MPI_File_iwrite(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    @native def MPI_File_iwrite_at(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    @native def MPI_File_iwrite_shared(fh: MPI_File, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, request: Pointer[MPI_Request]): Int
    @native def MPI_File_open(comm: MPI_Comm, filename: Pointer[Byte],
      amode: Int, info: MPI_Info, fh: Pointer[MPI_File]): Int
    @native def MPI_File_preallocate(fh: MPI_File, size: MPI_Offset): Int
    @native def MPI_File_read(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_all(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_all_begin(fh: MPI_File, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype): Int
    @native def MPI_File_read_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_at(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_at_all(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_at_all_begin(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype): Int
    @native def MPI_File_read_at_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_ordered(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_ordered_begin(fh: MPI_File, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype): Int
    @native def MPI_File_read_ordered_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_read_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_seek(fh: MPI_File, offset: MPI_Offset,
      whence: Int): Int
    @native def MPI_File_seek_shared(fh: MPI_File, offset: MPI_Offset,
      whence: Int): Int
    @native def MPI_File_set_atomicity(fh: MPI_File, flag: Int): Int
    @native def MPI_File_set_info(fh: MPI_File, info: MPI_Info): Int
    @native def MPI_File_set_size(fh: MPI_File, size: MPI_Offset): Int
    @native def MPI_File_set_view(fh: MPI_File, disp: MPI_Offset,
      etype: MPI_Datatype, filetype: MPI_Datatype, datarep: Pointer[Byte],
      info: MPI_Info): Int
    @native def MPI_File_sync(fh: MPI_File): Int
    @native def MPI_File_write(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_all(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_all_begin(fh: MPI_File, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype): Int
    @native def MPI_File_write_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_at(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_at_all(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype,
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_at_all_begin(fh: MPI_File, offset: MPI_Offset,
      buf: Pointer[_], count: Int, datatype: MPI_Datatype): Int
    @native def MPI_File_write_at_all_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_ordered(fh: MPI_File, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_ordered_begin(fh: MPI_File, buf: Pointer[_],
      count: Int, datatype: MPI_Datatype): Int
    @native def MPI_File_write_ordered_end(fh: MPI_File, buf: Pointer[_],
      status: Pointer[MPI_Status]): Int
    @native def MPI_File_write_shared(fh: MPI_File, buf: Pointer[_], count: Int,
      datatype: MPI_Datatype, status: Pointer[MPI_Status]): Int
    @native def MPI_Register_datarep(datarep: Pointer[Byte],
      read_conversion_fn: Pointer[MPI_Datarep_conversion_function],
      write_conversion_fn: Pointer[MPI_Datarep_conversion_function],
      dtype_file_extent_fn: Pointer[MPI_Datarep_extent_function],
      extra_state: Pointer[_]): Int
  }
}
