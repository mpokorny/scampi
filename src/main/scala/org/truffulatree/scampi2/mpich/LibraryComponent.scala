//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2.mpich

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

  def allocateDatatype(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateComm(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateGroup(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateWin(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateFile(n: Int = 1) =
    Pointer.allocateTypedPointers(classOf[lib.MPI_File], n)

  def allocateOp(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateErrhandler(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateRequest(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateInfo(n: Int = 1) = Pointer.allocateInts(n).as(classOf[Int])

  def allocateAint(n: Int = 1) = Pointer.allocateCLongs(n)

  def allocateOffset(n: Int = 1) = Pointer.allocateLongs(n).as(classOf[Long])

  //def associateStatus(peer: Pointer[lib.MPI_Status]) = lib.MpiStatus(peer)

  def newStatus(n: Int = 1) = lib.MpiStatus(n)

  protected def _aintToLong(aint: lib.MPI_Aint): Long = aint.longValue

  protected def _aintFromLong(long: Long): lib.MPI_Aint = CLong.valueOf(long)

  protected def _offsetToLong(offset: lib.MPI_Offset): Long = offset

  protected def _offsetFromLong(long: Long): lib.MPI_Offset = long

  @Library("mpich")
  @Runtime(classOf[CRuntime])
  final object lib extends Mpi2Library {
    BridJ.register()

    type MPI_Datatype = Int
    type MPI_Comm = Int
    type MPI_Group = Int
    type MPI_Win = Int
    type MPI_File = TypedPointers.MPI_File
    type MPI_Op = Int
    final class MPI_Status(peer: Pointer[MPI_Status])
        extends StructObject(peer) with CStatus {
      val sizeOf = typeInfo.sizeOf
      @Field(0)
      def count = io.getIntField(this, 0)
      @Field(0)
      def count_=(x: Int) {
	io.setIntField(this, 0, x)
      }
      @Field(1)
      def cancelled = io.getIntField(this, 1)
      @Field(1)
      def cancelled_=(x: Int) {
	io.setIntField(this, 1, x)
      }
      @Field(2)
      def MPI_SOURCE: Int = io.getIntField(this, 2)
      @Field(2)
      def MPI_SOURCE_=(x: Int) {
	io.setIntField(this, 2, x)
      }
      @Field(3)
      def MPI_TAG: Int = io.getIntField(this, 3)
      @Field(3)
      def MPI_TAG_=(x: Int) {
	io.setIntField(this, 3, x)
      }
      @Field(4)
      def MPI_ERROR: Int = io.getIntField(this, 4)
      @Field(4)
      def MPI_ERROR_=(x: Int) {
	io.setIntField(this, 4, x)
      }
      override def toString =
        s"""|MPI_Status(${count},${cancelled},${MPI_SOURCE},
            |${MPI_TAG},${MPI_ERROR})""".stripMargin
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
    type MPI_Request = Int
    type MPI_Errhandler = Int
    type MPI_Info = Int
    type MPI_Aint = CLong
    type MPI_Offset = Long

    // Values
    val MPI_IDENT = 0
    val MPI_CONGRUENT = 1
    val MPI_SIMILAR = 2
    val MPI_UNEQUAL = 3

    val MPI_CHAR = 0x4c000101
    val MPI_SIGNED_CHAR = 0x4c000118
    val MPI_UNSIGNED_CHAR = 0x4c000102
    val MPI_BYTE = 0x4c00010d
    val MPI_WCHAR = 0x4c00040e
    val MPI_SHORT = 0x4c000203
    val MPI_UNSIGNED_SHORT = 0x4c000204
    val MPI_INT = 0x4c000405
    val MPI_UNSIGNED = 0x4c000406
    val MPI_LONG = 0x4c000807
    val MPI_UNSIGNED_LONG = 0x4c000808
    val MPI_FLOAT = 0x4c00040a
    val MPI_DOUBLE = 0x4c00080b
    val MPI_LONG_DOUBLE = 0x4c00100c
    val MPI_LONG_LONG_INT = 0x4c000809
    val MPI_UNSIGNED_LONG_LONG = 0x4c000819
    val MPI_LONG_LONG = MPI_LONG_LONG_INT
    val MPI_PACKED = 0x4c00010f
    val MPI_LB = 0x4c000010
    val MPI_UB = 0x4c000011
    val MPI_FLOAT_INT = 0x8c000000
    val MPI_DOUBLE_INT = 0x8c000001
    val MPI_LONG_INT = 0x8c000002
    val MPI_SHORT_INT = 0x8c000003
    val MPI_2INT = 0x4c000816
    val MPI_LONG_DOUBLE_INT = 0x8c000004
    val MPI_INT8_T = 0x4c000137
    val MPI_INT16_T = 0x4c000238
    val MPI_INT32_T = 0x4c000439
    val MPI_INT64_T = 0x4c00083a
    val MPI_UINT8_T = 0x4c00013b
    val MPI_UINT16_T = 0x4c00023c
    val MPI_UINT32_T = 0x4c00043d
    val MPI_UINT64_T = 0x4c00083e
    val MPI_C_BOOL = 0x4c00013f
    val MPI_C_FLOAT_COMPLEX = 0x4c000840
    val MPI_C_COMPLEX = MPI_C_FLOAT_COMPLEX
    val MPI_C_DOUBLE_COMPLEX = 0x4c001041
    val MPI_C_LONG_DOUBLE_COMPLEX = 0x4c002042
    val MPI_AINT = 0x4c000843
    val MPI_OFFSET = 0x4c000844

    val MPI_COMM_WORLD = 0x44000000
    val MPI_COMM_SELF = 0x44000001

    val MPI_GROUP_EMPTY = 0x48000000

    val MPI_WIN_NULL = 0x20000000

    val MPI_FILE_NULL = Pointer.NULL.asInstanceOf[MPI_File]

    val MPI_MAX = 0x58000001
    val MPI_MIN = 0x58000002
    val MPI_SUM = 0x58000003
    val MPI_PROD = 0x58000004
    val MPI_LAND = 0x58000005
    val MPI_BAND = 0x58000006
    val MPI_LOR = 0x58000007
    val MPI_BOR = 0x58000008
    val MPI_LXOR = 0x58000009
    val MPI_BXOR = 0x5800000a
    val MPI_MINLOC = 0x5800000b
    val MPI_MAXLOC = 0x5800000c
    val MPI_REPLACE = 0x5800000d

    val MPI_TAG_UB = 0x64400001
    val MPI_HOST = 0x64400003
    val MPI_IO = 0x64400005
    val MPI_WTIME_IS_GLOBAL = 0x64400007
    val MPI_UNIVERSE_SIZE = 0x64400009
    val MPI_LASTUSEDCODE = 0x6440000b
    val MPI_APPNUM = 0x6440000d
    val MPI_WIN_BASE = 0x66000001
    val MPI_WIN_SIZE = 0x66000003
    val MPI_WIN_DISP_UNIT = 0x66000005

    val MPI_COMM_NULL = 0x04000000
    val MPI_OP_NULL = 0x18000000
    val MPI_GROUP_NULL = 0x08000000
    val MPI_DATATYPE_NULL = 0x0c000000
    val MPI_REQUEST_NULL = 0x2c000000
    val MPI_ERRHANDLER_NULL = 0x14000000

    val MPI_MAX_DATAREP_STRING = 128
    val MPI_MAX_PROCESSOR_NAME = 128
    val MPI_MAX_ERROR_STRING = 1024
    val MPI_MAX_PORT_NAME = 256
    val MPI_MAX_OBJECT_NAME = 128
    val MPI_UNDEFINED = -32766
    val MPI_KEYVAL_INVALID = 0x24000000
    val MPI_BSEND_OVERHEAD = 88

    val MPI_BOTTOM = Pointer.pointerToAddress(0, classOf[Byte], noRelease)
    val MPI_UNWEIGHTED = Pointer.pointerToAddress(0, classOf[Int], noRelease)

    val MPI_PROC_NULL = -1
    val MPI_ANY_SOURCE = -2
    val MPI_ROOT = -3
    val MPI_ANY_TAG = -1

    val MPI_LOCK_EXCLUSIVE = 234
    val MPI_LOCK_SHARED = 235

    val MPI_ERRORS_ARE_FATAL = 0x54000000
    val MPI_ERRORS_RETURN = 0x54000001

    val MPI_INFO_NULL = 0x1c000000
    val MPI_MAX_INFO_KEY = 255
    val MPI_MAX_INFO_VAL = 1024

    val MPI_ORDER_C = 56
    val MPI_ORDER_FORTRAN = 57
    val MPI_DISTRIBUTE_BLOCK = 121
    val MPI_DISTRIBUTE_CYCLIC = 122
    val MPI_DISTRIBUTE_NONE = 123
    val MPI_DISTRIBUTE_DFLT_DARG = -49767

    val MPI_IN_PLACE = Pointer.pointerToAddress(-1, classOf[Byte], noRelease)

    val MPI_MODE_APPEND = 128
    val MPI_MODE_CREATE = 1
    val MPI_MODE_DELETE_ON_CLOSE = 16
    val MPI_MODE_EXCL = 64
    val MPI_MODE_NOCHECK = 1024
    val MPI_MODE_NOSTORE = 2048
    val MPI_MODE_NOPUT = 4096
    val MPI_MODE_NOPRECEDE = 8192
    val MPI_MODE_NOSUCCEED = 16384
    val MPI_MODE_RDONLY = 2
    val MPI_MODE_RDWR = 8
    val MPI_MODE_SEQUENTIAL = 256
    val MPI_MODE_UNIQUE_OPEN = 32
    val MPI_MODE_WRONLY = 4

    val MPI_DISPLACEMENT_CURRENT = -54278278L

    val MPI_SEEK_SET = 600
    val MPI_SEEK_CUR = 602
    val MPI_SEEK_END = 604

    val MPI_STATUS_IGNORE =
      Pointer.pointerToAddress(1, classOf[MPI_Status], noRelease)
    val MPI_STATUSES_IGNORE = MPI_STATUS_IGNORE
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

    val MPI_GRAPH = 1
    val MPI_CART = 2
    val MPI_DIST_GRAPH = 3

    val MPI_COMBINER_NAMED = 1
    val MPI_COMBINER_DUP = 2
    val MPI_COMBINER_CONTIGUOUS = 3
    val MPI_COMBINER_VECTOR = 4
    val MPI_COMBINER_HVECTOR_INTEGER = 5
    val MPI_COMBINER_HVECTOR = 6
    val MPI_COMBINER_INDEXED = 7
    val MPI_COMBINER_HINDEXED_INTEGER = 8
    val MPI_COMBINER_HINDEXED = 9
    val MPI_COMBINER_INDEXED_BLOCK = 10
    val MPI_COMBINER_STRUCT_INTEGER = 11
    val MPI_COMBINER_STRUCT = 12
    val MPI_COMBINER_SUBARRAY = 13
    val MPI_COMBINER_DARRAY = 14
    val MPI_COMBINER_F90_REAL = 15
    val MPI_COMBINER_F90_COMPLEX = 16
    val MPI_COMBINER_F90_INTEGER = 17
    val MPI_COMBINER_RESIZED = 18

    val MPI_SUCCESS = 0
    val MPI_ERR_BUFFER = 1
    val MPI_ERR_COUNT = 2
    val MPI_ERR_TYPE = 3
    val MPI_ERR_TAG = 4
    val MPI_ERR_COMM = 5
    val MPI_ERR_RANK = 6
    val MPI_ERR_ROOT = 7
    val MPI_ERR_TRUNCATE = 14
    val MPI_ERR_GROUP = 8
    val MPI_ERR_OP = 9
    val MPI_ERR_REQUEST = 19
    val MPI_ERR_TOPOLOGY = 10
    val MPI_ERR_DIMS = 11
    val MPI_ERR_ARG = 12
    val MPI_ERR_OTHER = 15
    val MPI_ERR_UNKNOWN = 13
    val MPI_ERR_INTERN = 16
    val MPI_ERR_IN_STATUS = 17
    val MPI_ERR_PENDING = 18
    val MPI_ERR_FILE = 27
    val MPI_ERR_ACCESS = 20
    val MPI_ERR_AMODE = 21
    val MPI_ERR_BAD_FILE = 22
    val MPI_ERR_FILE_EXISTS = 25
    val MPI_ERR_FILE_IN_USE = 26
    val MPI_ERR_NO_SPACE = 36
    val MPI_ERR_NO_SUCH_FILE = 37
    val MPI_ERR_IO = 32
    val MPI_ERR_READ_ONLY = 40
    val MPI_ERR_CONVERSION = 23
    val MPI_ERR_DUP_DATAREP = 24
    val MPI_ERR_UNSUPPORTED_DATAREP = 43
    val MPI_ERR_INFO = 28
    val MPI_ERR_INFO_KEY = 29
    val MPI_ERR_INFO_VALUE = 30
    val MPI_ERR_INFO_NOKEY = 31
    val MPI_ERR_NAME = 33
    val MPI_ERR_NO_MEM = 34
    val MPI_ERR_NOT_SAME = 35
    val MPI_ERR_PORT = 38
    val MPI_ERR_QUOTA = 39
    val MPI_ERR_SERVICE = 41
    val MPI_ERR_SPAWN = 42
    val MPI_ERR_UNSUPPORTED_OPERATION = 44
    val MPI_ERR_WIN = 45
    val MPI_ERR_BASE = 46
    val MPI_ERR_LOCKTYPE = 47
    val MPI_ERR_KEYVAL = 48
    val MPI_ERR_RMA_CONFLICT = 49
    val MPI_ERR_RMA_SYNC = 50
    val MPI_ERR_SIZE = 51
    val MPI_ERR_DISP = 52
    val MPI_ERR_ASSERT = 53
    val MPI_ERR_LASTCODE = 0x3fffffff

    type MPI_Comm_copy_attr_function = Callbacks.MPI_Comm_copy_attr_function
    def MPI_Comm_copy_attr_function(
      fn: CopyAttrFunction[MPI_Comm]): MPI_Comm_copy_attr_function =
      new Callbacks.MPI_Comm_copy_attr_function {
        def apply(
          t: MPI_Comm,
          keyval: Int,
          extraState: Pointer[_],
          attributeValIn: Pointer[_],
          attributeValOut: Pointer[Pointer[_]],
          flag: Pointer[Integer]) =
          fn(t, keyval, extraState, attributeValIn, attributeValOut, flag.as(classOf[Int]))
      }

    type MPI_Comm_delete_attr_function = Callbacks.MPI_Comm_delete_attr_function
    def MPI_Comm_delete_attr_function(
      fn: DeleteAttrFunction[MPI_Comm]): MPI_Comm_delete_attr_function =
      new Callbacks.MPI_Comm_delete_attr_function {
        def apply(
          t: MPI_Comm,
          keyval: Int,
          attributeVal: Pointer[_],
          extraState: Pointer[_]) =
          fn(t, keyval, attributeVal, extraState)
      }

    type MPI_Win_copy_attr_function = Callbacks.MPI_Win_copy_attr_function
    def MPI_Win_copy_attr_function(
      fn: CopyAttrFunction[MPI_Win]): MPI_Win_copy_attr_function =
      new Callbacks.MPI_Win_copy_attr_function {
        def apply(
          t: MPI_Win,
          keyval: Int,
          extraState: Pointer[_],
          attributeValIn: Pointer[_],
          attributeValOut: Pointer[Pointer[_]],
          flag: Pointer[Integer]) =
          fn(t, keyval, extraState, attributeValIn, attributeValOut, flag.as(classOf[Int]))
      }

    type MPI_Win_delete_attr_function = Callbacks.MPI_Win_delete_attr_function
    def MPI_Win_delete_attr_function(
      fn: DeleteAttrFunction[MPI_Win]): MPI_Win_delete_attr_function =
      new Callbacks.MPI_Win_delete_attr_function {
        def apply(
          t: MPI_Win,
          keyval: Int,
          attributeVal: Pointer[_],
          extraState: Pointer[_]) =
          fn(t, keyval, attributeVal, extraState)
      }

    type MPI_Type_copy_attr_function = Callbacks.MPI_Type_copy_attr_function
    def MPI_Type_copy_attr_function(
      fn: CopyAttrFunction[MPI_Datatype]): MPI_Type_copy_attr_function =
      new Callbacks.MPI_Type_copy_attr_function {
        def apply(
          t: MPI_Datatype,
          keyval: Int,
          extraState: Pointer[_],
          attributeValIn: Pointer[_],
          attributeValOut: Pointer[Pointer[_]],
          flag: Pointer[Integer]) =
          fn(t, keyval, extraState, attributeValIn, attributeValOut, flag.as(classOf[Int]))
      }

    type MPI_Type_delete_attr_function = Callbacks.MPI_Type_delete_attr_function
    def MPI_Type_delete_attr_function(
      fn: DeleteAttrFunction[MPI_Datatype]): MPI_Type_delete_attr_function =
      new Callbacks.MPI_Type_delete_attr_function {
        def apply(
          t: MPI_Datatype,
          keyval: Int,
          attributeVal: Pointer[_],
          extraState: Pointer[_]) =
          fn(t, keyval, attributeVal, extraState)
      }

    type MPI_Comm_errhandler_function = Callbacks.MPI_Comm_errhandler_function
    def MPI_Comm_errhandler_function(
      fn: ErrhandlerFunction[MPI_Comm]): MPI_Comm_errhandler_function =
      new Callbacks.MPI_Comm_errhandler_function {
        def apply(t: Pointer[Integer], errcode: Pointer[Integer]) {
          fn(t.as(classOf[Int]), errcode.as(classOf[Int]))
        }
      }

    type MPI_Win_errhandler_function = Callbacks.MPI_Win_errhandler_function
    def MPI_Win_errhandler_function(
      fn: ErrhandlerFunction[MPI_Win]): MPI_Win_errhandler_function =
      new Callbacks.MPI_Win_errhandler_function {
        def apply(t: Pointer[Integer], errcode: Pointer[Integer]) {
          fn(t.as(classOf[Int]), errcode.as(classOf[Int]))
        }
      }

    type MPI_File_errhandler_function = Callbacks.MPI_File_errhandler_function
    def MPI_File_errhandler_function(
      fn: ErrhandlerFunction[MPI_File]): MPI_File_errhandler_function =
      new Callbacks.MPI_File_errhandler_function {
        def apply(t: Pointer[Integer], errcode: Pointer[Integer]) {
          fn(t.as(classOf[Int]), errcode.as(classOf[Int]))
        }
      }

    type MPI_User_function = Callbacks.MPI_User_function
    def MPI_User_function(fn: UserFunction[MPI_Datatype]): MPI_User_function =
      new Callbacks.MPI_User_function {
        def apply(
          invec: Pointer[_],
          inoutvec: Pointer[_],
          len: Pointer[Integer],
          datatype: Pointer[Integer]) {
          fn(invec, inoutvec, len.as(classOf[Int]), datatype.as(classOf[Int]))
        }
      }

    type MPI_Grequest_cancel_function = Callbacks.MPI_Grequest_cancel_function
    def MPI_Grequest_cancel_function(
      fn: GrequestCancelFunction): MPI_Grequest_cancel_function =
      new Callbacks.MPI_Grequest_cancel_function {
        def apply(extraState: Pointer[_], complete: Int): Int =
          fn(extraState, complete)
      }

    type MPI_Grequest_free_function = Callbacks.MPI_Grequest_free_function
    def MPI_Grequest_free_function(
      fn: GrequestFreeFunction): MPI_Grequest_free_function =
      new Callbacks.MPI_Grequest_free_function {
        def apply(extraState: Pointer[_]): Int =
          fn(extraState)
      }

    type MPI_Grequest_query_function = Callbacks.MPI_Grequest_query_function
    def MPI_Grequest_query_function(
      fn: GrequestQueryFunction[MPI_Status]): MPI_Grequest_query_function =
      new Callbacks.MPI_Grequest_query_function {
        def apply(extraState: Pointer[_], status: Pointer[_]) =
          fn(extraState, MpiStatus(status.as(classOf[MPI_Status])))
      }

    type MPI_Datarep_conversion_function = Callbacks.MPI_Datarep_conversion_function
    def MPI_Datarep_conversion_function(
      fn: DatareqConversionFunction[MPI_Datatype, MPI_Offset]):
        MPI_Datarep_conversion_function =
      new Callbacks.MPI_Datarep_conversion_function {
        def apply(
          userbuf: Pointer[_],
          datatype: MPI_Datatype,
          count: Int,
          filebuf: Pointer[_],
          position: MPI_Offset,
          extraState: Pointer[_]): Int =
          fn(userbuf, datatype, count, filebuf, position, extraState)
      }

    type MPI_Datarep_extent_function = Callbacks.MPI_Datarep_extent_function
    def MPI_Datarep_extent_function(
      fn: DatareqExtentFunction[MPI_Datatype, MPI_Aint]):
        MPI_Datarep_extent_function =
      new Callbacks.MPI_Datarep_extent_function {
        def apply(
          datatype: MPI_Datatype,
          fileExtent: Pointer[MPI_Aint],
          extraState: Pointer[_]): Int =
          fn(datatype, fileExtent, extraState)
      }

    val MPI_COMM_NULL_COPY_FN = Pointer.pointerToAddress(
      0, classOf[Callbacks.MPI_Comm_copy_attr_function], noRelease)
    val MPI_COMM_NULL_DELETE_FN = Pointer.pointerToAddress(
      0, classOf[MPI_Comm_delete_attr_function], noRelease)
    private lazy val commDupFn: MPI_Comm_copy_attr_function =
      MPI_Comm_copy_attr_function((
        t: MPI_Comm,
        keyval: Int,
        extraState: Pointer[_],
        attributeValIn: Pointer[_],
        attributeValOut: Pointer[Pointer[_]],
        flag: Pointer[Int]) => {
        flag(0) = 1
        attributeValOut.set(attributeValIn)
        MPI_SUCCESS
      })
    lazy val MPI_COMM_DUP_FN = Pointer.pointerTo(commDupFn)
    val MPI_WIN_NULL_COPY_FN = Pointer.pointerToAddress(
      0, classOf[Callbacks.MPI_Win_copy_attr_function], noRelease)
    val MPI_WIN_NULL_DELETE_FN = Pointer.pointerToAddress(
      0, classOf[Callbacks.MPI_Win_delete_attr_function], noRelease)
    private lazy val winDupFn: MPI_Win_copy_attr_function =
      MPI_Win_copy_attr_function((
        t: MPI_Win,
        keyval: Int,
        extraState: Pointer[_],
        attributeValIn: Pointer[_],
        attributeValOut: Pointer[Pointer[_]],
        flag: Pointer[Int]) => {
        flag(0) = 1
        attributeValOut.set(attributeValIn)
        MPI_SUCCESS
      })
    lazy val MPI_WIN_DUP_FN = Pointer.pointerTo(winDupFn)
    val MPI_TYPE_NULL_COPY_FN = Pointer.pointerToAddress(
      0, classOf[Callbacks.MPI_Type_copy_attr_function], noRelease)
    val MPI_TYPE_NULL_DELETE_FN = Pointer.pointerToAddress(
      0, classOf[Callbacks.MPI_Type_delete_attr_function], noRelease)
    private lazy val typeDupFn: MPI_Type_copy_attr_function =
      MPI_Type_copy_attr_function((
        t: MPI_Datatype,
        keyval: Int,
        extraState: Pointer[_],
        attributeValIn: Pointer[_],
        attributeValOut: Pointer[Pointer[_]],
        flag: Pointer[Int]) => {
        flag(0) = 1
        attributeValOut.set(attributeValIn)
        MPI_SUCCESS
      })
    lazy val MPI_TYPE_DUP_FN = Pointer.pointerTo(typeDupFn)

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
    @native def MPI_Reduce_scatter_block(sendbuf: Pointer[_],
      recvbuf: Pointer[_], recvcount: Int, datatype: MPI_Datatype, op: MPI_Op,
      comm: MPI_Comm): Int
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
    @native def MPI_Dist_graph_create(comm_old: MPI_Comm, n: Int,
      sources: Pointer[Int], degrees: Pointer[Int], destinations: Pointer[Int],
      weights: Pointer[Int], info: MPI_Info, reorder: Int,
      comm_dist_graph: Pointer[MPI_Comm]): Int
    @native def MPI_Dist_graph_create_adjacent(comm_old: MPI_Comm,
      indegree: Int, sources: Pointer[Int], sourceweights: Pointer[Int],
      outdegree: Int, destinations: Pointer[Int], destweights: Pointer[Int],
      info: MPI_Info, reorder: Int, comm_dist_graph: Pointer[MPI_Comm]): Int
    @native def MPI_Dist_graph_neighbors(comm: MPI_Comm, maxindegree: Int,
      sources: Pointer[Int], sourceweights: Pointer[Int], maxoutdegree: Int,
      destinations: Pointer[Int], destweights: Pointer[Int]): Int
    @native def MPI_Dist_graph_neighbors_count(comm: MPI_Comm,
      indegree: Pointer[Int], outdegree: Pointer[Int],
      weighted: Pointer[Int]): Int
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
