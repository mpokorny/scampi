//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.collection.mutable
import scala.ref.WeakReference
import scala.sys.SystemProperties
import scala.util.Properties
import org.bridj.Pointer
import java.util.concurrent.ConcurrentMap

trait CommComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  val anyTag: Int = mpi3.lib.MPI_ANY_TAG

  val anySource: Int = mpi3.lib.MPI_ANY_SOURCE

  def dimsCreate(nnodes: Int, dims: Seq[Int]): Seq[Int] = {
    require(dims.forall(_ >= 0), "Elements in 'dims' must be non-negative")
    require({
      val dsz = (1 /: dims) ((acc, d) => acc * (if (d > 0) d else 1))
      nnodes % dsz == 0
    })
    if (dims.length > 0) {
      val result = Pointer.pointerToInts(dims:_*).as(classOf[Int])
      try {
        mpiCall(mpi3.lib.MPI_Dims_create(nnodes, dims.length, result))
        result.getInts
      } finally result.release()
    } else {
      Seq.empty
    }
  }

  sealed class Comm protected ()
      extends mpi3.Named
      with mpi3.CommCache
      with mpi3.WithErrHandler {

    protected final val handlePtr: Pointer[mpi3.lib.MPI_Comm] = {
      val result = mpi3.allocateComm()
      result(0) = mpi3.lib.MPI_COMM_NULL
      result
    }

    final def handle = handlePtr(0)

    // override def equals(other: Any): Boolean = {
    //   other.isInstanceOf[Comm] &&
    //   other.asInstanceOf[Comm].handle == handle
    // }
    // override def hashCode: Int = handle ##

    final def fromMpiHandle(h: mpi3.lib.MPI_Comm): Comm = Comm(h)

    final def mpiSetAttr(keyval: Int, attribute: Pointer[_]) {
      mpiCall(mpi3.lib.MPI_Comm_set_attr(handle, keyval, attribute))
    }

    final def mpiGetAttr(
        keyval: Int,
        attribute: Pointer[Pointer[_]],
        flag: Pointer[Int]) {
      mpiCall(mpi3.lib.MPI_Comm_get_attr(handle, keyval, attribute, flag))
    }

    final def mpiDeleteAttr(keyval: Int) {
      mpiCall(mpi3.lib.MPI_Comm_delete_attr(handle, keyval))
    }

    protected final val selfException = mpi3.CommException.curried(this)

    protected def mpiCall(c: => Int) = mpi3.mpiCall(c, selfException)

    def free() {
      if (!isNull) {
        Comm.remove(this)
        mpiCall(mpi3.lib.MPI_Comm_free(handlePtr))
      }
    }

    def disconnect() {
      if (!isNull) {
        Comm.remove(this)
        mpiCall(mpi3.lib.MPI_Comm_disconnect(handlePtr))
      }
    }

    final lazy val group: mpi3.Group =
      withOutVar { group: Pointer[mpi3.lib.MPI_Group] =>
        mpiCall(mpi3.lib.MPI_Comm_group(handle, group))
        mpi3.Group(group(0))
      }

    final lazy val size: Int = withOutVar { size: Pointer[Int] =>
      mpiCall(mpi3.lib.MPI_Comm_size(handle, size))
      size(0)
    }

    final lazy val rank: Int = withOutVar { rank: Pointer[Int] =>
      mpiCall(mpi3.lib.MPI_Comm_rank(handle, rank))
      rank(0)
    }

    def compare(other: Comm): Comparison.Comparison =
      withOutVar { comp: Pointer[Int] =>
        mpiCall(mpi3.lib.MPI_Comm_compare(handle, other.handle, comp))
        Comparison(comp(0))
      }

    def dup: Comm = withOutVar { comm: Pointer[mpi3.lib.MPI_Comm] =>
      mpiCall(mpi3.lib.MPI_Comm_dup(handle, comm))
      Comm(comm(0))
    }

    def create(group: mpi3.Group): Option[Comm] =
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        mpiCall(mpi3.lib.MPI_Comm_create(handle, group.handle, newComm))
        if (newComm(0) != mpi3.lib.MPI_COMM_NULL) Some(Comm(newComm(0)))
        else None
      }

    def split(colorOpt: Option[Int], key: Int): Option[Comm] =
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        val color = colorOpt.getOrElse(mpi3.lib.MPI_UNDEFINED)
        mpiCall(mpi3.lib.MPI_Comm_split(handle, color, key, newComm))
        if (newComm(0) != mpi3.lib.MPI_COMM_NULL) Some(Comm(newComm(0)))
        else None
      }

    def barrier() { mpiCall(mpi3.lib.MPI_Barrier(handle)) }

    def send(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int) {
      mpiCall(
        mpi3.lib.MPI_Send(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle))
    }

    def recv(buff: mpi3.ValueBuffer[_], source: Int, tag: Int): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_Recv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          source,
          tag,
          handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doRecv(buff: mpi3.ValueBuffer[_], source: Int, tag: Int) {
      mpiCall(
        mpi3.lib.MPI_Recv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          source,
          tag,
          handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def imrecv(buff: mpi3.ValueBuffer[_], message: mpi3.Message): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Imrecv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          message.handlePtr,
          result.handlePtr))
      result
    }

    def mrecv(buff: mpi3.ValueBuffer[_], message: mpi3.Message): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_Mrecv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          message.handlePtr,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doMrecv(buff: mpi3.ValueBuffer[_], message: mpi3.Message) {
      mpiCall(
        mpi3.lib.MPI_Mrecv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          message.handlePtr,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def bsend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int) {
      mpiCall(
        mpi3.lib.MPI_Bsend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle))
    }

    def ssend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int) {
      mpiCall(
        mpi3.lib.MPI_Ssend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle))
    }

    def rsend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int) {
      mpiCall(
        mpi3.lib.MPI_Rsend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle))
    }

    def isend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Isend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def ibsend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Ibsend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def issend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Issend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def irsend(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Irsend(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def irecv(buff: mpi3.ValueBuffer[_], source: Int, tag: Int): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Irecv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          source,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def probe(source: Int, tag: Int): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_Probe(source, tag, handle, Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doProbe(source: Int, tag: Int) {
      mpiCall(
        mpi3.lib.MPI_Probe(source, tag, handle, mpi3.lib.MPI_STATUS_IGNORE))
    }

    def iprobe(source: Int, tag: Int): Option[mpi3.Status] = {
      val status = mpi3.newStatus()
      withOutVar { flag: Pointer[Int] =>
        mpiCall(
          mpi3.lib.MPI_Iprobe(
            source,
            tag,
            handle,
            flag,
            Pointer.pointerTo(status(0))))
        if (flag(0) != 0) Some(new mpi3.Status(status(0)))
        else None
      }
    }

    def doIprobe(source: Int, tag: Int): Boolean =
      withOutVar { flag: Pointer[Int] =>
        mpiCall(
          mpi3.lib.MPI_Iprobe(
            source,
            tag,
            handle,
            flag,
            mpi3.lib.MPI_STATUS_IGNORE))
        flag(0) != 0
      }

    def improbe(source: Int, tag: Int): Option[(mpi3.Message, mpi3.Status)] = {
      val status = mpi3.newStatus()
      val message = new mpi3.Message
      withOutVar { flag: Pointer[Int] =>
        mpiCall(
          mpi3.lib.MPI_Improbe(
            source,
            tag,
            handle,
            flag,
            message.handlePtr,
            Pointer.pointerTo(status(0))))
        if (flag(0) != 0) Some((message, new mpi3.Status(status(0))))
        else None
      }
    }

    def doImprobe(source: Int, tag: Int): Option[mpi3.Message] = {
      val message = new mpi3.Message
      withOutVar { flag: Pointer[Int] =>
        mpiCall(
          mpi3.lib.MPI_Improbe(
            source,
            tag,
            handle,
            flag,
            message.handlePtr,
            mpi3.lib.MPI_STATUS_IGNORE))
        if (flag(0) != 0) Some((new mpi3.Status(status(0)), message))
        else None
      }
    }

    def mprobe(source: Int, tag: Int): Option[(mpi3.Message, mpi3.Status)] = {
      val status = mpi3.newStatus()
      val message = new mpi3.Message
      mpiCall(
        mpi3.lib.MPI_Mprobe(
          source,
          tag,
          handle,
          message.handlePtr,
          Pointer.pointerTo(status(0))))
      if (flag(0) != 0) Some((message, new mpi3.Status(status(0))))
      else None
    }

    def doMprobe(source: Int, tag: Int): Option[mpi3.Message] = {
      val message = new mpi3.Message
      mpiCall(
        mpi3.lib.MPI_Improbe(
          source,
          tag,
          handle,
          message.handlePtr,
          mpi3.lib.MPI_STATUS_IGNORE))
      if (flag(0) != 0) Some((new mpi3.Status(status(0)), message))
      else None
    }

    def sendInit(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int):
        mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Send_init(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def bsendInit(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int):
        mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Bsend_init(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def ssendInit(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int):
        mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Ssend_init(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def rsendInit(buff: mpi3.ValueBuffer[_], dest: Int, tag: Int):
        mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Rsend_init(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def recvInit(buff: mpi3.ValueBuffer[_], source: Int, tag: Int):
        mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_Recv_init(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          source,
          tag,
          handle,
          result.handlePtr))
      result
    }

    def sendrecv(
        sendBuff: mpi3.ValueBuffer[_],
        dest: Int,
        sendTag: Int,
        recvBuff: mpi3.ValueBuffer[_],
        source: Int,
        recvTag: Int): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_Sendrecv(
          sendBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          dest,
          sendTag,
          recvBuff.pointer,
          recvBuff.valueCount,
          recvBuff.datatype.handle,
          source,
          recvTag,
          handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doSendrecv(
        sendBuff: mpi3.ValueBuffer[_],
        dest: Int,
        sendTag: Int,
        recvBuff: mpi3.ValueBuffer[_],
        source: Int,
        recvTag: Int) {
      mpiCall(
        mpi3.lib.MPI_Sendrecv(
          sendBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          dest,
          sendTag,
          recvBuff.pointer,
          recvBuff.valueCount,
          recvBuff.datatype.handle,
          source,
          recvTag,
          handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def sendrecvReplace(
        buff: mpi3.ValueBuffer[_],
        dest: Int,
        sendTag: Int,
        source: Int,
        recvTag: Int): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_Sendrecv_replace(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          sendTag,
          source,
          recvTag,
          handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }
    def doSendrecvReplace(
        buff: mpi3.ValueBuffer[_],
        dest: Int,
        sendTag: Int,
        source: Int,
        recvTag: Int) {
      mpiCall(
        mpi3.lib.MPI_Sendrecv_replace(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          dest,
          sendTag,
          source,
          recvTag,
          handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    // allgather()
    protected def allgather(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvCount: Int) {
      mpiCall(
        mpi3.lib.MPI_Allgather(
          sendBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          recvBuff.pointer,
          recvCount,
          recvBuff.datatype.handle,
          handle))
    }

    private def getBlockPars(
        bufflen: Int,
        blocks: Seq[mpi3.Block],
        counts: Pointer[Int],
        displs: Pointer[Int]) {
      var idx = 0
      blocks foreach {
        case mpi3.Block(length, displacement) => {
          require(
            0 <= displacement && displacement + length <= bufflen,
            mpi3.bufferCompatibilityErrorMsg)
          counts.set(idx, length)
          displs.set(idx, displacement)
          idx += 1
        }
      }
    }

    // allgatherv()
    protected def allgatherv(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      val buffer =
        if (recvBlocks.length > 0)
          Pointer.allocateInts(2 * recvBlocks.length).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val recvcounts = buffer
        val displs =
          if (buffer != Pointer.NULL) buffer.next(recvBlocks.length) else buffer
        getBlockPars(recvBuff.valueCount, recvBlocks, recvcounts, displs)
        mpiCall(
          mpi3.lib.MPI_Allgatherv(
            sendBuff.pointer,
            sendBuff.valueCount,
            sendBuff.datatype.handle,
            recvBuff.pointer,
            recvcounts,
            displs,
            recvBuff.datatype.handle,
            handle))
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    // alltoall()
    protected def alltoall(
        sendBuff: mpi3.ValueBuffer[_],
        sendCount: Int,
        recvBuff: mpi3.ValueBuffer[_],
        recvCount: Int) {
      require(sendCount <= sendBuff.valueCount, mpi3.countExceedsLengthErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Alltoall(
          sendBuff.pointer,
          sendCount,
          sendBuff.datatype.handle,
          recvBuff.pointer,
          recvCount,
          recvBuff.datatype.handle,
          handle))
    }

    // alltoallv()
    protected def alltoallv(
        sendBuff: mpi3.ValueBuffer[_],
        sendBlocks: Seq[mpi3.Block],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      require(sendBlocks.length == size, mpi3.numBlocksUnequalToSizeErrorMsg)
      val buffer =
        if (recvBlocks.length > 0 || sendBlocks.length > 0)
          Pointer.allocateInts(
            2 * recvBlocks.length + 2 * sendBlocks.length).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val recvcounts = buffer
        val rdispls =
          if (buffer != Pointer.NULL) buffer.next(recvBlocks.length)
          else buffer
        val sendcounts =
          if (buffer != Pointer.NULL) buffer.next(2 * recvBlocks.length)
          else buffer
        val sdispls =
          if (buffer != Pointer.NULL)
            buffer.next(sendBlocks.length + 2 * recvBlocks.length)
          else
            buffer
        getBlockPars(recvBuff.valueCount, recvBlocks, recvcounts, rdispls)
        getBlockPars(sendBuff.valueCount, sendBlocks, sendcounts, sdispls)
        mpiCall(
          mpi3.lib.MPI_Alltoallv(
            sendBuff.pointer,
            sendcounts,
            sdispls,
            sendBuff.datatype.handle,
            recvBuff.pointer,
            recvcounts,
            rdispls,
            recvBuff.datatype.handle,
            handle))
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    // alltoallw()
    protected def alltoallw(
        sendBuff: mpi3.ValueBuffer[_],
        sendStructs: Seq[mpi3.StructBlock[_]],
        recvBuff: mpi3.ValueBuffer[_],
        recvStructs: Seq[mpi3.StructBlock[_]]) {
      require(sendStructs.length == size, mpi3.numBlocksUnequalToSizeErrorMsg)
      require(
        StructBlock.displacementsAreValid(sendStructs),
        mpi3.structBlocksAlignmentErrorMsg)
      require(
        StructBlock.displacementsAreValid(recvStructs),
        mpi3.structBlocksAlignmentErrorMsg)
      def getStructPars(
          buff: mpi3.ValueBuffer[_],
          structs: Seq[mpi3.StructBlock[_]],
          counts: Pointer[Int],
          displs: Pointer[Int],
          types: Pointer[mpi3.lib.MPI_Datatype]) {
        var idx = 0
        structs foreach {
          case mpi3.StructBlock(datatype, length, optDisp) => {
            val displacement = optDisp.get
            require(
              0 <= displacement &&
                displacement + length * datatype.extent.range <= buff.region.size,
              mpi3.bufferCompatibilityErrorMsg)
            counts(idx) = length
            displs(idx) = displacement.toInt
            types(idx) = datatype.handle
            idx += 1
          }
        }
      }
      // Note that "sdispls" and "rdispls" arguments to MPI_Alltoallw
      // are arrays of type int (not Aint).
      val intBuffer =
        if (sendStructs.length > 0 || recvStructs.length > 0)
          Pointer.allocateInts(
            2 * (sendStructs.length + recvStructs.length)).as(classOf[Int])
        else
          nullPointer[Int]
      val dtsBuffer =
        if (sendStructs.length > 0 || recvStructs.length > 0)
          mpi3.allocateDatatype(sendStructs.length + recvStructs.length)
        else
          nullPointer[mpi3.lib.MPI_Datatype]
      try {
        var off  = 0
        val recvcounts = intBuffer
        off += recvStructs.length
        val rdispls =
          if (intBuffer != Pointer.NULL) intBuffer.next(off) else intBuffer
        off += recvStructs.length
        val sendcounts =
          if (intBuffer != Pointer.NULL) intBuffer.next(off) else intBuffer
        off += sendStructs.length
        val sdispls =
          if (intBuffer != Pointer.NULL) intBuffer.next(off) else intBuffer
        off = 0
        val recvtypes = dtsBuffer
        off += recvStructs.length
        val sendtypes =
          if (dtsBuffer != Pointer.NULL) dtsBuffer.next(off) else dtsBuffer
        getStructPars(recvBuff, recvStructs, recvcounts, rdispls, recvtypes)
        getStructPars(sendBuff, sendStructs, sendcounts, sdispls, sendtypes)
        mpiCall(
          mpi3.lib.MPI_Alltoallw(
            sendBuff.pointer,
            sendcounts,
            sdispls,
            sendtypes,
            recvBuff.pointer,
            recvcounts,
            rdispls,
            recvtypes,
            handle))
      } finally {
        if (intBuffer != Pointer.NULL) intBuffer.release()
        if (dtsBuffer != Pointer.NULL) dtsBuffer.release()
      }
    }

    // allreduce()
    def allreduce(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op) {
      mpiCall(
        mpi3.lib.MPI_Allreduce(
          sendBuff.pointer,
          recvBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          op.handle,
          handle))
    }

    // reduceScatterBlock()
    def reduceScatterBlock(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvCount: Int,
        op: mpi3.Op) {
      require(
        sendBuff.valueCount == recvCount * size,
        mpi3.scatterBufferSizeErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Reduce_scatter_block(
          sendBuff.pointer,
          recvBuff.pointer,
          recvCount,
          sendBuff.datatype.handle,
          op.handle,
          handle))
    }

    // reduceScatter()
    def reduceScatter(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvCounts: Seq[Int],
        op: mpi3.Op) {
      require(
        sendBuff.valueCount == recvCounts.sum,
        mpi3.scatterBufferSizeErrorMsg)
      val rcvcnts = Pointer.pointerToInts(recvCounts:_*).as(classOf[Int])
      try {
        mpiCall(
          mpi3.lib.MPI_Reduce_scatter(
            sendBuff.pointer,
            recvBuff.pointer,
            rcvcnts,
            sendBuff.datatype.handle,
            op.handle,
            handle))
      } finally rcvcnts.release()
    }

    protected def setName(s: Pointer[Byte]) {
      mpiCall(mpi3.lib.MPI_Comm_set_name(handle, s))
    }

    protected def getName(buffer: Pointer[Byte]) {
      withOutVar { resultlen: Pointer[Int] =>
        mpiCall(mpi3.lib.MPI_Comm_get_name(handle, buffer, resultlen))
      }
    }

    type ErrHandlerType = CommErrHandler

    protected var currentErrHandler: CommErrHandler = CommErrHandler.Abort

    protected def mpiSetErrhandler(errhandler: mpi3.lib.MPI_Errhandler): Int =
      mpi3.lib.MPI_Comm_set_errhandler(handle, errhandler)

    def isNull: Boolean = handle == mpi3.lib.MPI_COMM_NULL

    // def encode(codec: mpi3.PackedCodec, blocks: mpi3.ValueSeq[_]*): PackedValueBuffer[_] =
    //   codec.encodeTo(this, blocks:_*)

    // def packedValueBuffer[_](codec: mpi3.PackedCodec) =
    //   new PackedValueBuffer[_](
    //     this,
    //     mpi3.CommBuffer(codec.maxSize),
    //     codec.maxSize.toInt,
    //     codec)
  }

  sealed class IntraComm protected[scampi3] () extends Comm {

    override def dup: IntraComm = super.dup.asInstanceOf[IntraComm]

    override def create(group: mpi3.Group): Option[IntraComm] =
      super.create(group).map(_.asInstanceOf[IntraComm])

    override def split(colorOpt: Option[Int], key: Int): Option[IntraComm] =
      super.split(colorOpt, key).map(_.asInstanceOf[IntraComm])

    def createIntercomm(
        localLeader: Int,
        peerComm: Comm,
        remoteLeader: Int,
        tag: Int): InterComm =
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        mpiCall(
          mpi3.lib.MPI_Intercomm_create(
            handle,
            localLeader,
            peerComm.handle,
            remoteLeader,
            tag,
            newComm))
        Comm(newComm(0)).asInstanceOf[InterComm]
      }

    def spawn(
        command: String,
        argv: Seq[String],
        maxprocs: Int,
        info: Info,
        root: Int): (InterComm, Seq[Int]) = {
      require(maxprocs > 0, "maxprocs must be positive")
      val commandp = Pointer.pointerToCString(command).as(classOf[Byte])
      val argvp =
        Pointer.pointerToCStrings((argv :+ ""):_*).as(classOf[Pointer[Byte]])
      argvp(argv.length) = nullPointer
      val errcodes = Pointer.allocateInts(maxprocs).as(classOf[Int])
      val interComm = allocateComm()
      try {
        mpiCall(
          mpi3.lib.MPI_Comm_spawn(
            commandp,
            argvp,
            maxprocs,
            info.handle,
            root,
            handle,
            interComm,
            errcodes))
        (Comm(interComm(0)).asInstanceOf[InterComm], errcodes.getInts)
      } finally {
        errcodes.release()
        interComm.release()
        argvp.release()
        commandp.release()
      }
    }

    def spawnObjects(
        objectName: String,
        objectArgs: Seq[String],
        scalaArgs: Seq[String],
        maxprocs: Int,
        info: Info,
        root: Int): (InterComm, Seq[Int]) = {
      val properties = new SystemProperties
      val pathSeparator = properties("path.separator")
      val currentClasspath =
        properties("java.class.path").split(pathSeparator).distinct
      val classpathArgIdx = scalaArgs.indexOf("-classpath")
      val scalaArgs1 = {
        if (classpathArgIdx != -1) {
          val givenClasspath =
            scalaArgs(classpathArgIdx + 1).split(pathSeparator).distinct
          val addlClasspath = currentClasspath.diff(givenClasspath)
          val classpath =
            (givenClasspath ++ addlClasspath).mkString(pathSeparator)
          scalaArgs.updated(classpathArgIdx + 1, classpath)
        } else {
          scalaArgs ++ List("-classpath", properties("java.class.path"))
        }
      }
      val scalaArgs2 =
        scalaArgs1.filterNot(_.startsWith("-howtorun:")) :+ "-howtorun:object"
      spawn(
        "scala",
        scalaArgs2 ++ (objectName +: objectArgs),
        maxprocs,
        info,
        root)
    }

    // gather(), for root
    def gatherIn(sendBuff: mpi3.ValueBuffer[_], recvBuff: mpi3.ValueBuffer[_]) {
      require(recvBuff.valueCount >= size, mpi3.gatherBufferSizeErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Gather(
          sendBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          recvBuff.pointer,
          recvBuff.valueCount / size,
          recvBuff.datatype.handle,
          rank, handle))
    }

    // gather(), for non-root
    def gatherOut(buff: mpi3.ValueBuffer[_], root: Int) {
      mpiCall(
        mpi3.lib.MPI_Gather(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          root,
          handle))
    }

    // gather(), for any rank
    def gather(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        root: Int) {
      if (root == rank) gatherIn(sendBuff, recvBuff)
      else gatherOut(sendBuff, root)
    }

    // gatherv(), for root
    def gathervIn(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      require(recvBlocks.length == size, mpi3.gatherBufferSizeErrorMsg)
      assume(recvBlocks.length > 0)
      val buffer = Pointer.allocateInts(2 * recvBlocks.length).as(classOf[Int])
      try {
        val recvcounts = buffer
        val displs = buffer.next(recvBlocks.length)
        var idx = 0
        recvBlocks.foreach( _ match {
          case mpi3.Block(length, displacement) => {
            require(
              0 <= displacement && displacement + length <= recvBuff.valueCount,
              mpi3.bufferCompatibilityErrorMsg)
            recvcounts(idx) = length
            displs(idx) = displacement
            idx += 1
          }
        })
        mpiCall(
          mpi3.lib.MPI_Gatherv(
            sendBuff.pointer,
            sendBuff.valueCount,
            sendBuff.datatype.handle,
            recvBuff.pointer,
            recvcounts,
            displs,
            recvBuff.datatype.handle,
            rank,
            handle))
      } finally buffer.release()
    }

    // gatherv(), for non-root
    def gathervOut(buff: mpi3.ValueBuffer[_], root: Int) {
      mpiCall(
        mpi3.lib.MPI_Gatherv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          nullPointer[Byte],
          nullPointer[Int],
          nullPointer[Int],
          mpi3.lib.MPI_DATATYPE_NULL,
          root,
          handle))
    }

    // gatherv(), for any rank
    def gatherv(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block],
        root: Int) {
      if (root == rank) gathervIn(sendBuff, recvBuff, recvBlocks)
      else gathervOut(sendBuff, root)
    }

    // scatter(), for root
    def scatterOut(
        sendBuff: mpi3.ValueBuffer[_],
        sendCount: Int,
        recvBuff: mpi3.ValueBuffer[_]) {
      require(
        sendCount * size <= sendBuff.valueCount,
        mpi3.scatterBufferSizeErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Scatter(
          sendBuff.pointer,
          sendCount,
          sendBuff.datatype.handle,
          recvBuff.pointer,
          recvBuff.valueCount,
          recvBuff.datatype.handle,
          rank,
          handle))
    }

    // scatter(), for non-root
    def scatterIn(buff: mpi3.ValueBuffer[_], root: Int) {
      mpiCall(
        mpi3.lib.MPI_Scatter(
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          root,
          handle))
    }

    // scatter(), for any rank
    def scatter(
        sendBuff: mpi3.ValueBuffer[_],
        sendCount: Int,
        recvBuff: mpi3.ValueBuffer[_],
        root: Int) {
      if (root == rank) scatterOut(sendBuff, sendCount, recvBuff)
      else scatterIn(recvBuff, root)
    }

    // scatterv(), for root
    def scattervOut(
        sendBuff: mpi3.ValueBuffer[_],
        sendBlocks: Seq[mpi3.Block],
        recvBuff: mpi3.ValueBuffer[_]) {
      require(sendBlocks.length == size, mpi3.scatterBufferSizeErrorMsg)
      assume(sendBlocks.length > 0)
      val buffer = Pointer.allocateInts(2 * sendBlocks.length).as(classOf[Int])
      try {
        val sendcounts = buffer
        val displs = buffer.next(sendBlocks.length)
        var idx = 0
        sendBlocks.foreach( _ match {
          case mpi3.Block(length, displacement) => {
            require(
              0 <= displacement && displacement + length <= sendBuff.valueCount,
              mpi3.bufferCompatibilityErrorMsg)
            sendcounts(idx) = length
            displs(idx) = displacement
            idx += 1
          }
        })
        mpiCall(
          mpi3.lib.MPI_Scatterv(
            sendBuff.pointer,
            sendcounts,
            displs,
            sendBuff.datatype.handle,
            recvBuff.pointer,
            recvBuff.valueCount,
            recvBuff.datatype.handle,
            rank,
            handle))
      } finally buffer.release()
    }

    // scatterv(), for non-root
    def scattervIn(buff: mpi3.ValueBuffer[_], root: Int) {
      mpiCall(
        mpi3.lib.MPI_Scatterv(
          nullPointer[Byte],
          nullPointer[Int],
          nullPointer[Int],
          mpi3.lib.MPI_DATATYPE_NULL,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          root,
          handle))
    }

    // scatterv(), for any rank
    def scatterv(
        sendBuff: mpi3.ValueBuffer[_],
        sendBlocks: Seq[mpi3.Block],
        recvBuff: mpi3.ValueBuffer[_],
        root: Int) {
      if (root == rank) scattervOut(sendBuff, sendBlocks, recvBuff)
      else scattervIn(recvBuff, root)
    }

    // allgather()
    def allgather(sendBuff: mpi3.ValueBuffer[_], recvBuff: mpi3.ValueBuffer[_]) {
      allgather(sendBuff, recvBuff, recvBuff.valueCount / size)
    }

    // allgatherv()
    override def allgatherv(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      require(recvBlocks.length == size, mpi3.gatherBufferSizeErrorMsg)
      super.allgatherv(sendBuff, recvBuff, recvBlocks)
    }

    // alltoall()
    override def alltoall(
        sendBuff: mpi3.ValueBuffer[_],
        sendCount: Int,
        recvBuff: mpi3.ValueBuffer[_],
        recvCount: Int) {
      require(
        recvCount * size <= recvBuff.valueCount,
        mpi3.gatherBufferSizeErrorMsg)
      super.alltoall(sendBuff, sendCount, recvBuff, recvCount)
    }

    // alltoallv()
    override def alltoallv(
        sendBuff: mpi3.ValueBuffer[_],
        sendBlocks: Seq[mpi3.Block],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      require(recvBlocks.length == size, mpi3.gatherBufferSizeErrorMsg)
      super.alltoallv(sendBuff, sendBlocks, recvBuff, recvBlocks)
    }

    // alltoallw()
    override def alltoallw(
        sendBuff: mpi3.ValueBuffer[_],
        sendStructs: Seq[mpi3.StructBlock[_]],
        recvBuff: mpi3.ValueBuffer[_],
        recvStructs: Seq[mpi3.StructBlock[_]]) {
      require(recvStructs.length == size, mpi3.gatherBufferSizeErrorMsg)
      super.alltoallw(sendBuff, sendStructs, recvBuff, recvStructs)
    }

    // reduce(), for root
    def reduceIn(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op) {
      require(
        sendBuff.valueCount == recvBuff.valueCount,
        mpi3.reduceBufferLengthErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Reduce(
          sendBuff.pointer,
          recvBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          op.handle,
          rank,
          handle))
    }

    // reduce(), for non-root
    def reduceOut(buff: mpi3.ValueBuffer[_], op: mpi3.Op, root: Int) {
      mpiCall(
        mpi3.lib.MPI_Reduce(
          buff.pointer,
          nullPointer[Byte],
          buff.valueCount,
          buff.datatype.handle,
          op.handle,
          root,
          handle))
    }

    // reduce(), for any rank
    def reduce(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op,
        root: Int) {
      if (root == rank) reduceIn(sendBuff, recvBuff, op)
      else reduceOut(sendBuff, op, root)
    }

    // bcast()
    def bcast(buff: mpi3.ValueBuffer[_], root: Int) {
      mpiCall(
        mpi3.lib.MPI_Bcast(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          root,
          handle))
    }

    // scan()
    def scan(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op) {
      require(
        sendBuff.valueCount == recvBuff.valueCount,
        mpi3.reduceBufferLengthErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Scan(
          sendBuff.pointer,
          recvBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          op.handle,
          handle))
    }

    // exscan(), for root
    def exscanRoot(sendBuff: mpi3.ValueBuffer[_], op: mpi3.Op) {
      require(rank == 0, "exscanRoot called from non-root rank")
      mpiCall(
        mpi3.lib.MPI_Exscan(
          sendBuff.pointer,
          nullPointer[Byte],
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          op.handle,
          handle))
    }

    // exscan(), for non-root
    def exscanNonRoot(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op) {
      require(rank != 0, "exscanNonRoot called from root rank")
      require(
        sendBuff.valueCount == recvBuff.valueCount,
        mpi3.reduceBufferLengthErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Exscan(
          sendBuff.pointer,
          recvBuff.pointer,
          sendBuff.valueCount,
          sendBuff.datatype.handle,
          op.handle,
          handle))
    }

    // exscan(), for any rank
    def exscan(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op) {
      if (rank == 0) exscanRoot(sendBuff, op)
      else exscanNonRoot(sendBuff, recvBuff, op)
    }

    def cartCreate(
        dims: Seq[(Int, Boolean)],
        reorder: Boolean): Option[CartComm] = {
      require(
        dims.forall(d => 0 < d._1),
        "Cartesion grid size in each dimension must be at least one")
      require(
        dims.foldLeft(1)((acc, d) => acc * d._1) <= size,
        "Cartesion grid size cannot exceed group size")
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        val buffer =
          if (dims.length > 0)
            Pointer.allocateInts(2 * dims.length).as(classOf[Int])
          else
            nullPointer[Int]
        try {
          val dimsp = buffer
          val periods =
            if (buffer != Pointer.NULL) buffer.next(dims.length) else buffer
          var idx = 0
          for (d <- dims) {
            dimsp(idx) = d._1
            periods(idx) = if (d._2) 1 else 0
            idx += 1
          }
          mpiCall(
            mpi3.lib.MPI_Cart_create(
              handle,
              dims.length,
              dimsp,
              periods,
              if (reorder) 1 else 0,
              newComm))
          if (newComm(0) != mpi3.lib.MPI_COMM_NULL)
            Some(Comm(newComm(0)).asInstanceOf[CartComm])
          else
            None
        } finally {
          if (buffer != Pointer.NULL) buffer.release()
        }
      }
    }

    def graphCreate(
        neighbors: Seq[Seq[Int]],
        reorder: Boolean): Option[GraphComm] = {
      require(neighbors.length <= size, "Graph size cannot exceed group size")
      require(
        neighbors.forall(ns => ns.forall(n => 0 <= n && n < neighbors.length)),
        "All edge destinations do not point within the set of sources")
      val flatNeighbors = neighbors.flatten
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        val buffer =
          if (neighbors.length > 0 || flatNeighbors.length > 0)
            Pointer.allocateInts(
              neighbors.length + flatNeighbors.length).as(classOf[Int])
          else
            nullPointer[Int]
        try {
          val index = buffer
          val edges =
            if (buffer != Pointer.NULL) buffer.next(neighbors.length)
            else buffer
          var nIdx = 0
          var fnIdx = 0
          for (ns <- neighbors) {
            for (n <- ns) {
              edges(fnIdx) = n
              fnIdx += 1
            }
            index(nIdx) = fnIdx
            nIdx += 1
          }
          mpiCall(
            mpi3.lib.MPI_Graph_create(
              handle,
              neighbors.length,
              index,
              edges,
              if (reorder) 1 else 0,
              newComm))
          if (newComm(0) != mpi3.lib.MPI_COMM_NULL)
            Some(Comm(newComm(0)).asInstanceOf[GraphComm])
          else
            None
        } finally {
          if (buffer != Pointer.NULL) buffer.release()
        }
      }
    }

    def distGraphCreate(
        edges: Seq[(Int, Seq[(Int, Option[Int])])],
        info: Info,
        reorder: Boolean): DistGraphComm = {
      val weighted =
        edges.nonEmpty && edges(0)._2.nonEmpty && edges(0)._2(0)._2.isDefined
      require(
        edges.forall(e => e._2.forall(d => d._2.isDefined == weighted)),
        "Inconsistent assignment of edge weights")
      require(
        !weighted ||
          edges.forall(e => e._2.forall(d => d._2.get >= 0)),
        "Invalid edge weight(s)")
      val numDests = edges.foldLeft(0)((acc, e) => e._2.length + acc)
      val buffer =
        if (edges.length > 0 || numDests > 0)
          Pointer.allocateInts(
            2 * edges.length + numDests * (if (weighted) 2 else 1)).
            as(classOf[Int])
        else
          nullPointer[Int]
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        try {
          val sources = buffer
          val degrees =
            if (buffer != Pointer.NULL) buffer.next(edges.length)
            else buffer
          val destinations =
            if (buffer != Pointer.NULL) buffer.next(2 * edges.length)
            else buffer
          val weights =
            if (weighted) {
              if (buffer != Pointer.NULL)
                buffer.next(2 * edges.length + numDests)
              else
                buffer
            }
            else {
              mpi3.lib.MPI_UNWEIGHTED
            }
          var srcIdx = 0
          var degIdx = 0
          var dstIdx = 0
          var wgtIdx = 0
          for (e <- edges) {
            e match {
              case (source, dests) => {
                sources(srcIdx) = source
                srcIdx += 1
                degrees(degIdx) = dests.length
                degIdx += 1
                for (d <- dests) {
                  d match {
                    case (dst, wgtOpt) => {
                      destinations(dstIdx) = dst
                      dstIdx += 1
                      wgtOpt.foreach(w => {
                        weights(wgtIdx) = w
                        wgtIdx += 1
                      })
                    }
                  }
                }
              }
            }
          }
          mpiCall(
            mpi3.lib.MPI_Dist_graph_create(
              handle,
              edges.length,
              sources,
              degrees,
              destinations,
              weights,
              info.handle,
              if (reorder) 1 else 0,
              newComm))
          Comm(newComm(0)).asInstanceOf[DistGraphComm]
        } finally {
          if (buffer != Pointer.NULL) buffer.release()
        }
      }
    }

    def distGraphCreateAdjacent(
        sourceEdges: Seq[(Int, Option[Int])],
        destinationEdges: Seq[(Int, Option[Int])],
        info: Info,
        reorder: Boolean): DistGraphComm = {
      val weighted = List(sourceEdges, destinationEdges).map(edges => {
        edges.nonEmpty && edges(0)._2.isDefined
      })
      require(
        List(sourceEdges, destinationEdges).zip(weighted).forall(_ match {
          case (edges, wgtd) => edges.forall(e => e._2.isDefined == wgtd)
        }),
        "Inconsistent assignment of edge weights")
      require(
        List(sourceEdges, destinationEdges).zip(weighted).forall(_ match {
          case (edges, wgtd) => !wgtd || edges.forall(e => e._2.get >= 0)
        }),
        "Invalid edge weight(s)")
      val buffer =
        if (sourceEdges.length > 0 || destinationEdges.length > 0)
          Pointer.allocateInts(
            sourceEdges.length * (if (weighted(0)) 2 else 1) +
              destinationEdges.length * (if (weighted(1)) 2 else 1)).
            as(classOf[Int])
        else
          nullPointer[Int]
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        try {
          var offset = 0
          val sources =
            if (buffer != Pointer.NULL) buffer.next(offset) else buffer
          offset += sourceEdges.length
          val destinations =
            if (buffer != Pointer.NULL) buffer.next(offset) else buffer
          offset += destinationEdges.length
          val sourceweights =
            if (weighted(0)) {
              val prevOffset = offset
              offset += sourceEdges.length
              if (buffer != Pointer.NULL) buffer.next(prevOffset) else buffer
            } else {
              mpi3.lib.MPI_UNWEIGHTED
            }
          val destweights =
            if (weighted(1)) {
              if (buffer != Pointer.NULL) buffer.next(offset) else buffer
            }
            else {
              mpi3.lib.MPI_UNWEIGHTED
            }
          var rIdx = 0
          var wgtIdx = 0
          for (edges <- List(sourceEdges, destinationEdges)) {
            for (e <- edges) {
              e match {
                case (r, wgtOpt) => {
                  sources.set(rIdx, r)
                  rIdx += 1
                  wgtOpt.foreach(w => {
                    sourceweights.set(wgtIdx, w)
                    wgtIdx += 1
                  })
                }
              }
            }
          }
          mpiCall(
            mpi3.lib.MPI_Dist_graph_create_adjacent(
              handle,
              sourceEdges.length,
              sources,
              sourceweights,
              destinationEdges.length,
              destinations,
              destweights,
              info.handle,
              if (reorder) 1 else 0,
              newComm))
          Comm(newComm(0)).asInstanceOf[DistGraphComm]
        } finally {
          if (buffer != Pointer.NULL) buffer.release()
        }
      }
    }

    def winCreate(
        base: Pointer[_],
        size: Long,
        dispUnit: Int,
        info: mpi3.Info): mpi3.Win =
      new Win(base, size, dispUnit, info, this)

    def accept(portName: String, info: mpi3.Info, root: Int): InterComm =
      withOutVar{ newComm: Pointer[mpi3.lib.MPI_Comm] =>
        mpiCall(
          mpi3.lib.MPI_Comm_accept(
            Pointer.pointerToCString(portName).as(classOf[Byte]),
            info.handle,
            root,
            handle,
            newComm))
        Comm(newComm(0)).asInstanceOf[InterComm]
      }

    def connect(portName: String, info: mpi3.Info, root: Int): InterComm =
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        mpiCall(
          mpi3.lib.MPI_Comm_connect(
            Pointer.pointerToCString(portName).as(classOf[Byte]),
            info.handle,
            root,
            handle,
            newComm))
        Comm(newComm(0)).asInstanceOf[InterComm]
      }
  }

  final class InterComm protected[scampi3] () extends Comm {

    override def dup: InterComm = super.dup.asInstanceOf[InterComm]

    override def create(group: mpi3.Group): Option[InterComm] =
      super.create(group).map(_.asInstanceOf[InterComm])

    override def split(colorOpt: Option[Int], key: Int): Option[InterComm] =
      super.split(colorOpt, key).map(_.asInstanceOf[InterComm])

    lazy val remoteSize: Int = withOutVar { size: Pointer[Int] =>
      mpiCall(mpi3.lib.MPI_Comm_remote_size(handle, size))
      size(0)
    }

    lazy val remoteGroup: mpi3.Group =
      withOutVar { group: Pointer[mpi3.lib.MPI_Group] =>
        mpiCall(mpi3.lib.MPI_Comm_remote_group(handle, group))
        mpi3.Group(group(0))
      }

    def merge(high: Boolean): IntraComm =
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        mpiCall(
          mpi3.lib.MPI_Intercomm_merge(handle, if (high) 1 else 0, newComm))
        Comm(newComm(0)).asInstanceOf[IntraComm]
      }

    def local(rank: Int) = LocalRank(this, rank)

    def remote(rank: Int) = RemoteRank(this, rank)

    // gather(), for root
    def gatherIn(buff: mpi3.ValueBuffer[_], recvCount: Int) {
      require(
        recvCount * remoteSize <= buff.valueCount,
        mpi3.gatherBufferSizeErrorMsg)
      mpiCall(
        mpi3.lib.MPI_Gather(
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          buff.pointer,
          recvCount,
          buff.datatype.handle,
          mpi3.lib.MPI_ROOT,
          handle))
    }

    // gather(), for non-root in receiving group
    def gatherIn() {
      mpiCall(
        mpi3.lib.MPI_Gather(
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          mpi3.lib.MPI_PROC_NULL,
          handle))
    }

    // gather(), for any rank in sending group
    def gatherOut(buff: mpi3.ValueBuffer[_], root: mpi3.GroupRank) {
      require(root.group == remoteGroup, "gather root not in remote group")
      mpiCall(
        mpi3.lib.MPI_Gather(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          root.rank,
          handle))
    }

    // gather(), for any rank in either group
    def gather(
       sendBuff: mpi3.ValueBuffer[_],
       recvBuff: mpi3.ValueBuffer[_],
       recvCount: Int,
       root: mpi3.GroupRank) {
      if (root.group == group) {
        if (root.rank == rank) gatherIn(recvBuff, recvCount)
        else gatherIn()
      }
      else gatherOut(sendBuff, root)
    }

    // gatherv(), for root
    def gathervIn(buff: mpi3.ValueBuffer[_], recvBlocks: Seq[mpi3.Block]) {
      require(recvBlocks.length == remoteSize, mpi3.gatherBufferSizeErrorMsg)
      val buffer =
        if (recvBlocks.length > 0)
          Pointer.allocateInts(2 * recvBlocks.length).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val recvcounts = buffer
        val displs =
          if (buffer != Pointer.NULL) buffer.next(recvBlocks.length) else buffer
        var idx = 0
        recvBlocks.foreach( _ match {
          case mpi3.Block(length, displacement) => {
            require(
              0 <= displacement && displacement + length <= buff.valueCount,
              mpi3.bufferCompatibilityErrorMsg)
            recvcounts.set(idx, length)
            displs.set(idx, displacement)
            idx += 1
          }
        })
        mpiCall(
          mpi3.lib.MPI_Gatherv(
            nullPointer[Byte],
            0,
            mpi3.lib.MPI_DATATYPE_NULL,
            buff.pointer,
            recvcounts,
            displs,
            buff.datatype.handle,
            mpi3.lib.MPI_ROOT,
            handle))
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    // gatherv(), for non-root in receiving group
    def gathervIn() {
      mpiCall(
        mpi3.lib.MPI_Gatherv(
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          nullPointer[Byte],
          nullPointer[Int],
          nullPointer[Int],
          mpi3.lib.MPI_DATATYPE_NULL,
          mpi3.lib.MPI_PROC_NULL,
          handle))
    }

    // gatherv(), for any rank in sending group
    def gathervOut(buff: mpi3.ValueBuffer[_], root: mpi3.GroupRank) {
      require(root.group == remoteGroup, "gather root not in remote group")
      mpiCall(
        mpi3.lib.MPI_Gatherv(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          nullPointer[Byte],
          nullPointer[Int],
          nullPointer[Int],
          mpi3.lib.MPI_DATATYPE_NULL,
          root.rank,
          handle))
    }

    // gatherv(), for any rank in either group
    def gatherv(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block],
        root: mpi3.GroupRank) {
      if (root.group == group) {
        if (root.rank == rank) gathervIn(recvBuff, recvBlocks)
        else gathervIn()
      }
      else gathervOut(sendBuff, root)
    }

    // scatterv(), for root
    def scattervOut(sendBuff: mpi3.ValueBuffer[_], sendBlocks: Seq[mpi3.Block]) {
      require(sendBlocks.length == remoteSize, mpi3.scatterBufferSizeErrorMsg)
      assume(sendBlocks.length > 0)
      val buffer = Pointer.allocateInts(2 * sendBlocks.length).as(classOf[Int])
      try {
        val sendcounts = buffer
        val displs = buffer.next(sendBlocks.length)
        var idx = 0
        sendBlocks.foreach( _ match {
          case mpi3.Block(length, displacement) => {
            require(
              0 <= displacement && displacement + length <= sendBuff.valueCount,
              mpi3.bufferCompatibilityErrorMsg)
            sendcounts.set(idx, length)
            displs.set(idx, displacement)
            idx += 1
          }
        })
        mpiCall(
          mpi3.lib.MPI_Scatterv(
            sendBuff.pointer,
            sendcounts,
            displs,
            sendBuff.datatype.handle,
            nullPointer[Byte],
            0,
            mpi3.lib.MPI_DATATYPE_NULL,
            mpi3.lib.MPI_ROOT,
            handle))
      } finally buffer.release()
    }

    // scatterv(), for non-root in sending group
    def scattervOut() {
      mpiCall(
        mpi3.lib.MPI_Scatterv(
          nullPointer[Byte],
          nullPointer[Int],
          nullPointer[Int],
          mpi3.lib.MPI_DATATYPE_NULL,
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          mpi3.lib.MPI_PROC_NULL,
          handle))
    }

    // scatterv(), for any rank in receiving group
    def scattervIn(buff: mpi3.ValueBuffer[_], root: mpi3.GroupRank) {
      require(root.group == remoteGroup, "scatter root not in remote group")
      mpiCall(
        mpi3.lib.MPI_Scatterv(
          nullPointer[Byte],
          nullPointer[Int],
          nullPointer[Int],
          mpi3.lib.MPI_DATATYPE_NULL,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          root.rank,
          handle))
    }

    // scatterv(), for any rank in either group
    def scatterv(
        sendBuff: mpi3.ValueBuffer[_],
        sendBlocks: Seq[mpi3.Block],
        recvBuff: mpi3.ValueBuffer[_],
        root: mpi3.GroupRank) {
      if (root.group == group) {
        if (root.rank == rank) scattervOut(sendBuff, sendBlocks)
        else scattervOut()
      }
      else scattervIn(recvBuff, root)
    }

    // allgather()
    def allgather(sendBuff: mpi3.ValueBuffer[_], recvBuff: mpi3.ValueBuffer[_]) {
      allgather(sendBuff, recvBuff, recvBuff.valueCount / remoteSize)
    }

    // allgatherv()
    override def allgatherv(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      require(recvBlocks.length == remoteSize, mpi3.gatherBufferSizeErrorMsg)
      super.allgatherv(sendBuff, recvBuff, recvBlocks)
    }

    // alltoall()
    override def alltoall(
        sendBuff: mpi3.ValueBuffer[_],
        sendCount: Int,
        recvBuff: mpi3.ValueBuffer[_],
        recvCount: Int) {
      require(
        recvCount * remoteSize <= recvBuff.valueCount,
        mpi3.gatherBufferSizeErrorMsg)
      super.alltoall(sendBuff, sendCount, recvBuff, recvCount)
    }

    // alltoallv()
    override def alltoallv(
        sendBuff: mpi3.ValueBuffer[_],
        sendBlocks: Seq[mpi3.Block],
        recvBuff: mpi3.ValueBuffer[_],
        recvBlocks: Seq[mpi3.Block]) {
      require(recvBlocks.length == remoteSize, mpi3.gatherBufferSizeErrorMsg)
      super.alltoallv(sendBuff, sendBlocks, recvBuff, recvBlocks)
    }

    // alltoallw()
    override def alltoallw(
        sendBuff: mpi3.ValueBuffer[_],
        sendStructs: Seq[mpi3.StructBlock[_]],
        recvBuff: mpi3.ValueBuffer[_],
        recvStructs: Seq[mpi3.StructBlock[_]]) {
      require(recvStructs.length == remoteSize, mpi3.gatherBufferSizeErrorMsg)
      super.alltoallw(sendBuff, sendStructs, recvBuff, recvStructs)
    }

    // reduce(), for root (in receiving group)
    def reduceIn(buff: mpi3.ValueBuffer[_], op: mpi3.Op) {
      mpiCall(
        mpi3.lib.MPI_Reduce(
          nullPointer[Byte],
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          op.handle,
          mpi3.lib.MPI_ROOT,
          handle))
    }

    // reduce(), for non-root in receiving group
    def reduceIn(op: mpi3.Op) {
      mpiCall(
        mpi3.lib.MPI_Reduce(
          nullPointer[Byte],
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          op.handle,
          mpi3.lib.MPI_PROC_NULL,
          handle))
    }

    // reduce(), for any rank in sending group
    def reduceOut(buff: mpi3.ValueBuffer[_], op: mpi3.Op, root: mpi3.GroupRank) {
      require(root.group == remoteGroup, "reduce root not in remote group")
      mpiCall(
        mpi3.lib.MPI_Reduce(
          buff.pointer,
          nullPointer[Byte],
          buff.valueCount,
          buff.datatype.handle,
          op.handle,
          root.rank,
          handle))
    }

    // reduce(), for any rank in either group
    def reduce(
        sendBuff: mpi3.ValueBuffer[_],
        recvBuff: mpi3.ValueBuffer[_],
        op: mpi3.Op,
        root: mpi3.GroupRank) {
      if (root.group == group) {
        if (root.rank == rank) reduceIn(recvBuff, op)
        else reduceIn(op)
      }
      else reduceOut(sendBuff, op, root)
    }

    // bcast(), for root (in sending group)
    def bcastOut(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_Bcast(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_ROOT,
          handle))
    }

    // bcast(), for non-root in sending group
    def bcastOut() {
      mpiCall(
        mpi3.lib.MPI_Bcast(
          nullPointer[Byte],
          0,
          mpi3.lib.MPI_DATATYPE_NULL,
          mpi3.lib.MPI_PROC_NULL,
          handle))
    }

    // bcast(), for any rank in receiving group
    def bcastIn(buff: mpi3.ValueBuffer[_], root: mpi3.GroupRank) {
      require(root.group == remoteGroup, "bcast root not in remote group")
      mpiCall(
        mpi3.lib.MPI_Bcast(
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          root.rank,
          handle))
    }

    // bcast(), for any rank in either group
    def bcast(buff: mpi3.ValueBuffer[_], root: mpi3.GroupRank) {
      if (root.group == group) {
        if (root.rank == rank) bcastOut(buff)
        else bcastOut()
      } else bcastIn(buff, root)
    }
  }

  final class CartComm protected[scampi3] () extends IntraComm {

    override def dup: CartComm = super.dup.asInstanceOf[CartComm]

    override def create(group: Group): Option[CartComm] =
      super.create(group).map(_.asInstanceOf[CartComm])

    override def split(colorOpt: Option[Int], key: Int): Option[CartComm] =
      super.split(colorOpt, key).map(_.asInstanceOf[CartComm])

    lazy val ndims: Int = withOutVar { result: Pointer[Int] =>
      mpiCall(mpi3.lib.MPI_Cartdim_get(handle, result))
      result(0)
    }

    lazy val topo: Seq[(Int, Boolean, Int)] = {
      val buffer =
        if (ndims > 0) Pointer.allocateInts(3 * ndims).as(classOf[Int])
        else nullPointer[Int]
      try {
        val dims = buffer
        val periods =
          if (buffer != Pointer.NULL) buffer.next(ndims) else buffer
        val coords =
          if (buffer != Pointer.NULL) buffer.next(2 * ndims) else buffer
        mpiCall(mpi3.lib.MPI_Cart_get(handle, ndims, dims, periods, coords))
        val result = (List.empty[(Int, Boolean, Int)] /: (0 until ndims)) {
          (acc, i) => ((dims(i), periods(i) != 0, coords(i)) :: acc)
        }
        result.reverse
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    def rank(coords: Seq[Int]): Int = {
      require(coords.length == ndims, "Invalid coordinates dimension")
      require(
        coords.zip(topo).forall(_ match {
          case (c, (size, periodic, _)) => {
            periodic || 0 <= c && c < size
          }}),
        "Invalid coordinate value(s)")
      withOutVar { result: Pointer[Int] =>
        val coordsp = Pointer.pointerToInts(coords:_*).as(classOf[Int])
        try {
          mpiCall(mpi3.lib.MPI_Cart_rank(handle, coordsp, result))
          result(0)
        } finally coordsp.release()
      }
    }

    def coords(rank: Int): Seq[Int] = {
      val result =
        if (ndims > 0) Pointer.allocateInts(ndims).as(classOf[Int])
        else nullPointer[Int]
      try {
        mpiCall(mpi3.lib.MPI_Cart_coords(handle, rank, ndims, result))
        result.getInts
      } finally {
        if (result != Pointer.NULL) result.release()
      }
    }

    lazy val coords: Seq[Int] = coords(rank)

    def shift(direction: Int, disp: Int): (Option[Int], Option[Int]) = {
      require(0 <= direction && direction < ndims, "Invalid direction")
      val result = Pointer.allocateInts(2).as(classOf[Int])
      try {
        val rank_source = result
        val rank_dest = result.next(1)
        mpiCall(
          mpi3.lib.MPI_Cart_shift(
            handle,
            direction,
            disp,
            rank_source,
            rank_dest))
        (if (rank_source(0) != mpi3.lib.MPI_PROC_NULL)
          Some(rank_source(0)) else None,
          if (rank_dest(0) != mpi3.lib.MPI_PROC_NULL)
            Some(rank_dest(0)) else None)
      } finally result.release()
    }

    def sub(remainDims: Seq[Boolean]): CartComm = {
      require(
        remainDims.length == ndims,
        "Subspace selection sequence length is not equal to number of dimensions")
      withOutVar { newComm: Pointer[mpi3.lib.MPI_Comm] =>
        val buffer =
          if (ndims > 0) Pointer.allocateInts(ndims).as(classOf[Int])
          else nullPointer[Int]
        try {
          remainDims.zipWithIndex foreach { case (rd, i) =>
            buffer(i) = if (rd) 1 else 0
          }
          mpiCall(mpi3.lib.MPI_Cart_sub(handle, buffer, newComm))
          Comm(newComm(0)).asInstanceOf[CartComm]
        } finally {
          if (buffer != Pointer.NULL) buffer.release()
        }
      }
    }
  }

  final class GraphComm protected[scampi3] () extends IntraComm {

    override def dup: GraphComm = super.dup.asInstanceOf[GraphComm]

    override def create(group: Group): Option[GraphComm] =
      super.create(group).map(_.asInstanceOf[GraphComm])

    override def split(colorOpt: Option[Int], key: Int): Option[GraphComm] =
      super.split(colorOpt, key).map(_.asInstanceOf[GraphComm])

    lazy val topo: Seq[Seq[Int]] = {
      val (nnodes, nedges) = {
        val result = Pointer.allocateInts(2).as(classOf[Int])
        try {
          mpiCall(mpi3.lib.MPI_Graphdims_get(handle, result, result.next))
          (result(0), result(1))
        } finally result.release()
      }
      val buffer =
        if (nnodes + nedges > 0)
          Pointer.allocateInts(nnodes + nedges).as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val index = buffer
        val edges = if (buffer != Pointer.NULL) buffer.next(nnodes) else buffer
        mpiCall(mpi3.lib.MPI_Graph_get(handle, nnodes, nedges, index, edges))
        var remEdges = edges
        var result = List.empty[Seq[Int]]
        if (nnodes > 0) {
          var j = 0
          for (i <- index.getInts(nnodes)) {
            val numEdges = i - j
            result = remEdges.getInts(numEdges) :: result
            remEdges = remEdges.next(numEdges)
            j = i
          }
        }
        result.reverse
      } finally {
        if (buffer != Pointer.NULL) buffer.release()
      }
    }

    def neighbors(rank: Int): Seq[Int] = {
      val nneighbors = {
        withOutVar { result: Pointer[Int] =>
          mpiCall(mpi3.lib.MPI_Graph_neighbors_count(handle, rank, result))
          result(0)
        }
      }
      if (nneighbors > 0) {
        val result = Pointer.allocateInts(nneighbors).as(classOf[Int])
        try {
          mpiCall(mpi3.lib.MPI_Graph_neighbors(handle, rank, nneighbors, result))
          result.getInts
        } finally result.release()
      } else {
        Seq.empty
      }
    }

    lazy val neighbors: Seq[Int] = neighbors(rank)
  }

  final class DistGraphComm protected[scampi3] () extends IntraComm {

    override def dup: DistGraphComm = super.dup.asInstanceOf[DistGraphComm]

    override def create(group: Group): Option[DistGraphComm] =
      super.create(group).map(_.asInstanceOf[DistGraphComm])

    override def split(colorOpt: Option[Int], key: Int): Option[DistGraphComm] =
      super.split(colorOpt, key).map(_.asInstanceOf[DistGraphComm])

    lazy val neighbors: (Seq[Int], Seq[Int], Option[(Seq[Int], Seq[Int])]) = {
      val (indegree, outdegree, weighted) = {
        val buffer = Pointer.allocateInts(3).as(classOf[Int])
        try {
          val indg = buffer
          val outdg = buffer.next(1)
          val wgtd = buffer.next(2)
          mpiCall(
            mpi3.lib.MPI_Dist_graph_neighbors_count(handle, indg, outdg, wgtd))
          (indg(0), outdg(0), wgtd(0) != 0)
        } finally buffer.release()
      }
      val resultp =
        if (indegree + outdegree > 0)
          Pointer.allocateInts((indegree + outdegree) * (if (weighted) 2 else 1)).
            as(classOf[Int])
        else
          nullPointer[Int]
      try {
        val sources = resultp
        val destinations =
          if (resultp != Pointer.NULL) resultp.next(indegree) else resultp
        val (sourceweights, destweights) =
          if (weighted) {
            if (resultp != Pointer.NULL)
              (resultp.next(indegree + outdegree),
                resultp.next(2 * indegree + outdegree))
            else
              (resultp, resultp)
          } else {
            (mpi3.lib.MPI_UNWEIGHTED, mpi3.lib.MPI_UNWEIGHTED)
          }
        mpiCall(
          mpi3.lib.MPI_Dist_graph_neighbors(
            handle,
            indegree,
            sources,
            sourceweights,
            outdegree,
            destinations,
            destweights))
        if (resultp != Pointer.NULL) {
          (sources.getInts(indegree),
            destinations.getInts(outdegree),
            if (weighted)
              Some((sourceweights.getInts(indegree),
                destweights.getInts(outdegree)))
            else None)
        } else {
          (Seq.empty,
            Seq.empty,
            if (weighted) Some((Seq.empty, Seq.empty)) else None)
        }
      } finally {
        if (resultp != Pointer.NULL) resultp.release()
      }
    }
  }

  object Comm {
    private val comms: mutable.Map[mpi3.lib.MPI_Comm, WeakReference[Comm]] =
      mutable.Map.empty

    def apply(comm: mpi3.lib.MPI_Comm): Comm = comms.synchronized {
      val optComm =
        if (comms.contains(comm)) {
          comms(comm) match {
            case WeakReference(c) if !c.isNull => Some(c)
            case _ => None
          }
        } else None
      optComm.getOrElse(
        if (comm != mpi3.lib.MPI_COMM_NULL) {
          withOutVar { flag: Pointer[Int] =>
            mpiCall(mpi3.lib.MPI_Comm_test_inter(comm, flag))
            val result =
              if (flag(0) == 0) {
                mpiCall(mpi3.lib.MPI_Topo_test(comm, flag))
                val topo = flag(0)
                if (topo == mpi3.lib.MPI_CART) new CartComm
                else if (topo == mpi3.lib.MPI_GRAPH) new GraphComm
                else if (topo == mpi3.lib.MPI_DIST_GRAPH) new DistGraphComm
                else {
                  assume(topo == mpi3.lib.MPI_UNDEFINED)
                  new IntraComm
                }
              } else {
                new InterComm
              }
            result.handlePtr(0) = comm
            result.errHandler = CommErrHandler.Return
            comms(comm) = WeakReference(result)
            result
          }
        } else {
          throw new mpi3.Exception("Null communicator cannot be instantiated")
        }
      )
    }

    protected[scampi3] def remove(comm: Comm) {
      comms.synchronized {
        comms -= comm.handle
      }
    }

    lazy val world: IntraComm =
      Comm(mpi3.lib.MPI_COMM_WORLD).asInstanceOf[IntraComm]

    lazy val self: IntraComm =
      Comm(mpi3.lib.MPI_COMM_SELF).asInstanceOf[IntraComm]

    def parent: Option[InterComm] =
      withOutVar { comm: Pointer[mpi3.lib.MPI_Comm] =>
        mpi3.mpiCall(mpi3.lib.MPI_Comm_get_parent(comm))
        if (comm(0) != mpi3.lib.MPI_COMM_NULL)
          Some(Comm(comm(0)).asInstanceOf[InterComm])
        else
          None
      }

    object PredefCommIntKeyval
        extends mpi3.RestrictedAttributeKeyval[mpi3.Comm,Int]
        with mpi3.KeyvalInt

    object TagUBKey extends mpi3.CacheKey(
      PredefCommIntKeyval, mpi3.lib.MPI_TAG_UB)

    object UniverseSizeKey
        extends mpi3.CacheKey(PredefCommIntKeyval, mpi3.lib.MPI_UNIVERSE_SIZE)

    object LastUsedCodeKey
        extends mpi3.CacheKey(PredefCommIntKeyval, mpi3.lib.MPI_LASTUSEDCODE)

    object AppNumKey
        extends mpi3.CacheKey(PredefCommIntKeyval, mpi3.lib.MPI_APPNUM)

    object WtimeIsGlobalKey
        extends mpi3.CacheKey(
      new mpi3.RestrictedAttributeKeyval[mpi3.Comm,Boolean]
          with mpi3.KeyvalBoolean,
      mpi3.lib.MPI_WTIME_IS_GLOBAL)

    object HostKey extends mpi3.CacheKey(
      new mpi3.RestrictedAttributeKeyval[mpi3.Comm,Option[Int]] {
        def toPointer(v: Option[Int]): Pointer[_] =
          Pointer.pointerToInt(v.getOrElse(mpi3.lib.MPI_PROC_NULL))

        def fromPointer(p: Pointer[_]): Option[Int] = {
          val v = p.as(classOf[Int])(0)
          if (v != mpi3.lib.MPI_PROC_NULL) Some(v) else None
        }
      },
      mpi3.lib.MPI_HOST)

    object IOKey extends mpi3.CacheKey(
      new RestrictedAttributeKeyval[mpi3.Comm,Either[Boolean,Option[Int]]] {
        def toPointer(v: Either[Boolean,Option[Int]]): Pointer[_] =
          Pointer.pointerToInt(
            if (v.isLeft) mpi3.lib.MPI_ANY_SOURCE
            else v.right.get.getOrElse(mpi3.lib.MPI_PROC_NULL))

        def fromPointer(p: Pointer[_]): Either[Boolean,Option[Int]] = {
          val v = p.as(classOf[Int])(0)
          if
            (v == mpi3.lib.MPI_ANY_SOURCE) Left(true)
          else
            Right(if (v != mpi3.lib.MPI_PROC_NULL) Some(v) else None)
        }
      },
      mpi3.lib.MPI_IO)
  }

  trait CommErrHandler extends mpi3.ErrHandler

  class CommUserErrHandler(fn: Function2[Comm, Int, (Comm, Int)])
      extends CommErrHandler
      with mpi3.UserErrHandler {
    // The error handler should only be called within the context of a
    // an mpiCall function.
    private def handleError(comm: Pointer[mpi3.lib.MPI_Comm], err: Pointer[Int]) {
      fn(Comm(comm(0)), err(0)) match {
        case (newcomm, code) => {
          comm(0) = newcomm.handle
          err(0) = code
        }
      }
    }

    private val errhandlerFunction =
      mpi3.lib.MPI_Comm_errhandler_function(handleError)

    mpi3.mpiCall(
      mpi3.lib.MPI_Comm_create_errhandler(
        Pointer.pointerTo(errhandlerFunction),
        handlePtr))
  }

  object CommErrHandler {
    object Abort extends CommErrHandler {
      handlePtr.set(mpi3.lib.MPI_ERRORS_ARE_FATAL)
    }
    object Return extends CommErrHandler {
      handlePtr.set(mpi3.lib.MPI_ERRORS_RETURN)
    }
  }
}
