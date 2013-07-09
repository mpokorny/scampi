//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import scala.language.existentials
import scala.collection.mutable
import scala.ref.WeakReference
import org.bridj.Pointer
import java.io.{File => JFile}

trait FileComponent {
  mpi2: Scampi2 with Mpi2LibraryComponent =>

  case class FileView(
    disp: mpi2.lib.MPI_Offset,
    etype: mpi2.Datatype[_],
    filetype: mpi2.Datatype[_],
    datarep: String)

  class File(
    comm: mpi2.IntraComm,
    filePath: JFile,
    openMode: Seq[mpi2.FileMode.FileMode],
    openInfo: mpi2.Info = mpi2.InfoNull)
      extends mpi2.WithErrHandler {

    protected final val handlePtr: Pointer[mpi2.lib.MPI_File] = {
      val result = mpi2.allocateFile()
      result.set(mpi2.lib.MPI_FILE_NULL)
      result
    }

    final def handle = handlePtr(0)

    mpi2.mpiCall(
      mpi2.lib.MPI_File_open(
        comm.handle,
        Pointer.pointerToCString(filePath.getPath).as(classOf[Byte]),
        mpi2.FileMode.amode(openMode),
        openInfo.handle,
        handlePtr),
      CommException.curried(comm))

    File.register(this)

    type ErrHandlerType = FileErrHandler

    protected var currentErrHandler: FileErrHandler = File.defaultErrHandler

    errHandler = currentErrHandler

    protected def mpiSetErrhandler(errhandler: mpi2.lib.MPI_Errhandler): Int =
      mpi2.lib.MPI_File_set_errhandler(handle, errhandler)

    protected final val selfException = mpi2.FileException.curried(this)

    protected def mpiCall(c: => Int) = mpi2.mpiCall(c, selfException)

    override def finalize() {
      mpi2.lifecycleSync { if (!mpi2.finalized) close() }
      super.finalize()
    }

    final def close() {
      if (!isNull) {
        File.remove(this)
        mpiCall(mpi2.lib.MPI_File_close(handlePtr))
      }
    }

    final def isNull: Boolean = handle == mpi2.lib.MPI_FILE_NULL

    def size: mpi2.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi2.lib.MPI_Offset] =>
        mpiCall(mpi2.lib.MPI_File_get_size(handle, result))
        result(0)
      }

    def size_=(sz: mpi2.lib.MPI_Offset) {
      mpiCall(mpi2.lib.MPI_File_set_size(handle, sz))
    }

    def preallocate(sz: mpi2.lib.MPI_Offset) {
      mpiCall(mpi2.lib.MPI_File_preallocate(handle, sz))
    }

    def group: mpi2.Group = withOutVar { group: Pointer[mpi2.lib.MPI_Group] =>
      mpiCall(mpi2.lib.MPI_File_get_group(handle, group))
      Group(group(0))
    }

    def amode: Seq[mpi2.FileMode.FileMode] = withOutVar { flagsp: Pointer[Int] =>
      mpiCall(mpi2.lib.MPI_File_get_amode(handle, flagsp))
      var result = List.empty[mpi2.FileMode.FileMode]
      val flags = flagsp(0)
      var mode = 1
      while (flags >= mode) {
        if ((flags & mode) != 0) result = mpi2.FileMode(mode) :: result
        mode <<= 1
      }
      result
    }

    def info: mpi2.Info = {
      val result = new mpi2.Info
      mpiCall(mpi2.lib.MPI_File_get_info(handle, result.handlePtr))
      result
    }

    def info_=(info: mpi2.Info) {
      mpiCall(mpi2.lib.MPI_File_set_info(handle, info.handle))
    }

    def view: FileView =
      withOutVar { disp: Pointer[mpi2.lib.MPI_Offset] =>
        val dts = mpi2.allocateDatatype(2)
        val datarep = Pointer.allocateBytes(mpi2.lib.MPI_MAX_DATAREP_STRING + 1)
        try {
          val etype = dts
          val filetype = dts.next(1)
          mpiCall(
            mpi2.lib.MPI_File_get_view(
              handle,
              disp,
              etype,
              filetype,
              datarep.as(classOf[Byte])))
          FileView(
            disp(0),
            mpi2.Datatype.lookup(etype(0)),
            mpi2.Datatype.lookup(filetype(0)),
            datarep.getCString)
        } finally {
          dts.release()
          datarep.release()
        }
      }

    def view_=(view: FileView) { setView(view) }

    def setView(view: FileView, info: Info = InfoNull) {
      withInString(view.datarep) { datarep: Pointer[Byte] =>
        mpiCall(
          mpi2.lib.MPI_File_set_view(
            handle,
            view.disp,
            view.etype.handle,
            view.filetype.handle,
            datarep,
            info.handle))
      }
    }

    def readAt(
        offset: mpi2.lib.MPI_Offset,
        buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadAt(offset: mpi2.lib.MPI_Offset, buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def readAtAll(
        offset: mpi2.lib.MPI_Offset,
        buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadAtAll(offset: mpi2.lib.MPI_Offset, buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeAt(
        offset: mpi2.lib.MPI_Offset,
        buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteAt(offset: mpi2.lib.MPI_Offset, buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeAtAll(
        offset: mpi2.lib.MPI_Offset,
        buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteAtAll(offset: mpi2.lib.MPI_Offset, buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def ireadAt(
        offset: mpi2.lib.MPI_Offset,
        buff: mpi2.ValueBuffer[_]): mpi2.Request = {
      val result = new mpi2.Request
      mpiCall(
        mpi2.lib.MPI_File_iread_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def iwriteAt(
        offset: mpi2.lib.MPI_Offset,
        buff: mpi2.ValueBuffer[_]): mpi2.Request = {
      val result = new mpi2.Request
      mpiCall(
        mpi2.lib.MPI_File_iwrite_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def read(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doRead(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def readAll(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadAll(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def write(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWrite(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeAll(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }
    def doWriteAll(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def iread(buff: mpi2.ValueBuffer[_]): mpi2.Request = {
      val result = new mpi2.Request
      mpiCall(
        mpi2.lib.MPI_File_iread(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def iwrite(buff: mpi2.ValueBuffer[_]): mpi2.Request = {
      val result = new mpi2.Request
      mpiCall(
        mpi2.lib.MPI_File_iwrite(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def seek(offset: mpi2.lib.MPI_Offset, whence: mpi2.Seek.Seek) {
      mpiCall(mpi2.lib.MPI_File_seek(handle, offset, whence.id))
    }

    def position: mpi2.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi2.lib.MPI_Offset] =>
        mpiCall(mpi2.lib.MPI_File_get_position(handle, result))
        result(0)
      }

    def byteOffset(offset: mpi2.lib.MPI_Offset): mpi2.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi2.lib.MPI_Offset] =>
        mpiCall(mpi2.lib.MPI_File_get_byte_offset(handle, offset, result))
        result(0)
      }

    def readShared(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadShared(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeShared(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteShared(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def ireadShared(buff: mpi2.ValueBuffer[_]): mpi2.Request = {
      val result = new mpi2.Request
      mpiCall(
        mpi2.lib.MPI_File_iread_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def iwriteShared(buff: mpi2.ValueBuffer[_]): mpi2.Request = {
      val result = new mpi2.Request
      mpiCall(
        mpi2.lib.MPI_File_iwrite_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def readOrdered(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadOrdered(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeOrdered(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteOrdered(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def seekShared(offset: mpi2.lib.MPI_Offset, whence: mpi2.Seek.Seek) {
      mpiCall(mpi2.lib.MPI_File_seek_shared(handle, offset, whence.id))
    }

    def positionShared: mpi2.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi2.lib.MPI_Offset] =>
        mpiCall(mpi2.lib.MPI_File_get_position_shared(handle, result))
        result(0)
      }

    def readAtAllBegin(offset: mpi2.lib.MPI_Offset, buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_at_all_begin(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def readAtAllEnd(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_at_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadAtAllEnd(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_at_all_end(
          handle,
          buff.pointer,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeAtAllBegin(offset: mpi2.lib.MPI_Offset, buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_at_all_begin(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def writeAtAllEnd(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_at_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteAtAllEnd(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_at_all_end(
          handle,
          buff.pointer,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def readAllBegin(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_all_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def readAllEnd(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadAllEnd(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_all_end(
          handle,
          buff.pointer,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeAllBegin(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_all_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def writeAllEnd(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteAllEnd(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_all_end(
          handle,
          buff.pointer,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def readOrderedBegin(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_ordered_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def readOrderedEnd(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_read_ordered_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doReadOrderedEnd(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_read_ordered_end(
          handle,
          buff.pointer,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def writeOrderedBegin(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_ordered_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def writeOrderedEnd(buff: mpi2.ValueBuffer[_]): mpi2.Status = {
      val status = mpi2.newStatus()
      mpiCall(
        mpi2.lib.MPI_File_write_ordered_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi2.Status(status(0))
    }

    def doWriteOrderedEnd(buff: mpi2.ValueBuffer[_]) {
      mpiCall(
        mpi2.lib.MPI_File_write_ordered_end(
          handle,
          buff.pointer,
          mpi2.lib.MPI_STATUS_IGNORE))
    }

    def typeExtent(datatype: mpi2.Datatype[_]): mpi2.lib.MPI_Aint =
      withOutVar { result: Pointer[mpi2.lib.MPI_Aint] =>
        mpiCall(
          mpi2.lib.MPI_File_get_type_extent(handle, datatype.handle, result))
        result(0)
      }

    def atomicity: Boolean = withOutVar { result: Pointer[Int] =>
      mpiCall(mpi2.lib.MPI_File_get_atomicity(handle, result))
      result(0) != 0
    }

    def atomicity_=(state: Boolean) {
      mpiCall(mpi2.lib.MPI_File_set_atomicity(handle, if (state) 1 else 0))
    }

    def sync() {
      mpiCall(mpi2.lib.MPI_File_sync(handle))
    }
  }

  object File {
    private val files: mutable.Map[mpi2.lib.MPI_File, File] =
      mutable.Map.empty

    def register(file: File) {
      files.synchronized {
        require(!file.isNull, "Registered file may have a null handle")
        files(file.handle) = file
      }
    }

    protected[scampi2] def remove(file: File) {
      files.synchronized { files -= file.handle }
    }

    def lookup(file: mpi2.lib.MPI_File): Option[File] = files.synchronized {
      files.get(file)
    }

    def delete(filePath: JFile, info: Info = mpi2.InfoNull) {
      withInString(filePath.getPath) { f: Pointer[Byte] =>
        mpi2.mpiCall(mpi2.lib.MPI_File_delete(f, info.handle))
      }
    }

    private var currentErrHandler: mpi2.FileErrHandler = FileErrHandler.Return

    def defaultErrHandler: mpi2.FileErrHandler = synchronized {
      currentErrHandler
    }

    def defaultErrHandler_=(eh: mpi2.FileErrHandler) {
      synchronized {
        currentErrHandler = eh
        mpi2.mpiCall(
          mpi2.lib.MPI_File_set_errhandler(mpi2.lib.MPI_FILE_NULL, eh.handle))
      }
    }
  }

  trait FileErrHandler extends mpi2.ErrHandler

  // We use Option[File] arguments in FileUserErrHandler.fn so that
  // FileUserErrHandler may be a default error handler.
  class FileUserErrHandler(fn: Function2[Option[File], Int, (Option[File], Int)])
      extends mpi2.lib.MPI_File_errhandler_function
      with FileErrHandler
      with mpi2.UserErrHandler {
    mpi2.mpiCall(
      mpi2.lib.MPI_File_create_errhandler(Pointer.pointerTo(this), handlePtr))

    // The error handler should only be called within the context of a
    // an mpiCall function.
    def apply(file: Pointer[mpi2.lib.MPI_File], err: Pointer[Int]) {
      val result = fn(File.lookup(file(0)), err(0))
      result._1.foreach(f => file.set(f.handle))
      err.set(result._2)
    }
  }

  object FileErrHandler {
    object Abort extends FileErrHandler {
      handlePtr.set(mpi2.lib.MPI_ERRORS_ARE_FATAL)
    }
    object Return extends FileErrHandler {
      handlePtr.set(mpi2.lib.MPI_ERRORS_RETURN)
    }
  }
}
