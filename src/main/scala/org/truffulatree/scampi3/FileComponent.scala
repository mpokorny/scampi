//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.language.existentials
import scala.collection.mutable
import scala.ref.WeakReference
import org.bridj.Pointer
import java.io.{File => JFile}

trait FileComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  case class FileView(
    disp: mpi3.lib.MPI_Offset,
    etype: mpi3.Datatype[_],
    filetype: mpi3.Datatype[_],
    datarep: String)

  class File(
    comm: mpi3.IntraComm,
    filePath: JFile,
    openMode: Seq[mpi3.FileMode.FileMode],
    openInfo: mpi3.Info = mpi3.InfoNull)
      extends mpi3.WithErrHandler {

    protected final val handlePtr: Pointer[mpi3.lib.MPI_File] = {
      val result = mpi3.allocateFile()
      result.set(mpi3.lib.MPI_FILE_NULL)
      result
    }

    protected[scampi3] final def handle = handlePtr(0)

    mpi3.mpiCall(
      mpi3.lib.MPI_File_open(
        comm.handle,
        Pointer.pointerToCString(filePath.getPath).as(classOf[Byte]),
        mpi3.FileMode.amode(openMode),
        openInfo.handle,
        handlePtr),
      CommException.curried(comm))

    File.register(this)

    type ErrHandlerType = FileErrHandler

    protected var currentErrHandler: FileErrHandler = File.defaultErrHandler

    errHandler = currentErrHandler

    protected def mpiSetErrhandler(errhandler: mpi3.lib.MPI_Errhandler): Int =
      mpi3.lib.MPI_File_set_errhandler(handle, errhandler)

    protected final val selfException = mpi3.FileException.curried(this)

    protected def mpiCall(c: => Int) = mpi3.mpiCall(c, selfException)

    override def finalize() {
      mpi3.lifecycleSync { if (!mpi3.finalized) close() }
      super.finalize()
    }

    final def close() {
      if (!isNull) {
        File.remove(this)
        mpiCall(mpi3.lib.MPI_File_close(handlePtr))
      }
    }

    final def isNull: Boolean = handle == mpi3.lib.MPI_FILE_NULL

    def size: mpi3.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi3.lib.MPI_Offset] =>
        mpiCall(mpi3.lib.MPI_File_get_size(handle, result))
        result(0)
      }

    def size_=(sz: mpi3.lib.MPI_Offset) {
      mpiCall(mpi3.lib.MPI_File_set_size(handle, sz))
    }

    def preallocate(sz: mpi3.lib.MPI_Offset) {
      mpiCall(mpi3.lib.MPI_File_preallocate(handle, sz))
    }

    def group: mpi3.Group = withOutVar { group: Pointer[mpi3.lib.MPI_Group] =>
      mpiCall(mpi3.lib.MPI_File_get_group(handle, group))
      Group(group(0))
    }

    def amode: Seq[mpi3.FileMode.FileMode] = withOutVar { flagsp: Pointer[Int] =>
      mpiCall(mpi3.lib.MPI_File_get_amode(handle, flagsp))
      var result = List.empty[mpi3.FileMode.FileMode]
      val flags = flagsp(0)
      var mode = 1
      while (flags >= mode) {
        if ((flags & mode) != 0) result = mpi3.FileMode(mode) :: result
        mode <<= 1
      }
      result
    }

    def info: mpi3.Info = {
      val result = new mpi3.Info
      mpiCall(mpi3.lib.MPI_File_get_info(handle, result.handlePtr))
      result
    }

    def info_=(info: mpi3.Info) {
      mpiCall(mpi3.lib.MPI_File_set_info(handle, info.handle))
    }

    def view: FileView =
      withOutVar { disp: Pointer[mpi3.lib.MPI_Offset] =>
        val dts = mpi3.allocateDatatype(2)
        val datarep = Pointer.allocateBytes(mpi3.lib.MPI_MAX_DATAREP_STRING + 1)
        try {
          val etype = dts
          val filetype = dts.next(1)
          mpiCall(
            mpi3.lib.MPI_File_get_view(
              handle,
              disp,
              etype,
              filetype,
              datarep.as(classOf[Byte])))
          FileView(
            disp(0),
            mpi3.Datatype.lookup(etype(0)),
            mpi3.Datatype.lookup(filetype(0)),
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
          mpi3.lib.MPI_File_set_view(
            handle,
            view.disp,
            view.etype.handle,
            view.filetype.handle,
            datarep,
            info.handle))
      }
    }

    def readAt(
        offset: mpi3.lib.MPI_Offset,
        buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadAt(offset: mpi3.lib.MPI_Offset, buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def readAtAll(
        offset: mpi3.lib.MPI_Offset,
        buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadAtAll(offset: mpi3.lib.MPI_Offset, buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeAt(
        offset: mpi3.lib.MPI_Offset,
        buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteAt(offset: mpi3.lib.MPI_Offset, buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeAtAll(
        offset: mpi3.lib.MPI_Offset,
        buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteAtAll(offset: mpi3.lib.MPI_Offset, buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_at_all(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def ireadAt(
        offset: mpi3.lib.MPI_Offset,
        buff: mpi3.ValueBuffer[_]): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_File_iread_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def iwriteAt(
        offset: mpi3.lib.MPI_Offset,
        buff: mpi3.ValueBuffer[_]): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_File_iwrite_at(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def read(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doRead(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def readAll(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadAll(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def write(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWrite(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeAll(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }
    def doWriteAll(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_all(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def iread(buff: mpi3.ValueBuffer[_]): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_File_iread(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def iwrite(buff: mpi3.ValueBuffer[_]): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_File_iwrite(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def seek(offset: mpi3.lib.MPI_Offset, whence: mpi3.Seek.Seek) {
      mpiCall(mpi3.lib.MPI_File_seek(handle, offset, whence.id))
    }

    def position: mpi3.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi3.lib.MPI_Offset] =>
        mpiCall(mpi3.lib.MPI_File_get_position(handle, result))
        result(0)
      }

    def byteOffset(offset: mpi3.lib.MPI_Offset): mpi3.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi3.lib.MPI_Offset] =>
        mpiCall(mpi3.lib.MPI_File_get_byte_offset(handle, offset, result))
        result(0)
      }

    def readShared(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadShared(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeShared(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteShared(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def ireadShared(buff: mpi3.ValueBuffer[_]): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_File_iread_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def iwriteShared(buff: mpi3.ValueBuffer[_]): mpi3.Request = {
      val result = new mpi3.Request
      mpiCall(
        mpi3.lib.MPI_File_iwrite_shared(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          result.handlePtr))
      result
    }

    def readOrdered(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadOrdered(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeOrdered(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteOrdered(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_ordered(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def seekShared(offset: mpi3.lib.MPI_Offset, whence: mpi3.Seek.Seek) {
      mpiCall(mpi3.lib.MPI_File_seek_shared(handle, offset, whence.id))
    }

    def positionShared: mpi3.lib.MPI_Offset =
      withOutVar { result: Pointer[mpi3.lib.MPI_Offset] =>
        mpiCall(mpi3.lib.MPI_File_get_position_shared(handle, result))
        result(0)
      }

    def readAtAllBegin(offset: mpi3.lib.MPI_Offset, buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_at_all_begin(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def readAtAllEnd(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_at_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadAtAllEnd(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_at_all_end(
          handle,
          buff.pointer,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeAtAllBegin(offset: mpi3.lib.MPI_Offset, buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_at_all_begin(
          handle,
          offset,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def writeAtAllEnd(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_at_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteAtAllEnd(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_at_all_end(
          handle,
          buff.pointer,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def readAllBegin(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_all_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def readAllEnd(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadAllEnd(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_all_end(
          handle,
          buff.pointer,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeAllBegin(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_all_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def writeAllEnd(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_all_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteAllEnd(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_all_end(
          handle,
          buff.pointer,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def readOrderedBegin(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_ordered_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def readOrderedEnd(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_read_ordered_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doReadOrderedEnd(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_read_ordered_end(
          handle,
          buff.pointer,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def writeOrderedBegin(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_ordered_begin(
          handle,
          buff.pointer,
          buff.valueCount,
          buff.datatype.handle))
    }

    def writeOrderedEnd(buff: mpi3.ValueBuffer[_]): mpi3.Status = {
      val status = mpi3.newStatus()
      mpiCall(
        mpi3.lib.MPI_File_write_ordered_end(
          handle,
          buff.pointer,
          Pointer.pointerTo(status(0))))
      new mpi3.Status(status(0))
    }

    def doWriteOrderedEnd(buff: mpi3.ValueBuffer[_]) {
      mpiCall(
        mpi3.lib.MPI_File_write_ordered_end(
          handle,
          buff.pointer,
          mpi3.lib.MPI_STATUS_IGNORE))
    }

    def typeExtent(datatype: mpi3.Datatype[_]): mpi3.lib.MPI_Aint =
      withOutVar { result: Pointer[mpi3.lib.MPI_Aint] =>
        mpiCall(
          mpi3.lib.MPI_File_get_type_extent(handle, datatype.handle, result))
        result(0)
      }

    def atomicity: Boolean = withOutVar { result: Pointer[Int] =>
      mpiCall(mpi3.lib.MPI_File_get_atomicity(handle, result))
      result(0) != 0
    }

    def atomicity_=(state: Boolean) {
      mpiCall(mpi3.lib.MPI_File_set_atomicity(handle, if (state) 1 else 0))
    }

    def sync() {
      mpiCall(mpi3.lib.MPI_File_sync(handle))
    }
  }

  object File {
    private val files: mutable.Map[mpi3.lib.MPI_File, WeakReference[File]] =
      mutable.Map.empty

    def register(file: File) {
      files.synchronized {
        require(!file.isNull, "Registered file may have a null handle")
        files(file.handle) = WeakReference(file)
      }
    }

    protected[scampi3] def remove(file: File) {
      files.synchronized { files -= file.handle }
    }

    def lookup(file: mpi3.lib.MPI_File): Option[File] = files.synchronized {
      if (files.contains(file)) {
        files(file) match {
          case WeakReference(f) if !f.isNull => Some(f)
          case _ => None
        }
      } else None
    }

    def delete(filePath: JFile, info: Info = mpi3.InfoNull) {
      withInString(filePath.getPath) { f: Pointer[Byte] =>
        mpi3.mpiCall(mpi3.lib.MPI_File_delete(f, info.handle))
      }
    }

    private var currentErrHandler: mpi3.FileErrHandler = FileErrHandler.Return

    def defaultErrHandler: mpi3.FileErrHandler = synchronized {
      currentErrHandler
    }

    def defaultErrHandler_=(eh: mpi3.FileErrHandler) {
      synchronized {
        currentErrHandler = eh
        mpi3.mpiCall(
          mpi3.lib.MPI_File_set_errhandler(mpi3.lib.MPI_FILE_NULL, eh.handle))
      }
    }
  }

  trait FileErrHandler extends mpi3.ErrHandler

  // We use Option[File] arguments in FileUserErrHandler.fn so that
  // FileUserErrHandler may be a default error handler.
  class FileUserErrHandler(fn: Function2[Option[File], Int, (Option[File], Int)])
      extends FileErrHandler
      with mpi3.UserErrHandler {
    // The error handler should only be called within the context of a
    // an mpiCall function.
    def handleError(file: Pointer[mpi3.lib.MPI_File], err: Pointer[Int]) {
      fn(File.lookup(file(0)), err(0)) match {
        case (Some(newfile), code) => {
          file(0) = newfile.handle
          err(0) = code
        }
        case (None, code) =>
          err(0) = code
      }
    }

    private val errhandlerFunction =
      mpi3.lib.MPI_File_errhandler_function(handleError)

    mpi3.mpiCall(
      mpi3.lib.MPI_File_create_errhandler(
        Pointer.pointerTo(errhandlerFunction),
        handlePtr))
  }

  object FileErrHandler {
    object Abort extends FileErrHandler {
      handlePtr.set(mpi3.lib.MPI_ERRORS_ARE_FATAL)
    }
    object Return extends FileErrHandler {
      handlePtr.set(mpi3.lib.MPI_ERRORS_RETURN)
    }
  }
}
