//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

import scala.collection.mutable
import scala.collection.generic.CanBuildFrom
import org.bridj.Pointer

trait InfoComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  sealed class Info () extends mutable.Map[String, String]
      with mutable.MapLike[String, String, Info] {

    protected[scampi3] final val handlePtr: Pointer[mpi3.lib.MPI_Info] = {
      val result = allocateInfo()
      result(0) = mpi3.lib.MPI_INFO_NULL
      result
    }

    protected[scampi3] final def handle = handlePtr(0)

    override def finalize() {
      mpi3.lifecycleSync { if (!mpi3.finalized) free() }
      super.finalize()
    }

    def free() {
      if (!isNull) {
        mpi3.mpiCall(mpi3.lib.MPI_Info_free(handlePtr))
      }
    }

    def +=(kv: (String, String)) = {
      withInString(kv._1) { key =>
        withInString(kv._2) { value =>
          mpi3.mpiCall(mpi3.lib.MPI_Info_set(handle, key, value))
          this
        }
      }
    }

    def -=(key: String) = {
      withInString(key) { keyp =>
        mpi3.mpiCall(mpi3.lib.MPI_Info_delete(handle, keyp))
        this
      }
    }

    override def empty = Info.empty

    def get(key: String): Option[String] = {
      withInString(key) { keyp =>
        withOutVar { flag: Pointer[Int] =>
          withOutVar { valuelen: Pointer[Int] =>
            mpi3.mpiCall(
              mpi3.lib.MPI_Info_get_valuelen(handle, keyp, valuelen, flag))
            if (flag(0) != 0)
              Some(getString(valuelen(0)) { (len, valuebuf) =>
                mpi3.mpiCall(
                  mpi3.lib.MPI_Info_get(handle, keyp, len, valuebuf, flag))
                assert(flag(0) != 0)
              })
              else
                None
          }
        }
      }
    }

    def iterator: Iterator[(String, String)] = {
      var valbufLen = 80
      var valbuf = Pointer.allocateBytes(valbufLen).as(classOf[Byte])
      try {
        withByteBuffer(mpi3.lib.MPI_MAX_INFO_KEY + 1) { keybuf =>
          withOutVar { flag: Pointer[Int] =>
            withOutVar { valuelen: Pointer[Int] =>
              ((0 until size) map { i =>
                mpi3.mpiCall(mpi3.lib.MPI_Info_get_nthkey(handle, i, keybuf))
                mpi3.mpiCall(
                  mpi3.lib.MPI_Info_get_valuelen(handle, keybuf, valuelen, flag))
                if (valuelen(0) >= valbufLen) {
                  valbuf.release()
                  valbufLen = valuelen(0) + 1
                  valbuf = Pointer.allocateBytes(valbufLen).as(classOf[Byte])
                }
                mpi3.mpiCall(
                  mpi3.lib.MPI_Info_get(
                    handle,
                    keybuf,
                    valbufLen - 1,
                    valbuf,
                    flag))
                (keybuf.getCString, valbuf.getCString)
              }).toIterator
            }
          }
        }
      } finally valbuf.release()
    }

    override def size: Int = {
      withOutVar { nkeys: Pointer[Int] =>
        mpi3.mpiCall(mpi3.lib.MPI_Info_get_nkeys(handle, nkeys))
        nkeys(0)
      }
    }

    def dup: Info = withOutVar { newInfo: Pointer[mpi3.lib.MPI_Info] =>
      mpi3.mpiCall(mpi3.lib.MPI_Info_dup(handle, newInfo))
      Info.newInfo(newInfo(0))
    }

    def isNull: Boolean = handle != mpi3.lib.MPI_INFO_NULL
  }

  object InfoNull extends Info {
    handlePtr.set(mpi3.lib.MPI_INFO_NULL)
  }

  object InfoEnv extends Info {
    handlePtr.set(mpi3.lib.MPI_INFO_ENV)
  }

  object Info {
    def apply(): Info = withOutVar { info: Pointer[mpi3.lib.MPI_Info] =>
      mpi3.mpiCall(mpi3.lib.MPI_Info_create(info))
      newInfo(info(0))
    }

    private def newInfo(info: mpi3.lib.MPI_Info): Info = {
      require(info != mpi3.lib.MPI_INFO_NULL)
      val result = new Info
      result.handlePtr.set(info)
      result
    }

    def apply(kvs: (String, String)*): Info = {
      val result = empty
      for (kv <- kvs) result += kv
      result
    }

    def empty = Info()

    def newBuilder: mutable.Builder[(String, String), Info] =
      new mutable.MapBuilder[String, String, Info](empty)

    implicit def canBuildFrom: CanBuildFrom[Info, (String, String), Info] =
      new CanBuildFrom[Info, (String, String), Info] {
        def apply(from: Info) = newBuilder
        def apply() = newBuilder
      }
  }
}
