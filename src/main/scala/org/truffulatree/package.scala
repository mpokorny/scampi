//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree

import scala.collection.JavaConversions._
import org.bridj.{CLong, Pointer, PointerIO}

package object scampi2 {

  protected val properties = {
    val result = new java.util.Properties
    result.load(
      getClass.getClassLoader.getResourceAsStream("scampi2.properties"))
    result
  }

  var libraryName: String = {
    val env = java.lang.System.getenv
    if (env.contains("SCAMPI2_LIBRARY_NAME")) env("SCAMPI2_LIBRARY_NAME")
    else {
      try {
        properties.getProperty("library.name")
      } catch {
        case nse: NoSuchElementException =>
          "Missing library.name property"
      }
    }
  }

  lazy val mpi2: Scampi2 with Mpi2LibraryComponent = {
    if (libraryName == "mpich") mpich.mpi2
    else if (libraryName == "openmpi") openmpi.mpi2
    else throw new Exception(
      s"Unimplemented MPI2 library interface: ${libraryName}")
  }

  def nullPointer[A]: Pointer[A] = Pointer.NULL.asInstanceOf[Pointer[A]]

  object AlignHelper {
    val byteByteSize = java.lang.Byte.SIZE / 8
    val byteAlignment = PointerIO.getByteInstance.getTargetAlignment.toInt
    val intByteSize = java.lang.Integer.SIZE / 8
    val intAlignment = PointerIO.getIntInstance.getTargetAlignment.toInt
    val longByteSize = java.lang.Long.SIZE / 8
    val longAlignment = PointerIO.getLongInstance.getTargetAlignment.toInt
    val floatByteSize = java.lang.Float.SIZE / 8
    val floatAlignment = PointerIO.getFloatInstance.getTargetAlignment.toInt
    val doubleByteSize = java.lang.Double.SIZE / 8
    val doubleAlignment = PointerIO.getDoubleInstance.getTargetAlignment.toInt
    val shortByteSize = java.lang.Short.SIZE / 8
    val shortAlignment = PointerIO.getShortInstance.getTargetAlignment.toInt
    val cLongByteSize = CLong.SIZE
    val cLongAlignment = PointerIO.getCLongInstance.getTargetAlignment.toInt

    def align(offset: Long, alignment: Int): Long = {
      val result = offset + (alignment - (offset % alignment)) % alignment
      result
    }
  }

  implicit def allocateInt(): Pointer[Int] =
    Pointer.allocateInt().as(classOf[Int])

  implicit def allocateLong(): Pointer[Long] =
    Pointer.allocateLong().as(classOf[Long])

  implicit def allocateCLong(): Pointer[CLong] =
    Pointer.allocateCLong()

  implicit def allocatePointer(): Pointer[Pointer[_]] =
    Pointer.allocatePointer()

  def withOutVar[A,B](f: Pointer[A] => B)
                     (implicit alloc: () => Pointer[A]): B = {
    val a = alloc()
    try { f(a) } finally a.release()
  }

  def withInOutVar[A, B](init: A)
                        (f: Pointer[A] => B)
                        (implicit alloc: () => Pointer[A]): B = {
    val a = alloc()
    a.set(init)
    try { f(a) } finally a.release()
  }

  def withByteBuffer[A](len: Int)(f: Pointer[Byte] => A) = {
    val buf = Pointer.allocateBytes(len).as(classOf[Byte])
    try { f(buf) } finally buf.release()
  }

  def withInString[A](str: String)(f: Pointer[Byte] => A) = {
    val sptr = Pointer.pointerToCString(str).as(classOf[Byte])
    try { f(sptr) } finally sptr.release()
  }

  def getString[A](len: Int)(f: (Int, Pointer[Byte]) => A) = {
    val str = Pointer.allocateBytes(len + 1).as(classOf[Byte])
    try {
      f(len + 1, str)
      str.getCString
    } finally str.release()
  }
}
