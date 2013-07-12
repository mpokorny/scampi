//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

import org.specs2.mutable.Specification
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.util.Properties
import scala.collection.JavaConversions._
import scala.collection.mutable
import java.io.File

class ScampiSpecification extends Specification {
  step(mpi2.initThread(mpi2.ThreadLevel.Multiple))

  private lazy val properties = {
    val result = new java.util.Properties
    result.load(
      getClass.getClassLoader.getResourceAsStream("test.properties"))
    result
  }

  def runTest(
    objectName: String,
    numProcesses: Int,
    timeout: Long = 20000): Boolean = {
    val (intercomm, errcodes) = mpi2.Comm.world.synchronized {
      mpi2.Comm.world.spawn(
        properties.getProperty("command").trim,
        List(
          "-classpath",
          List(
            properties.getProperty("classDir"),
            properties.getProperty("pathJarPath").trim).mkString(":"),
          "org.truffulatree.scampi2." + objectName),
        numProcesses,
        mpi2.InfoNull,
        0)
    }
    val reduce = future {
      val result = mpi2.MpiBoolean.alloc(1)
      try {
        intercomm.reduce(
          mpi2.EmptyBuffer,
          result,
          mpi2.Op.logicalAnd,
          intercomm.local(0))
        result(0)
      } finally intercomm.disconnect()
    }
    try {
      Await.result(reduce, Duration(timeout, MILLISECONDS))
    } catch {
      case _: TimeoutException => {
        println(s"timeout in runTest(${objectName},${numProcesses},${timeout})")
        Await.ready(reduce, Duration.Inf)
        false
      }
      case _: InterruptedException =>
        println(s"interrupted in runTest(${objectName},${numProcesses},${timeout})")
        false
    }
  }
}

trait ScampiApp extends DelayedInit {
  protected def args: Array[String] = _args
  private var _args: Array[String] = _
  private val initCode = new mutable.ListBuffer[() => Unit]
  def delayedInit(body: => Unit) {
    initCode += (() => body)
  }
  protected def result = _result(0)
  protected def result_=(bool: Boolean) { _result(0) = bool }
  private var _result: mpi2.SeqValueBuffer[Boolean] = _

  def main(args: Array[String]) {
    this._args = args
    mpi2.init()
    try {
      this._result = mpi2.MpiBoolean(false)
      try {
        for (proc <- initCode) proc()
      } catch {
        case ni: NotImplementedError => {
          println(s"NotImplementedError in test ${getClass.getSimpleName}")
          result = false
        }
        case e: Exception => {
          println(s"Uncaught exception in test ${getClass.getSimpleName}: ${e}")
          println(e.getStackTraceString)
          result = false
        }
      }
      val parentComm = mpi2.Comm.parent.get
      try {
        parentComm.reduce(
          this._result,
          mpi2.EmptyBuffer,
          mpi2.Op.logicalAnd,
          parentComm.remote(0))
      } finally parentComm.disconnect()
    } finally mpi2.fin()
  }
}
