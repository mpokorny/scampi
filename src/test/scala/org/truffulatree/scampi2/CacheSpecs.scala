//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class CacheSpec extends ScampiSpecification {
  import mpi2._
  "Cache" should {
    "allow writing and reading value" in {
      val c = Comm.self.dup
      val kv = new RestrictedAttributeKeyval[Comm,Int] with KeyvalInt
      val ck = c.newKey(kv)
      val testValue = 42
      c(ck) = testValue
      val value = c(ck)
      c.remove(ck)
      c.free()
      value.get must_== testValue
    }
    "copy Attribute values when desired" in {
      val c = Comm.self.dup
      val kv = new ValAttributeKeyval[Comm,Int] with KeyvalInt
      val ck = c.newKey(kv)
      val testValue = 42
      c(ck) = testValue
      val d = c.dup
      val dupValue = d(ck)
      d.free()
      c.remove(ck)
      c.free()
      dupValue.get must_== testValue
    }
    "not copy Attribute values when desired" in {
      val c = Comm.self.dup
      val kv = new RestrictedAttributeKeyval[Comm,Int] with KeyvalInt
      val ck = c.newKey(kv)
      val testValue = 42
      c(ck) = testValue
      val d = c.dup
      val dupValue = d(ck)
      d.free()
      c.free()
      dupValue must_== None
    }
  }
}
