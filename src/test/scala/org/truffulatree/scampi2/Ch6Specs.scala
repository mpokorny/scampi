//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi2

class Ch6Spec extends ScampiSpecification {
  "ScaMPI" should {
    "pass example 6.1" in {
      runTest("Ex6_1", 4) must beTrue
    }
    "pass example 6.2" in {
      runTest("Ex6_2", 6) must beTrue
    }
  }
}

object Ex6_1 extends ScampiApp {
  import mpi2._
  val peerComm = Comm.world.dup
  val initialCommSize = peerComm.size / 2
  val initialComm = peerComm.split(
    Some(peerComm.rank / initialCommSize),
    peerComm.rank % initialCommSize).get
  val interComm = initialComm.createIntercomm(
    0,
    peerComm,
    initialCommSize - peerComm.rank,
    101)
  val group =
    if (peerComm.rank / initialCommSize == 0) interComm.group.incl(Seq(0))
    else interComm.group
  val newInterCommOpt = interComm.create(group)
  if (peerComm.rank / initialCommSize == 0)
    result = newInterCommOpt.isDefined == (interComm.rank == 0)
  else
    result = newInterCommOpt.isDefined
}

object Ex6_2 extends ScampiApp {
  import mpi2._
  val peerComm = Comm.world.dup
  val serverCommSize = peerComm.size / 3
  val clientCommSize = peerComm.size - serverCommSize
  assume(clientCommSize % serverCommSize == 0)
  val isServer = peerComm.rank < serverCommSize
  val initialComm =
    peerComm.split(Some(if (isServer) 0 else 1), peerComm.rank).get
  val multipleComm = initialComm.createIntercomm(
    0,
    peerComm,
    if (isServer) serverCommSize else 0,
    101)
  if (!isServer) {
    // client
    val singleComm = multipleComm.split(
      Some(multipleComm.rank % multipleComm.remoteSize), multipleComm.rank).get
    result = ((singleComm.size == clientCommSize / serverCommSize) &&
                 (singleComm.remoteSize == 1))
  } else {
    // server
    val singleComm = multipleComm.split(Some(multipleComm.rank), 0).get
    result = ((singleComm.size == 1) &&
                 (singleComm.remoteSize == clientCommSize / serverCommSize))
  }
}
