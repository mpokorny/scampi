//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

trait GroupRankComponent {
  mpi3: Scampi3 with Mpi3LibraryComponent =>

  sealed case class GroupRank(group: mpi3.Group, rank: Int)
  class LocalRank(c: mpi3.InterComm, r: Int) extends GroupRank(c.group, r)
  class RemoteRank(c: mpi3.InterComm, r: Int) extends GroupRank(c.remoteGroup, r)

  object LocalRank {
    def apply(c: mpi3.InterComm, r: Int) = new LocalRank(c, r)
  }
  object RemoteRank {
    def apply(c: mpi3.InterComm, r: Int) = new RemoteRank(c, r)
  }
}
