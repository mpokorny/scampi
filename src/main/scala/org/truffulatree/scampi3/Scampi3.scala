//
// Copyright 2013, Martin Pokorny <martin@truffulatree.org>
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/.
//
package org.truffulatree.scampi3

trait Scampi3
    extends Exceptions
    with Enumerations
    with StatusComponent
    with NamedComponent
    with ErrHandlerComponent
    with ValueBufferComponent
    with CommBufferComponent
    with InfoComponent
    with StructBlockComponent
    with CacheComponent
    with GroupComponent
    with CommComponent
    with WinComponent
    with GroupRankComponent
    with DatatypeComponent
    with PredefDatatypeComponent
    with DerivedDatatypeComponent
    with RequestComponent
    with CombinerComponent
    with FileComponent
    with BufferComponent
    with OpComponent
    with ThreadsComponent
    with Errors
    with Utilities
    with LifecycleMethods {
  this: Mpi3LibraryComponent =>

}
