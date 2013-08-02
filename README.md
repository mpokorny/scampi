ScaMPI: a Scala language interface to MPI
=========================================

ScaMPI does not provide an implementation of MPI; rather, it provides an interface to existing MPI libraries. Currently, the only MPI library tested is [mpich](http://www.mpich.org "mpich"), although it is likely that other, mpich-derived implementations would also work. Additional implementation libraries could be added with little additional Scala code. [BridJ](http://code.google.com/p/bridj "BridJ") is used for the glue between Scala and the MPI library; however, the code generation features of BridJ are not used given the well-defined MPI API.

Installation
------------

ScaMPI may be built and installed using [sbt](http://www.scala-sbt.org/ "sbt") (tested with v0.12.3 ad v0.12.4).

Usage
-----

See the Scala files under the src/test directory for usage examples. Many of the tests are derived form the examples in the MPI report; these tests are in files that are named according to the MPI report chapter in which the tests are found.

Unit tests
----------

Forcing multi-process unit tests (more or less required by MPI) into the sbt testing framework is not pretty. The current implementation nevertheless supports multi-process tests on a single host. The build.sbt file uses the scalaHome setting to ensure that spawned processes use a widely available scala executable (by "widely available", I mean a valid path on all test hosts, although the set of test hosts is currently limited to a single host); you may need to change this setting to point at your desired Scala installation to run the unit tests.

The test suite can only be run by launching sbt with mpiexec. The simplest way to run tests using your preferred MPI library is as follows

```
SCAMPI2_LIBRARY_NAME=mpich mpiexec.hydra sbt -sbt-version 0.12.4 test
```

or

```
SCAMPI2_LIBRARY_NAME=openmpi mpiexec.openmpi sbt -sbt-version 0.12.4 test
```

Running tests with OpenMPI's mpiexec will generate a (harmless) error message from mpiexec because the sbt process itself doesn't call MPI_Init or MPI_Finalize.

Future work
-----------

+ Test with mvapich2

Feedback
--------

This is a very young, experimental project. Please send all comments, suggestions, complaints, requests, *etc.* to <martin@truffulatree.org>.

License
-------

Copyright 2013 Martin Pokorny <martin@truffulatree.org>

The source code files in this project are subject to the terms of the Mozilla Public License, v. 2.0. A copy of the MPL may be found in the [LICENSE](LICENSE) file distributed with this project.
