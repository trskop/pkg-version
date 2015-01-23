Pkg-version
===========

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/pkg-version.svg)](https://travis-ci.org/trskop/pkg-version)


Description
-----------

Data types for version strings used by RPM and DPKG package manager for
packages.

Naming convention used by RPM package is sometimes refered to as NEVRA which
looks like:

    name-epoch:version-release.architecture

Portion that contains only version information is sometimes called EVR:

    epoch:version-release

This library provides `RpmVersion` data type for EVR and correct `Eq` and `Ord`
instances.



[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
