Pkg-version
===========


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
