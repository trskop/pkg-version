{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for RPM version
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Data type for RPM version.
module Data.PkgVersion.Internal.DpkgVersion
    (
    -- * DpkgVersion
      DpkgVersion(..)

    -- * Lenses
    , dpkgVersion

    -- * Utility functions
--  , compareDpkgVersion
    )
  where

--import Data.Bool (otherwise)
import Data.Data (Data)
--import Data.Eq (Eq((==), (/=)))
import Data.Function ((.))
import Data.Functor (Functor, (<$>))
--import Data.Ord (Ord(compare), Ordering(EQ))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec))

import Data.Default.Class (Default(def))

import Data.PkgVersion.Class
    ( HasEpoch(epoch)
    , HasVersion(version)
    , HasRelease(release)
    , Serializable(toStrictText, toString)
    )
import Data.PkgVersion.Internal.PkgVersion (PkgVersion(PkgVersion))
--import Data.PkgVersion.Internal.DpkgVerCmp (dpkgVerCmp)


-- | <https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version Debian Policy Manual: 5.6.12 Version>
-- defines version format as:
--
-- > [epoch:]upstream_version[-debian_revision]
--
-- Here is a short description of individual fields:
--
-- [@epoch@]
--   This is a single (generally small) unsigned integer. It may be omitted, in
--   which case zero is assumed. If it is omitted then the @upstream_version@
--   may not contain any colons. Its purpose is to allow correct version
--   comparision in cases when comparing @upstream_version@ values wouldn't
--   yield correct result. Like when versioning schema changes, etc.
--
-- [@upstream_version@]
--   This is the main, and mandatory, part of the version number. It is usually
--   the version number of the original (\"upstream\") package from which the
--   @.deb@ file has been made, if this is applicable. Usually this will be in
--   the same format as that specified by the upstream author(s); however, it
--   may need to be reformatted to fit into the package management system's
--   format and comparison scheme. It may contain only alphanumeric characters
--   and @\'.\'@, @\'+\'@, @\'-\'@, @\':\'@ and @\'~\'@, and it should start
--   with a digit. If there is no @debian_revision@ present, then @\'-\'@ is
--   not allowed; and if there is no epoch portion present, then @\':\'@ is not
--   allowed either.
--
-- [@debian_revision@]
--   Part of the version number that specifies the version of Debian package
--   based on the same upstream version. It may contain only alphanumeric
--   characters and the characters @\'+\'@, @\'.\'@ and @\'~\'@. If this part
--   is omitted, then @upstream_version@ may not contain @\'-\'@ character, and
--   this part of version is considered equal to 0.
newtype DpkgVersion = DpkgVersion PkgVersion
  deriving (Data, Generic, Typeable)

-- | Lens for accessing 'PkgVersion' wrapped inside 'DpkgVersion'.
dpkgVersion
    :: Functor f
    => (PkgVersion -> f PkgVersion)
    -> DpkgVersion -> f DpkgVersion
dpkgVersion f (DpkgVersion a) = DpkgVersion <$> f a

{-
-- | Compare two unwrapped 'DpkgVersion' values using standard 'compare' for
-- 'epoch', and 'dpkgVerCmp' for 'version' and 'release'.
compareDpkgVersion :: PkgVersion -> PkgVersion -> Ordering
compareDpkgVersion (PkgVersion e1 v1 r1) (PkgVersion e2 v2 r2)
  | epochCmp   /= EQ = epochCmp
  | versionCmp /= EQ = versionCmp
  | otherwise        = releaseCmp
  where
    epochCmp   = e1 `compare`   e2
    versionCmp = v1 `dpkgVerCmp` v2
    releaseCmp = r1 `dpkgVerCmp` r2
-}

-- {{{ Instances for DpkgVersion ----------------------------------------------

{-
-- | Implemented using 'compareDpkgVersion'.
instance Eq DpkgVersion where
    DpkgVersion r1 == DpkgVersion r2 = compareDpkgVersion r1 r2 == EQ

-- | Implemented using 'compareDpkgVersion'.
instance Ord DpkgVersion where
    DpkgVersion r1 `compare` DpkgVersion r2 = r1 `compareDpkgVersion` r2
-}

-- | Implemented using 'Default' instance for 'PkgVersion'.
instance Default DpkgVersion where
    def = DpkgVersion def

instance HasEpoch DpkgVersion where
    epoch = dpkgVersion . epoch

instance HasVersion DpkgVersion where
    version = dpkgVersion . version

instance HasRelease DpkgVersion where
    release = dpkgVersion . release

instance Serializable DpkgVersion where
    toStrictText (DpkgVersion pkgVersion) = toStrictText pkgVersion
    toString (DpkgVersion pkgVersion) = toString pkgVersion

instance Show DpkgVersion where
    showsPrec i (DpkgVersion pkgVersion) = showsPrec i pkgVersion

-- }}} Instances for DpkgVersion ----------------------------------------------
