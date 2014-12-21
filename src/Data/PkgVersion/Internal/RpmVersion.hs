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
module Data.PkgVersion.Internal.RpmVersion
    (
    -- * RpmVersion
      RpmVersion(..)

    -- * Lenses
    , rpmVersion

    -- * Utility functions
    , compareRpmVersion
    )
  where

import Data.Bool (otherwise)
import Data.Data (Data)
import Data.Eq (Eq((==), (/=)))
import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import Data.Ord (Ord(compare), Ordering(EQ))
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
import Data.PkgVersion.Internal.RpmVerCmp (rpmVerCmp)


-- | Rerpresents EVR (@epoch:version-release@) portion of NEVRA
-- (@name-epoch:version-release.architecture@) naming convention used by RPM
-- package manager.
newtype RpmVersion = RpmVersion PkgVersion
  deriving (Data, Generic, Typeable)

rpmVersion
    :: Functor f
    => (PkgVersion -> f PkgVersion)
    -> RpmVersion -> f RpmVersion
rpmVersion f (RpmVersion a) = RpmVersion <$> f a

-- | Compare two unwrapped 'RpmVersion' values using standard 'compare' for
-- 'rpmEpoch', and 'rpmVerCmp' for 'rpmVersion' and 'rpmRelease'.
compareRpmVersion :: PkgVersion -> PkgVersion -> Ordering
compareRpmVersion (PkgVersion e1 v1 r1) (PkgVersion e2 v2 r2)
  | epochCmp   /= EQ = epochCmp
  | versionCmp /= EQ = versionCmp
  | otherwise        = releaseCmp
  where
    epochCmp   = e1 `compare`   e2
    versionCmp = v1 `rpmVerCmp` v2
    releaseCmp = r1 `rpmVerCmp` r2

-- {{{ Instances for RpmVersion -----------------------------------------------

-- | @
-- 'RpmVersion' r1 '==' 'RpmVersion' r2 = 'compareRpmVersion' r1 r2 '==' 'EQ'
-- @
instance Eq RpmVersion where
    RpmVersion r1 == RpmVersion r2 = compareRpmVersion r1 r2 == EQ

-- | @RpmVersion r1 `compare` RpmVersion r2 = r1 `compareRpmVersion` r2@
instance Ord RpmVersion where
    RpmVersion r1 `compare` RpmVersion r2 = r1 `compareRpmVersion` r2

-- | @'def' = 'RpmVersion' 'def'@
instance Default RpmVersion where
    def = RpmVersion def

instance HasEpoch RpmVersion where
    epoch = rpmVersion . epoch

instance HasVersion RpmVersion where
    version = rpmVersion . version

instance HasRelease RpmVersion where
    release = rpmVersion . release

instance Serializable RpmVersion where
    toStrictText (RpmVersion pkgVersion) = toStrictText pkgVersion
    toString (RpmVersion pkgVersion) = toString pkgVersion

instance Show RpmVersion where
    showsPrec i (RpmVersion pkgVersion) = showsPrec i pkgVersion

-- }}} Instances for RpmVersion -----------------------------------------------
