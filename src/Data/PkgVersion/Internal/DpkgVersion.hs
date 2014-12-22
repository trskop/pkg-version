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


-- | Rerpresents EVR (@epoch:version-release@) portion of NEVRA
-- (@name-epoch:version-release.architecture@) naming convention used by RPM
-- package manager.
newtype DpkgVersion = DpkgVersion PkgVersion
  deriving (Data, Generic, Typeable)

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
-- | @'DpkgVersion' r1 '==' 'DpkgVersion' r2 =
-- 'compareDpkgVersion' r1 r2 '==' 'EQ'@
instance Eq DpkgVersion where
    DpkgVersion r1 == DpkgVersion r2 = compareDpkgVersion r1 r2 == EQ

-- | @DpkgVersion r1 `compare` DpkgVersion r2 = r1 `compareDpkgVersion` r2@
instance Ord DpkgVersion where
    DpkgVersion r1 `compare` DpkgVersion r2 = r1 `compareDpkgVersion` r2
-}

-- | @'def' = 'DpkgVersion' 'def'@
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
