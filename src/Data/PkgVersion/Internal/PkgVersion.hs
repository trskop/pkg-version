{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for RPM and DPKG package version
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Data type for RPM and DPKG package version.
module Data.PkgVersion.Internal.PkgVersion
    (
    -- * PkgVersion
      PkgVersion(..)

    -- * Utility functions
    , compareRpmVersion
--  , compareDpkgVersion
    )
  where

import Data.Bool (otherwise)
import Data.Data (Data)
import Data.Eq (Eq((==), (/=)))
import Data.Function ((.), flip)
import Data.Functor (Functor(fmap))
import Data.Monoid ((<>))
import Data.Ord (Ord(compare), Ordering(EQ))
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Show (Show(show, showsPrec), showString)

import Data.Text as Strict (Text)
import Data.Text as Strict.Text (empty, null, pack, singleton)

import Data.Default.Class (Default(def))

import Data.PkgVersion.Class (IsPkgVersion(..))
import Data.PkgVersion.Internal.RpmVerCmp (rpmVerCmp)


-- | Rerpresents EVR (@epoch:version-release@) portion of NEVRA
-- (@name-epoch:version-release.architecture@) naming convention used by RPM
-- package manager.
data PkgVersion = PkgVersion
    { _pkgEpoch   :: !Word32
    , _pkgVersion :: !Strict.Text
    , _pkgRelease :: !Strict.Text
    }
  deriving (Data, Generic, Typeable)

-- | Compare two 'PkgVersion' values using standard 'compare' for 'pkgEpoch',
-- and 'pkgVerCmp' for 'pkgVersion' and 'pkgRelease'.
compareRpmVersion :: PkgVersion -> PkgVersion -> Ordering
compareRpmVersion (PkgVersion e1 v1 r1) (PkgVersion e2 v2 r2)
  | epochCmp   /= EQ = epochCmp
  | versionCmp /= EQ = versionCmp
  | otherwise        = releaseCmp
  where
    epochCmp   = e1 `compare`   e2
    versionCmp = v1 `rpmVerCmp` v2
    releaseCmp = r1 `rpmVerCmp` r2

{-
-- | Compare two 'PkgVersion' values using standard 'compare' for 'pkgEpoch',
-- and 'dpkgVerCmp' for 'pkgVersion' and 'pkgRelease'.
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

-- {{{ Instances for PkgVersion -----------------------------------------------

-- | @
-- 'def' = 'PkgVersion'
--     { '_pkgEpoch'   = 0
--     , '_pkgVersion' = \"\"
--     , '_pkgRelease' = \"\"
--     }
-- @
instance Default PkgVersion where
    def = PkgVersion
        { _pkgEpoch   = 0
        , _pkgVersion = Strict.Text.empty
        , _pkgRelease = Strict.Text.empty
        }

-- | Flipped version of 'fmap'. Not exported.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
{-# INLINE (<$$>) #-}

instance IsPkgVersion PkgVersion where
    epoch f s@(PkgVersion{_pkgEpoch = a}) =
        f a <$$> \b -> s{_pkgEpoch = b}

    version f s@(PkgVersion{_pkgVersion = a}) =
        f a <$$> \b -> s{_pkgVersion = b}

    release f s@(PkgVersion{_pkgRelease = a}) =
        f a <$$> \b -> s{_pkgRelease = b}

    toStrictText (PkgVersion e v r) = e' <> v <> r'
      where
        colon = Strict.Text.singleton ':'
        dash  = Strict.Text.singleton '-'

        e'
          | e == 0    = Strict.Text.empty
          | otherwise = Strict.Text.pack (show e) <> colon

        r'
          | Strict.Text.null r = Strict.Text.empty
          | otherwise          = dash <> r

instance Show PkgVersion where
    showsPrec _ = showString . toString

-- }}} Instances for PkgVersion -----------------------------------------------
