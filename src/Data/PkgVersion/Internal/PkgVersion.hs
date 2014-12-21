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

    -- * Lenses
    , pkgEpoch
    , pkgVersion
    , pkgRelease

    -- * Serialization
    , toStrictText
    , toString

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
import Data.String (String)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Show (Show(show, showsPrec), showString)

import Data.Text as Strict (Text)
import Data.Text as Strict.Text (empty, null, pack, singleton, unpack)

import Data.Default.Class (Default(def))

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

-- | Serialize 'PkgVersion' to strict 'Strict.Text'.
toStrictText :: PkgVersion -> Strict.Text
toStrictText (PkgVersion e version r) = epoch <> version <> release
  where
    colon = Strict.Text.singleton ':'
    dash  = Strict.Text.singleton '-'

    epoch
      | e == 0    = Strict.Text.empty
      | otherwise = Strict.Text.pack (show e) <> colon

    release
      | Strict.Text.null r = Strict.Text.empty
      | otherwise          = dash <> r

-- | Serialize 'PkgVersion' in to 'String'. Internally it uses 'toStrictText'.
toString :: PkgVersion -> String
toString = Strict.Text.unpack . toStrictText

instance Show PkgVersion where
    showsPrec _ = showString . toString

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

-- {{{ Lenses -----------------------------------------------------------------

-- | Flipped version of 'fmap'. Not exported.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
{-# INLINE (<$$>) #-}

-- | Epoch number within @[0, 'Prelude.maxBound' :: 'Word32']@ interval and
-- defaults to 0 if not present. It is used to determine which version is
-- greater when 'pkgVerCmp' algorithm would otherwise fail to do it correctly.
-- In example @2.01@ and @2.1@ are considered equal by 'pkgVerCmp', but may be
-- entirely different for a certain versioning scheme.
--
-- Here is a perfect summary:
--
-- \"RPM needs to be able to determine which version numbers are more recent
-- than others, in order to perform its version comparisons. It's pretty simple
-- to determine that version 1.5 is older than version 1.6. But what about 2.01
-- and 2.1? Or 7.6a and 7.6? There's no way for RPM to keep up with all the
-- different version-numbering schemes in use. But there is a solution: epoch
-- numbers.
--
-- When RPM can't decipher a package's version number, it's time to pull out
-- the Epoch tag. This tag is used to help RPM determine version number
-- ordering. If a packet has an epoch number of 42, what does the 42 mean? Only
-- that this version of the package is newer than the same package with an
-- epoch number of 41, but older than the same package with an epoch number of
-- 43. If you think of epoch numbers as being nothing more than very simple
-- version numbers, you'll be on the mark. In other words, Epoch is the most
-- significant component of a package's complete version identifier with
-- regards to RPM's version comparison algorithm.\"
--
-- Source:
-- <https://ask.fedoraproject.org/en/question/6987/whats-the-meaning-of-the-number-which-appears-sometimes-when-i-use-yum-to-install-a-fedora-package-before-a-colon-at-the-beginning-of-the-name-of-the/?answer=12058#post-id-12058>
pkgEpoch
    :: Functor f
    => (Word32 -> f Word32)
    -> PkgVersion -> f PkgVersion
pkgEpoch f s@(PkgVersion{_pkgEpoch = a}) =
    f a <$$> \b -> s{_pkgEpoch = b}

-- | Version number consisting of alpha-numeric characters separated by
-- non-alpha-numeric characters, it can not contain @\'-\'@, because that is
-- used as a delimiter between version number and release in NEVRA
-- (@name-epoch:version-release.architecture@) naming convention..
pkgVersion
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgVersion -> f PkgVersion
pkgVersion f s@(PkgVersion{_pkgVersion = a}) =
    f a <$$> \b -> s{_pkgVersion = b}

-- | Release number, similar restrictins as for 'pkgVersion' apply. Difference
-- is that it may contain @\'-\'@ character, but not @\'.\'@, since that is
-- used to delimit architecture portion of NEVRA
-- (@name-epoch:version-release.architecture@) naming convention.
pkgRelease
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> PkgVersion -> f PkgVersion
pkgRelease f s@(PkgVersion{_pkgRelease = a}) =
    f a <$$> \b -> s{_pkgRelease = b}

-- }}} Lenses -----------------------------------------------------------------
