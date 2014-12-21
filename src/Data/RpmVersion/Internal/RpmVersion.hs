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
module Data.RpmVersion.Internal.RpmVersion
    (
    -- * RpmVersion
      RpmVersion(..)

    -- * Lenses
    , rpmEpoch
    , rpmVersion
    , rpmRelease

    -- * Serialization
    , toStrictText
    , toString

    -- * Utility functions
    , compareRpmVersion
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

import Data.RpmVersion.Internal.RpmVerCmp (rpmVerCmp)


-- | Rerpresents EVR (@epoch:version-release@) portion of NEVRA
-- (@name-epoch:version-release.architecture@) naming convention used by RPM
-- package manager.
data RpmVersion = RpmVersion
    { _rpmEpoch   :: !Word32
    , _rpmVersion :: !Strict.Text
    , _rpmRelease :: !Strict.Text
    }
  deriving (Data, Generic, Typeable)

-- | Serialize 'RpmVersion' to strict 'Strict.Text'.
toStrictText :: RpmVersion -> Strict.Text
toStrictText (RpmVersion e version r) = epoch <> version <> release
  where
    colon = Strict.Text.singleton ':'
    dash  = Strict.Text.singleton '-'

    epoch
      | e == 0    = Strict.Text.empty
      | otherwise = Strict.Text.pack (show e) <> colon

    release
      | Strict.Text.null r = Strict.Text.empty
      | otherwise          = dash <> r

-- | Serialize 'RpmVersion' in to 'String'. Internally it uses 'toStrictText'.
toString :: RpmVersion -> String
toString = Strict.Text.unpack . toStrictText

instance Show RpmVersion where
    showsPrec _ = showString . toString

-- | Compare two 'RpmVersion' values using standard 'compare' for 'rpmEpoch',
-- and 'rpmVerCmp' for 'rpmVersion' and 'rpmRelease'.
compareRpmVersion :: RpmVersion -> RpmVersion -> Ordering
compareRpmVersion (RpmVersion e1 v1 r1) (RpmVersion e2 v2 r2)
  | epochCmp   /= EQ = epochCmp
  | versionCmp /= EQ = versionCmp
  | otherwise        = releaseCmp
  where
    epochCmp   = e1 `compare`   e2
    versionCmp = v1 `rpmVerCmp` v2
    releaseCmp = r1 `rpmVerCmp` r2

-- | @
-- r1 '==' r2 = 'compareRpmVersion' r1 r2 '==' 'EQ'
-- @
instance Eq RpmVersion where
    r1 == r2 = compareRpmVersion r1 r2 == EQ

-- | @
-- 'compare' = 'compareRpmVersion'
-- @
instance Ord RpmVersion where
    compare = compareRpmVersion

-- | @
-- 'def' = 'RpmVersion'
--     { '_rpmEpoch'   = 0
--     , '_rpmVersion' = \"\"
--     , '_rpmRelease' = \"\"
--     }
-- @
instance Default RpmVersion where
    def = RpmVersion
        { _rpmEpoch   = 0
        , _rpmVersion = Strict.Text.empty
        , _rpmRelease = Strict.Text.empty
        }

-- {{{ Lenses -----------------------------------------------------------------

-- | Flipped version of 'fmap'. Not exported.
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
{-# INLINE (<$$>) #-}

-- | Epoch number within @[0, 'Prelude.maxBound' :: 'Word32']@ interval and
-- defaults to 0 if not present. It is used to determine which version is
-- greater when 'rpmVerCmp' algorithm would otherwise fail to do it correctly.
-- In example @2.01@ and @2.1@ are considered equal by 'rpmVerCmp', but may be
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
rpmEpoch
    :: Functor f
    => (Word32 -> f Word32)
    -> RpmVersion -> f RpmVersion
rpmEpoch f s@(RpmVersion{_rpmEpoch = a}) =
    f a <$$> \b -> s{_rpmEpoch = b}

-- | Version number consisting of alpha-numeric characters separated by
-- non-alpha-numeric characters, it can not contain @\'-\'@, because that is
-- used as a delimiter between version number and release in NEVRA
-- (@name-epoch:version-release.architecture@) naming convention..
rpmVersion
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> RpmVersion -> f RpmVersion
rpmVersion f s@(RpmVersion{_rpmVersion = a}) =
    f a <$$> \b -> s{_rpmVersion = b}

-- | Release number, similar restrictins as for 'rpmVersion' apply. Difference
-- is that it may contain @\'-\'@ character, but not @\'.\'@, since that is
-- used to delimit architecture portion of NEVRA
-- (@name-epoch:version-release.architecture@) naming convention.
rpmRelease
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> RpmVersion -> f RpmVersion
rpmRelease f s@(RpmVersion{_rpmRelease = a}) =
    f a <$$> \b -> s{_rpmRelease = b}

-- }}} Lenses -----------------------------------------------------------------
