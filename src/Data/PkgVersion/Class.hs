{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type classes for package version types
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type classes for package version types.
module Data.PkgVersion.Class
    (
      HasEpoch(..)
    , HasVersion(..)
    , HasRelease(..)
    , Serializable(..)
    )
  where

import Data.Function ((.))
import Data.Functor (Functor)
import Data.String (String)
import Data.Word (Word32)

import Data.Text (Text)
import qualified Data.Text as Text (unpack)

import Data.Default.Class (Default)


-- | If package version has an epoch field, then instance of this class allows
-- access to it.
class Default a => HasEpoch a where
    -- | Epoch number within @[0, 'Prelude.maxBound' :: 'Word32']@ interval and
    -- defaults to 0 if not present. It is used to determine which version is
    -- greater when standard algorithm would otherwise fail to do it correctly.
    -- In example @2.01@ and @2.1@ are considered equal by
    -- 'Data.PkgVersion.Internal.RpmVerCmp.rpmVerCmp', but may be entirely
    -- different for a certain versioning scheme.
    --
    -- Here is a perfect summary:
    --
    -- \"RPM needs to be able to determine which version numbers are more
    -- recent than others, in order to perform its version comparisons. It's
    -- pretty simple to determine that version 1.5 is older than version 1.6.
    -- But what about 2.01 and 2.1? Or 7.6a and 7.6? There's no way for RPM to
    -- keep up with all the different version-numbering schemes in use. But
    -- there is a solution: epoch numbers.
    --
    -- When RPM can't decipher a package's version number, it's time to pull
    -- out the Epoch tag. This tag is used to help RPM determine version number
    -- ordering. If a packet has an epoch number of 42, what does the 42 mean?
    -- Only that this version of the package is newer than the same package
    -- with an epoch number of 41, but older than the same package with an
    -- epoch number of 43. If you think of epoch numbers as being nothing more
    -- than very simple version numbers, you'll be on the mark. In other words,
    -- Epoch is the most significant component of a package's complete version
    -- identifier with regards to RPM's version comparison algorithm.\"
    --
    -- Source:
    -- <https://ask.fedoraproject.org/en/question/6987/whats-the-meaning-of-the-number-which-appears-sometimes-when-i-use-yum-to-install-a-fedora-package-before-a-colon-at-the-beginning-of-the-name-of-the/?answer=12058#post-id-12058>
    epoch
        :: Functor f
        => (Word32 -> f Word32)
        -> a -> f a

-- | Probably all package version data types will have instance for this type
-- class since it allows access to version value in form of strict 'Text'.
class Default a => HasVersion a where
    -- | Version number consisting of alpha-numeric characters separated by
    -- non-alpha-numeric characters, it can not contain @\'-\'@, because that
    -- is used as a delimiter between version number and release in NEVRA
    -- (@name-epoch:version-release.architecture@) naming convention..
    version
        :: Functor f
        => (Text -> f Text)
        -> a -> f a

-- | If package version has a release field, then instance of this class allows
-- access to it.
class Default a => HasRelease a where
    -- | Release number, similar restrictins as for 'pkgVersion' apply.
    -- Difference is that it may contain @\'-\'@ character, but not @\'.\'@,
    -- since that is used to delimit architecture portion of NEVRA
    -- (@name-epoch:version-release.architecture@) naming convention.
    release
        :: Functor f
        => (Text -> f Text)
        -> a -> f a

-- | Serialize package version in to strict 'Text' and 'String'.
class Default a => Serializable a where
    -- | Serialize package version to strict 'Text'.
    toStrictText :: a -> Text

    -- | Serialize package version in to 'String'.
    --
    -- Default implementation:
    --
    -- @
    -- 'toString' = 'Text.unpack' '.' 'toStrictText'
    -- @
    toString :: a -> String
    toString = Text.unpack . toStrictText
