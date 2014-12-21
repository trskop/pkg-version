{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for versions as understood by RPM package manager
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Data type for versions as understood by RPM package manager.
module Data.RpmVersion
    (
    -- * RpmVersion Data Type
      RpmVersion

    -- * Smart Constructors
    , fromVersion

    -- * Lenses
    , rpmEpoch
    , rpmVersion
    , rpmRelease

    -- * Serialization
    , toStrictText
    , toString
    )
  where

import Data.Function ((.), ($))
import qualified Data.List as List (drop)
import Data.Version (Version(..), showVersion)

import qualified Data.Text as Strict.Text (pack)

import Data.Default.Class (Default(def))

import Data.RpmVersion.Internal.RpmVersion
    ( RpmVersion(..)
    , rpmEpoch
    , rpmRelease
    , rpmVersion
    , toStrictText
    , toString
    )


-- | Convert standard Haskell 'Version' in to 'RpmVersion'.
--
-- >>> fromVersion $ Version [0, 1, 2] ["alpha0", "i386"]
-- 0.1.2-alpha0-i386
-- >>> _rpmEpoch . fromVersion $ Version [0, 1, 2] ["alpha0", "i386"]
-- 0
-- >>> _rpmVersion . fromVersion $ Version [0, 1, 2] ["alpha0", "i386"]
-- "0.1.2"
-- >>> _rpmRelease . fromVersion $ Version [0, 1, 2] ["alpha0", "i386"]
-- "alpha0-i386"
fromVersion :: Version -> RpmVersion
fromVersion Version{versionBranch = b, versionTags = t} = def
    { _rpmVersion = Strict.Text.pack $ showVersion Version
        { versionBranch = b
        , versionTags   = []
        }
    , _rpmRelease = Strict.Text.pack . List.drop 1 $ showVersion Version
        { versionBranch = []
        , versionTags   = t
        }
    }
