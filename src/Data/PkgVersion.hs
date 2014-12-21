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
module Data.PkgVersion
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

import Data.Function (($))
import Data.Int (Int)
import Data.Version (Version(..), showVersion)

import qualified Data.Text as Strict.Text (pack)

import Data.Default.Class (Default(def))

import Data.PkgVersion.Internal.PkgVersion (PkgVersion(_pkgVersion))
import Data.PkgVersion.Internal.RpmVersion
    ( RpmVersion
    , rpmEpoch
    , rpmRelease
    , rpmVersion
    , toStrictText
    , toString
    )


-- | Convert simple version number in o PkgVersion.
--
-- >>> fromVersion [0, 1, 2]
-- 0.1.2
-- >>> _pkgEpoch $ fromVersion [0, 1, 2]
-- 0
-- >>> _pkgVersion $ fromVersion [0, 1, 2]
-- "0.1.2"
-- >>> _pkgRelease $ fromVersion [0, 1, 2]
-- ""
fromVersion :: [Int] -> PkgVersion
fromVersion v = def
    { _pkgVersion = Strict.Text.pack $ showVersion Version
        { versionBranch = v
        , versionTags   = []
        }
    }
