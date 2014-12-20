{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  \<Short text displayed on contents page.\>
-- Copyright:    (c) 2014 Peter Trsko
-- License:      All rights reserved. | BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable | experimental | provisional | stable | frozen
-- Portability:  portable | non-portable (<reason>)
--
-- \<Module description starting at first column.\>
module Data.RpmVersion
    ( RpmVersion
    , fromVersion
    )
  where

import Data.Function ((.), ($))
import qualified Data.List as List (drop)
import Data.Version (Version(..), showVersion)

import qualified Data.Text as Strict.Text (pack)

import Data.RpmVersion.Internal.RpmVersion (RpmVersion(..))


fromVersion :: Version -> RpmVersion
fromVersion Version{versionBranch = b, versionTags = t} = RpmVersion
    { _rpmEpoch   = 0
    , _rpmVersion = Strict.Text.pack $ showVersion Version
        { versionBranch = b
        , versionTags   = []
        }
    , _rpmRelease = Strict.Text.pack . List.drop 1 $ showVersion Version
        { versionBranch = []
        , versionTags   = t
        }
    }
