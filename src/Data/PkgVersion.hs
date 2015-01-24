{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for versions as understood by RPM package manager
-- Copyright:    (c) 2014-2015, Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Data type for versions as understood by RPM package manager.
module Data.PkgVersion
    (
    -- * Package Version Data Types
      DpkgVersion
    , PkgConfigVersion
    , RpmVersion

    -- * Abstract interface
    , module Data.PkgVersion.Class
    )
  where

import Data.PkgVersion.Class
import Data.PkgVersion.Internal.DpkgVersion (DpkgVersion)
import Data.PkgVersion.Internal.PkgConfigVersion (PkgConfigVersion)
import Data.PkgVersion.Internal.RpmVersion (RpmVersion)
