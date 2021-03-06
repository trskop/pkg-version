{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Data type for pkg-config package version
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Data type for pkg-config package version.
module Data.PkgVersion.Internal.PkgConfigVersion
    (
    -- * PkgConfigVersion
      PkgConfigVersion(..)

    -- * Utility functions
    , comparePkgConfigVersion
    )
  where

import Data.Data (Data)
import Data.Eq (Eq((==)))
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Ord (Ord(compare), Ordering(EQ))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), showString)

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text (empty, unpack)

import Data.Default.Class (Default(def))

import Data.PkgVersion.Class
    ( HasVersion(version)
    , Serializable(toStrictText, toString)
    )
import Data.PkgVersion.Internal.RpmVerCmp (pkgConfigVerCmp)


newtype PkgConfigVersion = PkgConfigVersion Strict.Text
  deriving (Data, Generic, Typeable)

-- | Compare two raw (unwrapped) 'PkgConfigVersion' values using variation of
-- 'Data.PkgVersion.Internal.RpmVerCmp.rpmVerCmp', namely 'pkgConfigVerCmp',
-- that doesn't treat @\'~\'@ character specially.
comparePkgConfigVersion :: Strict.Text -> Strict.Text -> Ordering
comparePkgConfigVersion = pkgConfigVerCmp
{-# INLINE comparePkgConfigVersion #-}

-- {{{ Instances for PkgConfigVersion -----------------------------------------

-- | Implemented using 'comparePkgConfigVersion'.
instance Eq PkgConfigVersion where
    PkgConfigVersion r1 == PkgConfigVersion r2 =
        pkgConfigVerCmp r1 r2 == EQ

-- | Implemented using 'comparePkgConfigVersion'.
instance Ord PkgConfigVersion where
    PkgConfigVersion r1 `compare` PkgConfigVersion r2 =
        r1 `pkgConfigVerCmp` r2

-- | Default value is empty version: @'def' = 'PkgConfigVersion' \"\"@.
instance Default PkgConfigVersion where
    def = PkgConfigVersion Strict.Text.empty

instance HasVersion PkgConfigVersion where
    version f (PkgConfigVersion a) = PkgConfigVersion <$> f a

instance Serializable PkgConfigVersion where
    toStrictText (PkgConfigVersion v) = v
    toString (PkgConfigVersion v) = Strict.Text.unpack v

instance Show PkgConfigVersion where
    showsPrec _ = showString . toString

-- }}} Instances for PkgConfigVersion -----------------------------------------
