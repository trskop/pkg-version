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
    )
  where

import Data.Bool (otherwise)
import Data.Data (Data)
import Data.Eq (Eq((==)))
import Data.Function ((.), flip)
import Data.Functor (Functor(fmap))
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Show (Show(show, showsPrec), showString)

import Data.Text as Strict (Text)
import Data.Text as Strict.Text (empty, null, pack, singleton)

import Data.Default.Class (Default(def))

import Data.PkgVersion.Class
    ( HasEpoch(epoch)
    , HasVersion(version)
    , HasRelease(release)
    , Serializable(toStrictText, toString)
    )


-- | Generic package version that works for both RPM and DPKG, but 'Data.Eq.Eq'
-- and 'Data.Ord.Ord' instances have to be defined separately for each.
--
-- It rerpresents version in format:
--
-- > [epoch:]version[-release]
--
-- Both RPM and DPKG use similar versioning scheme, therefore this data type is
-- used as basis for both 'Data.PkgVersion.Internal.RpmVersion.RpmVersion' and
-- 'Data.PkgVersion.Internal.DpkgVersion.DpkgVersion'.
data PkgVersion = PkgVersion
    { _pkgEpoch   :: !Word32
    , _pkgVersion :: !Strict.Text
    , _pkgRelease :: !Strict.Text
    }
  deriving (Data, Generic, Typeable)

-- {{{ Instances for PkgVersion -----------------------------------------------

-- | Instance reflects the shared idea that if epoch is not present then it is
-- considered to be zero and that release may be empty:
--
-- @
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

instance HasEpoch PkgVersion where
    epoch f s@(PkgVersion{_pkgEpoch = a}) =
        f a <$$> \b -> s{_pkgEpoch = b}

instance HasVersion PkgVersion where
    version f s@(PkgVersion{_pkgVersion = a}) =
        f a <$$> \b -> s{_pkgVersion = b}

instance HasRelease PkgVersion where
    release f s@(PkgVersion{_pkgRelease = a}) =
        f a <$$> \b -> s{_pkgRelease = b}

instance Serializable PkgVersion where
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
