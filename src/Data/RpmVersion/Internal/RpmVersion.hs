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
    , toStrictText

    -- * Lenses
    , rpmEpoch
    , rpmVersion
    , rpmRelease
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
import Text.Show (Show(show, showsPrec))

import Data.Text as Strict (Text)
import Data.Text as Strict.Text (empty, null, pack, singleton)

import Data.Default.Class (Default(def))

import Data.RpmVersion.Internal.RpmVerCmp (rpmVerCmp)


-- | Rerpresents EVR portion of NEVRA naming convention used by RPM.
data RpmVersion = RpmVersion
    { _rpmEpoch   :: !Word32
    , _rpmVersion :: !Strict.Text
    , _rpmRelease :: !Strict.Text
    }
  deriving (Data, Generic, Typeable)

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

instance Show RpmVersion where
    showsPrec i = showsPrec i . toStrictText

compareRpmVersion :: RpmVersion -> RpmVersion -> Ordering
compareRpmVersion (RpmVersion e1 v1 r1) (RpmVersion e2 v2 r2)
  | epochCmp   /= EQ = epochCmp
  | versionCmp /= EQ = versionCmp
  | otherwise        = releaseCmp
  where
    epochCmp   = e1 `compare`   e2
    versionCmp = v1 `rpmVerCmp` v2
    releaseCmp = r1 `rpmVerCmp` r2

instance Eq RpmVersion where
    r1 == r2 = compareRpmVersion r1 r2 == EQ

instance Ord RpmVersion where
    compare = compareRpmVersion

instance Default RpmVersion where
    def = RpmVersion
        { _rpmEpoch = 0
        , _rpmVersion = Strict.Text.empty
        , _rpmRelease = Strict.Text.empty
        }

-- {{{ Lenses -----------------------------------------------------------------

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap
{-# INLINE (<$$>) #-}

rpmEpoch
    :: Functor f
    => (Word32 -> f Word32)
    -> RpmVersion -> f RpmVersion
rpmEpoch f s@(RpmVersion{_rpmEpoch = a}) =
    f a <$$> \b -> s{_rpmEpoch = b}

rpmVersion
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> RpmVersion -> f RpmVersion
rpmVersion f s@(RpmVersion{_rpmVersion = a}) =
    f a <$$> \b -> s{_rpmVersion = b}

rpmRelease
    :: Functor f
    => (Strict.Text -> f Strict.Text)
    -> RpmVersion -> f RpmVersion
rpmRelease f s@(RpmVersion{_rpmRelease = a}) =
    f a <$$> \b -> s{_rpmRelease = b}

-- }}} Lenses -----------------------------------------------------------------
