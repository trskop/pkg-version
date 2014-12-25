{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Printf for versions
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Convert between various version representations using printf-like interface.
module Data.PkgVersion.AVersion
    (
    -- * Usage Example
    -- $usageExample

    -- * Smart Constructor
      aVersion

    -- ** Type Classes
    , VersionArguments(..)
    , VersionFields(..)

    -- ** Type Wrappers
    , AVersion(..)
    , AString(..)

    -- * Utility Functions
    , splittedVersionToAString
    )
  where

import Prelude (Integer, Integral(toInteger), Num(fromInteger))

import Control.Applicative (Applicative(pure))
import Data.Either (Either(Right))
import Data.Function ((.), ($))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.List ((++), concat, concatMap, map)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Monoid(mempty), (<>))
import Data.String (IsString(fromString), String)
import Data.Version (Version(Version, versionBranch))
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C.Types
    ( CInt
    , CIntMax
    , CLLong
    , CLong
    , CShort
    , CUInt
    , CUIntMax
    , CULLong
    , CULong
    , CUShort
    )
import Text.Show (Show(show))
import System.IO (IO)

import Control.Monad.Trans.Identity (IdentityT(..))
import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)

import Data.Default.Class (Default(def))

import Data.PkgVersion.Class (HasVersion(version))
import Data.PkgVersion.Internal.DpkgVersion (DpkgVersion)
import Data.PkgVersion.Internal.PkgConfigVersion (PkgConfigVersion)
import Data.PkgVersion.Internal.PkgVersion (PkgVersion)
import Data.PkgVersion.Internal.RpmVersion (RpmVersion)


-- | Used as type wrapper for overloaded versions that have instances for
-- 'HasVersion' and 'Default' type classes.
--
-- This way it is possible to use this interface without the need to provide
-- instance for 'VersionArguments'.
newtype AVersion t = AVersion {getAVersion :: t}

-- | Used as type wrapper for overloaded strings that are instances of
-- 'IsString' (implied by the term \"overloaded strings\") and 'Monoid' type
-- classes.
--
-- This way it is possible to use this interface without the need to provide
-- instance for 'VersionArguments'.
--
-- Examples:
--
-- @
-- do
--     AString someString <- aString major minor version maybePatch
--     Data.ByteString.Char8.putStrLn someString
--         -- There is no \"instance VersionArguments ByteString\",
--         -- but this code still works.
-- @
newtype AString s = AString {getAString :: s}

-- | Construct a version. This function behaves as variadic function, for
-- possible arguments see instances of 'VersionArguments' type class.
--
-- >>> aVersion 0 1 2 :: Text   -- Works with defaulting.
-- "0.1.2"
-- >>> aVersion [0, 1, 2 :: Word] (Just (1 :: Word)) :: Text
-- "0.1.2.1"
-- >>> aVersion [0, 1, 2 :: Word] (Nothing :: Maybe Word) :: Text
-- "0.1.2"
-- >>> aVersion ((0, 1, 2) :: (Int, Int, Int)) :: String
-- "0.1.2"
-- >>> aVersion (Version [0, 1, 2] ["foo"]) :: String
-- "0.1.2"
-- >>> aVersion (Version [0, 1, 2] ["foo"]) :: Version
-- Version {versionBranch = [0,1,2], versionTags = []}
aVersion :: VersionArguments args => args
aVersion = versionArguments []

-- {{{ VersionArguments Type Class --------------------------------------------

-- | Class of arguments for 'aVersion' function.
--
-- Note that results are instances of this ('VersionArguments') class and
-- arguments that form individual version fields, or set of fields, are
-- instances of 'VersionFields' type class.
class VersionArguments a where
    versionArguments :: [Integer] -> a

instance
    ( VersionFields a
    , VersionArguments r
    ) => VersionArguments (a -> r)
  where
    versionArguments args arg =
        versionArguments $ args ++ fromVersionFields arg

-- {{{ Applicative Functors and Monads ----------------------------------------

-- | Allows usage in context of any 'Applicative' instance.
--
-- @
-- foo :: Action ()
-- foo = do
--     AString someString <- runIdentityT $ aVersion [0..3 :: Int]
--     liftIO $ My.String.putStrLn someString
-- @
instance
    ( Applicative f
    , VersionArguments r
    ) => VersionArguments (IdentityT f r)
  where
    versionArguments = IdentityT . pure . versionArguments

instance VersionArguments r => VersionArguments (IO r) where
    versionArguments = pure . versionArguments

instance VersionArguments r => VersionArguments (Identity r) where
    versionArguments = pure . versionArguments

instance VersionArguments r => VersionArguments (Maybe r) where
    versionArguments = Just . versionArguments

instance VersionArguments r => VersionArguments (Either a r) where
    versionArguments = Right . versionArguments

-- }}} Applicative Functors and Monads ----------------------------------------

-- {{{ String Types -----------------------------------------------------------

instance (IsString s, Monoid s) => VersionArguments (AString s) where
    versionArguments = AString . constructAString
      where
        constructAString []       = mempty
        constructAString (x : xs) = fromString (show x) <> case xs of
            [] -> mempty
            _  -> fromString "." <> constructAString xs

instance VersionArguments Strict.Text where
    versionArguments = getAString . versionArguments

instance VersionArguments Lazy.Text where
    versionArguments = getAString . versionArguments

-- }}} String Types -----------------------------------------------------------

-- {{{ Version Types ----------------------------------------------------------

instance (Default a, HasVersion a) => VersionArguments (AVersion a) where
    versionArguments vs = AVersion . runIdentity
        $ version (\_ -> Identity $ splittedVersionToAString vs) def

instance VersionArguments RpmVersion where
    versionArguments = getAVersion . versionArguments

instance VersionArguments DpkgVersion where
    versionArguments = getAVersion . versionArguments

instance VersionArguments PkgConfigVersion where
    versionArguments = getAVersion . versionArguments

instance VersionArguments PkgVersion where
    versionArguments = getAVersion . versionArguments

instance VersionArguments Version where
    versionArguments vs = Version (map fromInteger vs) []

-- }}} Version Types ----------------------------------------------------------
-- }}} VersionArguments Type Class --------------------------------------------

-- {{{ VersionFields Type Class -----------------------------------------------

class VersionFields a where
    fromVersionFields :: a -> [Integer]

instance VersionFields a => VersionFields [a] where
    fromVersionFields = concatMap fromVersionFields

-- {{{ Tuples -----------------------------------------------------------------

instance
    ( VersionFields a
    , VersionFields b
    ) => VersionFields (a, b)
  where
    fromVersionFields (a, b) =
        fromVersionFields a ++ fromVersionFields b

instance
    ( VersionFields a
    , VersionFields b
    , VersionFields c
    ) => VersionFields (a, b, c)
  where
    fromVersionFields (a, b, c) = concat
        [ fromVersionFields a
        , fromVersionFields b
        , fromVersionFields c
        ]

instance
    ( VersionFields a
    , VersionFields b
    , VersionFields c
    , VersionFields d
    ) => VersionFields (a, b, c, d)
  where
    fromVersionFields (a, b, c, d) = concat
        [ fromVersionFields a
        , fromVersionFields b
        , fromVersionFields c
        , fromVersionFields d
        ]

instance
    ( VersionFields a
    , VersionFields b
    , VersionFields c
    , VersionFields d
    , VersionFields e
    ) => VersionFields (a, b, c, d, e)
  where
    fromVersionFields (a, b, c, d, e) = concat
        [ fromVersionFields a
        , fromVersionFields b
        , fromVersionFields c
        , fromVersionFields d
        , fromVersionFields e
        ]

instance
    ( VersionFields a
    , VersionFields b
    , VersionFields c
    , VersionFields d
    , VersionFields e
    , VersionFields f
    ) => VersionFields (a, b, c, d, e, f)
  where
    fromVersionFields (a, b, c, d, e, f) = concat
        [ fromVersionFields a
        , fromVersionFields b
        , fromVersionFields c
        , fromVersionFields d
        , fromVersionFields e
        , fromVersionFields f
        ]

instance
    ( VersionFields a
    , VersionFields b
    , VersionFields c
    , VersionFields d
    , VersionFields e
    , VersionFields f
    , VersionFields g
    ) => VersionFields (a, b, c, d, e, f, g)
  where
    fromVersionFields (a, b, c, d, e, f, g) = concat
        [ fromVersionFields a
        , fromVersionFields b
        , fromVersionFields c
        , fromVersionFields d
        , fromVersionFields e
        , fromVersionFields f
        , fromVersionFields g
        ]

instance
    ( VersionFields a
    , VersionFields b
    , VersionFields c
    , VersionFields d
    , VersionFields e
    , VersionFields f
    , VersionFields g
    , VersionFields h
    ) => VersionFields (a, b, c, d, e, f, g, h)
  where
    fromVersionFields (a, b, c, d, e, f, g, h) = concat
        [ fromVersionFields a
        , fromVersionFields b
        , fromVersionFields c
        , fromVersionFields d
        , fromVersionFields e
        , fromVersionFields f
        , fromVersionFields g
        , fromVersionFields h
        ]

-- }}} Tuples -----------------------------------------------------------------

-- {{{ Numbers ----------------------------------------------------------------

instance VersionFields Integer where
    fromVersionFields i = [i]

-- {{{ Data.Int ---------------------------------------------------------------

instance VersionFields Int where
    fromVersionFields i = [toInteger i]

instance VersionFields Int8 where
    fromVersionFields i = [toInteger i]

instance VersionFields Int16 where
    fromVersionFields i = [toInteger i]

instance VersionFields Int32 where
    fromVersionFields i = [toInteger i]

instance VersionFields Int64 where
    fromVersionFields i = [toInteger i]

-- }}} Data.Int ---------------------------------------------------------------

-- {{{ Data.Word --------------------------------------------------------------

instance VersionFields Word where
    fromVersionFields w = [toInteger w]

instance VersionFields Word8 where
    fromVersionFields w = [toInteger w]

instance VersionFields Word16 where
    fromVersionFields w = [toInteger w]

instance VersionFields Word32 where
    fromVersionFields w = [toInteger w]

instance VersionFields Word64 where
    fromVersionFields w = [toInteger w]

-- }}} Data.Word --------------------------------------------------------------

-- {{{ Foreign.C.Types --------------------------------------------------------

instance VersionFields CShort where
    fromVersionFields i = [toInteger i]

instance VersionFields CUShort where
    fromVersionFields i = [toInteger i]

instance VersionFields CInt where
    fromVersionFields i = [toInteger i]

instance VersionFields CUInt where
    fromVersionFields i = [toInteger i]

instance VersionFields CLong where
    fromVersionFields i = [toInteger i]

instance VersionFields CULong where
    fromVersionFields i = [toInteger i]

instance VersionFields CLLong where
    fromVersionFields i = [toInteger i]

instance VersionFields CULLong where
    fromVersionFields i = [toInteger i]

instance VersionFields CIntMax where
    fromVersionFields i = [toInteger i]

instance VersionFields CUIntMax where
    fromVersionFields i = [toInteger i]

-- }}} Foreign.C.Types --------------------------------------------------------
-- }}} Numbers ----------------------------------------------------------------

instance VersionFields Version where
    fromVersionFields = map toInteger . versionBranch

instance VersionFields a => VersionFields (Maybe a) where
    fromVersionFields Nothing   = []
    fromVersionFields (Just vf) = fromVersionFields vf

-- }}} VersionFields Type Class -----------------------------------------------

-- {{{ Utilities --------------------------------------------------------------

-- | Construct standard version string from list of integral numbers.
--
-- >>> splittedVersionToAString [0, 1, 2 :: Word] :: String
-- "0.1.2"
-- >>> splittedVersionToAString [0, 1, 2 :: Int] :: Text
-- "0.1.2"
splittedVersionToAString
    :: (Integral i, Show i, IsString s, Monoid s)
    => [i] -> s
splittedVersionToAString []       = mempty
splittedVersionToAString (v : vs) = fromString (show v) <> case vs of
    [] -> mempty
    _  -> fromString "." <> splittedVersionToAString vs
{-# SPECIALISE splittedVersionToAString :: [Integer] -> String      #-}
{-# SPECIALISE splittedVersionToAString :: [Integer] -> Strict.Text #-}
{-# SPECIALISE splittedVersionToAString :: [Integer] -> Lazy.Text   #-}
{-# SPECIALISE splittedVersionToAString :: [Int]     -> String      #-}
{-# SPECIALISE splittedVersionToAString :: [Int]     -> Strict.Text #-}
{-# SPECIALISE splittedVersionToAString :: [Int]     -> Lazy.Text   #-}
{-# SPECIALISE splittedVersionToAString :: [Word]    -> String      #-}
{-# SPECIALISE splittedVersionToAString :: [Word]    -> Strict.Text #-}
{-# SPECIALISE splittedVersionToAString :: [Word]    -> Lazy.Text   #-}
{-# INLINEABLE splittedVersionToAString #-}

-- }}} Utilities --------------------------------------------------------------

-- $usageExample
--
-- Define your own version scheme:
--
-- @
-- -- | Major and minor portions are mandatory and patch version is optional.
-- -- If patch version is provided, then there may be also a sub-patch level
-- -- version, e.g. to indicate documentation updates and fixes.
-- type MyVersion = (Word, Word, Maybe (Word, Maybe Word))
--
-- -- | Convert my version to String.
-- versionString :: MyVersion -> String
-- versionString = 'getAString' . 'aVersion'
--
-- haskellVersion :: MyVersion -> 'Version'
-- haskellVersion = 'aVersion'
--
-- debianPackageVersion :: MyVersion -> 'DpkgVersion'
-- debianPackageVersion = 'aVersion'
-- @
--
-- In context of above code following would be true:
--
-- >>> versionString (3, 14, Nothing)
-- "3.14"
-- >>> versionString (3, 14, Just (159, Nothing))
-- "3.14.159"
-- >>> versionString (3, 14, Just (159, Just 2653))
-- "3.14.159.2653"
--
-- >>> haskellVersion (3, 14, Nothing)
-- Version {versionBranch = [3,14], versionTags = []}
-- >>> haskellVersion (3, 14, Just (159, Nothing))
-- Version {versionBranch = [3,14,159], versionTags = []}
-- >>> haskellVersion (3, 14, Just (159, Just 2653))
-- Version {versionBranch = [3,14,159,2653], versionTags = []}
--
-- >>> debianPackageVersion (3, 14, Nothing)
-- 3.14
-- >>> debianPackageVersion (3, 14, Just (159, Nothing))
-- 3.14.159
-- >>> debianPackageVersion (3, 14, Just (159, Just 2653))
-- 3.14.159.2653
--
-- Version defined by Cabal can be also reused. For this purpose there is an
-- @instance VersionArguments Version@ and it can be used to take Haskell
-- package version and transform it in to a different representation. Example:
--
-- @
-- import qualified Paths_my_package as My (version)
--
-- debianPackageVersion :: 'DpkgVersion'
-- debianPackageVersion = 'aVersion' My.version
-- @
--
-- Here is a detail of how it works:
--
-- >>> aVersion (Version [3,14,159,2653] []) :: DpkgVersion
-- 3.14.159.2653
