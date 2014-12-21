{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  RPM version comparision
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Version comparision based on RPM package manager.
module Data.PkgVersion.Internal.RpmVerCmp
    (
    -- * RPM Version Comparison
      rpmVerCmp
    , rpmBreak

    -- * Pkg-config Version Comparison
    , pkgVerCmp
    , pkgBreak

    -- * Utility Functions
    , compareVersionsWith
    , breakWith
    , spanFor

    -- ** Character Predicates
    , notAlphaNum
    , isTilde
    , notTilde
    )
  where

import Control.Applicative (liftA2)
import Data.Bool (Bool(False, True), (&&), not, otherwise)
import Data.Char (Char, isAlpha, isAlphaNum, isDigit)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), on)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord(compare), Ordering(EQ))

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text


-- {{{ RPM Version Comparision ------------------------------------------------

-- | Version comparision as understood by /RPM/ package manager.
--
-- Implementation is based on:
-- <http://blog.jasonantman.com/2014/07/how-yum-and-rpm-compare-versions/>
rpmVerCmp :: Strict.Text -> Strict.Text -> Ordering
rpmVerCmp = compareVersionsWith rpmBreak

-- | Break string in to a version component and the rest of a string. It does
-- it in a way how /RPM/ package manager understands versions.
rpmBreak
    :: Strict.Text
    -> ((Bool, Strict.Text), Strict.Text)
    -- ^ Returns tuple of version component and reminder. Version component is
    -- itself a tuple where first argument is boolean that is 'True' if version
    -- component hadn't started with character @\'~\'@ and 'False' otherwise.
    -- This does preserve @\'~\'@ semantics in terms of standard 'Eq' instance
    -- for tuples.
rpmBreak s
  | Strict.Text.null s = ((True, s), s)                 -- = ((True, ""), "")
  | otherwise          = case Strict.Text.uncons s' of
    Nothing       -> ((True, s'), s')                   -- = ((True, ""), "")
    Just (c, s'')
      | isTilde c    -> case Strict.Text.uncons s'' of
        Nothing      -> ((False, s''), s'')             -- = ((False, ""), "")
        Just (c', _) ->
            let (frag, rest) = spanFor c' s''
            in  ((False, frag), rest)
      | otherwise ->
        let (frag, rest) = spanFor c s'
        in  ((True, frag), rest)
  where
    s' = Strict.Text.dropWhile (notAlphaNum <&&> notTilde) s :: Strict.Text
    (<&&>) = liftA2 (&&)

-- }}} RPM Version Comparision ------------------------------------------------

-- {{{ Pkg-config Version Comparision -----------------------------------------

-- | Version comparision as understood by /pkg-config/ tool.
--
-- Difference between 'rpmVerCmp' and 'pkgVerCmp' is that 'pkgVerCmp' doesn't
-- understand magic @~@ character.
pkgVerCmp :: Strict.Text -> Strict.Text -> Ordering
pkgVerCmp = compareVersionsWith pkgBreak

-- | Break string in to a version component and the resto of a string. It does
-- it in a way how /pkg-config/ understands versions.
pkgBreak :: Strict.Text -> (Strict.Text, Strict.Text)
pkgBreak s
  | Strict.Text.null s = (s, s)                         -- = ("", "")
  | otherwise          = case Strict.Text.uncons s' of
    Nothing     -> (s', s')                             -- = ("", "")
    Just (c, _) -> spanFor c s'
  where
    s' = Strict.Text.dropWhile notAlphaNum s :: Strict.Text

-- }}} Pkg-config Version Comparision -----------------------------------------

-- {{{ Generic Helper Functions -----------------------------------------------

-- | Split version string in to fragments, using provided function, and and
-- compare it piece by piece. See also 'breakWith' and 'compareFragments'.
compareVersionsWith
    :: Ord a
    => (Strict.Text -> (a, Strict.Text))
    -- ^ Break function.
    -> Strict.Text
    -> Strict.Text
    -> Ordering
compareVersionsWith someBreak v1 v2
  | v1 == v2  = EQ
  | otherwise = (compare `on` breakWith someBreak) v1 v2
    -- Function compare specialized to "Ord a => [a] -> [a] -> Ordering" does
    -- the right thing when comparing version string fragments.

-- | Iterate provided break function until it returns empty reminder.
breakWith
    :: Ord a
    => (Strict.Text -> (a, Strict.Text))
    -- ^ Break function.
    -> Strict.Text
    -- ^ Version string.
    -> [a]
    -- ^ Version string splitted in to components.
breakWith someBreak str
  | Strict.Text.null str = []
  | otherwise            = frag : breakWith someBreak rest
      where
        (frag, rest) = someBreak str

-- | Variant of 'Strict.Text.span' that changes its mode of operation depending
-- on the class of character passed to it as a first argument.
--
-- 1. If first argument is a character representing alphabet character (not
--    symbol or digit), then this function does @'Strict.Text.span' 'isAlpha'@.
-- 2. If first argument is a character representing digit, then this function
--    does @'Strict.Text.span' 'isDigit'@ and it removes all leading zeros.
-- 3. If first argument is neither alphabet character nor digit, then this
--    function behaves equivalently to
--    @'Strict.Text.span' ('Data.Bool.const' 'False')@
--
-- Examples:
--
-- >>> spanFor '0' "0123abc"
-- ("123","abc")
-- >>> spanFor 'a' "0123abc"
-- ("","0123abc")
-- >>> spanFor 'a' "abc0123"
-- ("abc","0123")
-- >>> spanFor '-' "abc0123"
-- ("","0123abc")
-- >>> spanFor '-' "0123abc"
-- ("","abc0123")
spanFor :: Char -> Strict.Text -> (Strict.Text, Strict.Text)
spanFor c s
  | isAlpha c = Strict.Text.span isAlpha s
  | isDigit c =
    let result@(frag, rest) =
            Strict.Text.span isDigit $ Strict.Text.dropWhile isZero s
                -- Leading zeros are removed: "0001" -> "1", because
                -- "001" `compare` "01" /= EQ.
    in if Strict.Text.null frag
        then (zero, rest)
            -- Function dropWhile may have removed the whole number.
        else result
  | otherwise = (Strict.Text.empty, s)
  where
    isZero :: Char -> Bool
    isZero = (== '0')

    zero :: Strict.Text
    zero = Strict.Text.singleton '0'

-- | @'isTilde' = ('==' \'~\')@
isTilde :: Char -> Bool
isTilde = (== '~')

-- | @'notTilde' = 'not' '.' 'isTilde'@
notTilde :: Char -> Bool
notTilde = not . isTilde

-- | @'notAlphaNum' = 'not' '.' 'isAlphaNum'@
notAlphaNum :: Char -> Bool
notAlphaNum = not . isAlphaNum

-- }}} Generic Helper Functions -----------------------------------------------
