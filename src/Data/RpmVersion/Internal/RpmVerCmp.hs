{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  RPM version comparision
-- Copyright:    (c) 2014 Peter Trsko
-- License:      All rights reserved. | BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Version comparision based on RPM package manager.
module Data.RpmVersion.Internal.RpmVerCmp
    ( rpmVerCmp
    , pkgVerCmp
    )
  where

import Control.Applicative (liftA2)
import Data.Bool (Bool(False, True), (&&), not, otherwise)
import Data.Char (Char, isAlpha, isAlphaNum, isDigit)
import Data.Eq (Eq((==), (/=)))
import Data.Function ((.), ($), on)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord(compare), Ordering(EQ, GT, LT))

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text


-- {{{ RPM Version Comparision ------------------------------------------------

-- | Version comparision as understood by /RPM/ package manager.
rpmVerCmp :: Strict.Text -> Strict.Text -> Ordering
rpmVerCmp = compareVersionsWith rpmBreak

-- | Break version in to components the way how /RPM/ understands them.
rpmBreak
    :: Strict.Text
    -> ((Bool, Strict.Text), Strict.Text)
    -- ^ Returns tuple of version component and reminder. Version component is
    -- itself a tuple where first argument is boolean that is 'True' if version
    -- component hadn't started with character @\'~\'@ and 'False' otherwise.
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
pkgVerCmp :: Strict.Text -> Strict.Text -> Ordering
pkgVerCmp = compareVersionsWith pkgBreak

-- | Break version in to components the way how /pkg-config/ understands them.
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

isTilde, notTilde, notAlphaNum :: Char -> Bool
isTilde = (== '~')
notTilde = not . isTilde
notAlphaNum = not . isAlphaNum

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
  | otherwise = (compareFragments `on` breakWith someBreak) v1 v2

-- | Compare version splitted in to corresponding components.
compareFragments :: Ord a => [a] -> [a] -> Ordering
compareFragments []       []       = EQ
compareFragments _        []       = GT
compareFragments []       _        = LT
compareFragments (x : xs) (y : ys) =
    if r /= EQ then r else compareFragments xs ys
  where
    r = x `compare` y

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

-- }}} Generic Helper Functions -----------------------------------------------
