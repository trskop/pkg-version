{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.Data.PkgVersion.Internal.RpmVerCmp as RpmVerCmp
    (tests)


tests :: [Test]
tests =
    [ testGroup "Data.PkgVersion.Internal.RpmVerCmp" LockFile.tests
    ]
