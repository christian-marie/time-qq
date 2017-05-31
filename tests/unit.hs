--
-- Copyright © 2013-2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Time.QQ
import Test.Hspec

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "UTC QQs" $ do
        it "has correct output #1" $
            show [utcIso8601| 2048-12-01  |]
                `shouldBe` "2048-12-01 00:00:00 UTC"
        it "has correct output #2" $
            show [utcIso8601ms| 2099-01-01T00:00:00.42324 |]
                `shouldBe` "2099-01-01 00:00:00.42324 UTC"
        it "has correct output #3" $
            show [utcIso8601ms| 2099-01-01T00:00:00 |]
                `shouldBe` "2099-01-01 00:00:00 UTC"
    describe "TimeZone QQs" $ do
        it "Shows British Summer Time" $
            show [timeZone| BST |]
                `shouldBe` "BST"
