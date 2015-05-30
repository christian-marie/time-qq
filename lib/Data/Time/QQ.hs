--
-- Copyright © 2013-2015 Christian Marie <christian@ponies.io>
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | This module provides quasi quoters for writing time literals.
--
-- First enable the 'QuasiQuotes' language extension, then you will be able to
-- write the the following:
--
-- >>> [utcIso8601| 2048-12-01  |] :: UTCTime
-- 2048-12-01 00:00:00 UTC
--
-- Unparseable dates will throw errors at compile time.
--
-- Zoned time quoters are not provided as the time library doesn't seem to do
-- anything with \"%Z\".
module Data.Time.QQ
(
    -- * ISO8601 UTC time QuasiQuoters
    utcIso8601,
    utcIso8601ms,

    -- * Re-exports
    UTCTime(..),

    -- * Specify your own formats
    utcFormat,
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.Locale

-- | ISO8601 date with seconds and optional "." with more precision
-- following to a 'UTCTime'.
--
-- Do not specify a timezone, the time should be UTC.
--
-- >>> [utcIso8601ms| 2099-01-01T00:00:00.42324 |]
-- 2099-01-01 00:00:00.42324 UTC
utcIso8601ms :: QuasiQuoter
utcIso8601ms = utcFormat . iso8601DateFormat $ Just "%H:%M:%S%Q"

-- | ISO8601 date with seconds and optional "." with more precision
-- following to a 'UTCTime'.
--
-- Do not specify a timezone, the time should be UTC.
--
-- >>> [utcIso8601| 2048-12-01  |] :: UTCTime
-- 2048-12-01 00:00:00 UTC
utcIso8601 :: QuasiQuoter
utcIso8601 = utcFormat $ iso8601DateFormat Nothing

-- | Build a 'UTCTime' QuasiQuoter for a given format string, as per
-- 'readTime'.
utcFormat :: String -> QuasiQuoter
utcFormat format = QuasiQuoter
    { quoteExp   = utcExp format
    , quotePat   = const $ error "No quotePat defined for any Data.Time.QQ QQ"
    , quoteType  = const $ error "No quoteType defined for any Data.Time.QQ QQ"
    , quoteDec   = const $ error "No quoteDec defined for any Data.Time.QQ QQ"
    }

-- | Parse a time as per the format and produce a 'UTCTime' 'ExpQ'.
utcExp :: String -> String -> ExpQ
utcExp format input =
    let x = readTime defaultTimeLocale format input :: UTCTime
    in x `seq` [| x |]


-- * Instances for lifting times

instance Lift UTCTime where
    lift (UTCTime day diff) = do
        day' <- lift day
        diff' <- lift diff
        return $ ConE (mkName "UTCTime") `AppE` day' `AppE` diff'

instance Lift DiffTime where
    lift x = [| toEnum $(lift $ fromEnum x) |]

instance Lift Day where
    lift x = [| toEnum $(lift $ fromEnum x) |]


