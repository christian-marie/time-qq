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

    -- * TimeZone QuasiQuoters
    timeZone,

    -- * Re-exports
    UTCTime(..),
    TimeZone(..),

    -- * Specify your own formats
    utcFormat,
    timeZoneFormat,
) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Locale.Compat (defaultTimeLocale)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- | ISO8601 date with seconds and optional "." with more precision
-- following to a 'UTCTime'.
--
-- Do not specify a timezone, the time should be UTC.
--
-- >>> [utcIso8601ms| 2099-01-01T00:00:00.42324 |]
-- 2099-01-01 00:00:00.42324 UTC
utcIso8601ms :: QuasiQuoter
utcIso8601ms = utcFormat "%Y-%m-%dT%H:%M:%S%Q"

-- | ISO8601 date with seconds and optional "." with more precision
-- following to a 'UTCTime'.
--
-- Do not specify a timezone, the time should be UTC.
--
-- >>> [utcIso8601| 2048-12-01  |] :: UTCTime
-- 2048-12-01 00:00:00 UTC
utcIso8601 :: QuasiQuoter
utcIso8601 = utcFormat "%Y-%m-%d"

-- | Timezones from a reference string, to a 'TimeZone'.
--
-- >>> [timeZone| BST |] :: TimeZone
-- BST
timeZone :: QuasiQuoter
timeZone = timeZoneFormat "%Z"

-- | Build a 'UTCTime' QuasiQuoter for a given format string, as per
-- 'readTime'.
utcFormat :: String -> QuasiQuoter
utcFormat = timeFormat utcExp

-- | Build a 'TimeZone' QuasiQuoter for a given format string, as per
-- 'readTime'.
timeZoneFormat :: String -> QuasiQuoter
timeZoneFormat = timeFormat timeZoneExp

-- | Build a QuasiQuoter for a given format string given the format interpreter,
-- as per 'readTime'.
timeFormat :: (String -> String -> ExpQ) -> String -> QuasiQuoter
timeFormat formatter format = QuasiQuoter
    { quoteExp   = formatter format
    , quotePat   = const $ error "No quotePat defined for any Data.Time.QQ QQ"
    , quoteType  = const $ error "No quoteType defined for any Data.Time.QQ QQ"
    , quoteDec   = const $ error "No quoteDec defined for any Data.Time.QQ QQ"
    }

-- | Parse a time as per the format and produce a 'UTCTime' 'ExpQ'.
utcExp :: String -> String -> ExpQ
utcExp format input =
    let x = parseTimeOrError True defaultTimeLocale format input :: UTCTime
    in x `seq` [| x |]

-- | Parse a time as per the format and produce a 'TimeZone 'ExpQ'.
timeZoneExp :: String -> String -> ExpQ
timeZoneExp format input =
    let x = parseTimeOrError True defaultTimeLocale format input :: TimeZone
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

-- * Instance for lifting timezones

instance Lift TimeZone where
    lift (TimeZone minutes summer name) = do
        minutes' <- lift minutes
        summer' <- lift summer
        name' <- lift name
        return $ ConE (mkName "TimeZone") `AppE` minutes' `AppE` summer' `AppE` name'
