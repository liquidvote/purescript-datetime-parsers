module Data.DateTime.Parse (fromString, fromUSAString, toString) where

import Prelude
import Data.Array (replicate)
import Data.Date (day, month, year)
import Data.Date.Parse (fromString, fromUSAString) as ISO8601Date
import Data.DateTime (DateTime(..), date, time)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop, fromCharArray, length, take)
import Data.Time (Time(..), hour, minute, second)
import Data.Time.Component (Hour, Millisecond, Minute, Second)

-- Parse a UTC ISO8061 date time combination string (example "2017-01-03:22:23:22Z").
--
-- Milliseconds are currently ignored and set to 0.
fromString :: String -> Either String DateTime
fromString s = do
  d <- ISO8601Date.fromString s
  hour <- parseHour (take 2 (drop 11 s))
  min <- parseMinute (take 2 (drop 14 s))
  sec <- parseSecond (take 2 (drop 17 s))
  msec <- parseMillisecond (take 3 (drop 20 s))
  pure $ DateTime d (Time hour min sec msec)

-- Parse a USA formatted date time string (example: "04-Jun-2017 13:04")
fromUSAString :: String -> Either String DateTime
fromUSAString s = do
  d <- ISO8601Date.fromUSAString s
  hour <- parseHour (take 2 (drop 12 s))
  min <- parseMinute (take 2 (drop 15 s))
  sec <- parseSecond "00"
  msec <- parseMillisecond "000"
  pure $ DateTime d (Time hour min sec msec)

-- Format a DateTime as a UTC ISO8016 date time combination string (example "2017-01-03:22:23:22Z").
toString :: DateTime -> String
toString dt =
  let d = date dt
      t = time dt
   in    padZeros 2 (fromEnum (year d))
      <> "-"
      <> padZeros 2 (fromEnum (month d))
      <> "-"
      <> padZeros 2 (fromEnum (day d))
      <> "T"
      <> padZeros 2 (fromEnum (hour t))
      <> ":"
      <> padZeros 2 (fromEnum (minute t))
      <> ":"
      <> padZeros 2 (fromEnum (second t))
      <> "Z"

parseHour :: String -> Either String Hour
parseHour s =
  let n = Int.fromString s
   in case n of
        Nothing -> Left "could not parse int for hour"
        Just i -> maybe (Left "invalid hour") Right (toEnum i)

parseMinute :: String -> Either String Minute
parseMinute s =
  let n = Int.fromString s
   in case n of
        Nothing -> Left "could not parse int for minute"
        Just i -> maybe (Left "invalid minute") Right (toEnum i)

parseSecond :: String -> Either String Second
parseSecond s =
  let n = Int.fromString s
   in case n of
        Nothing -> Left "could not parse int for seconds"
        Just i -> maybe (Left "invalid seconds") Right (toEnum i)

parseMillisecond :: String -> Either String Millisecond
parseMillisecond s =
  let n = Int.fromString s
   in case n of
        Nothing -> maybe (Left "expected milliseconds") Right (toEnum 0)
        Just i -> maybe (Left "invalid milliseconds") Right (toEnum i)

-- | Pad a string with the given character up to a maximum length.
padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = fromCharArray (replicate (len - length str) c)

-- | Pad a number with leading zeros up to the given length.
padZeros :: Int -> Int -> String
padZeros len num | num >= 0  = padLeft '0' len (show num)
                 | otherwise = "-" <> padLeft '0' len (show (-num))
