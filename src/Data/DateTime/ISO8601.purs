module Data.DateTime.ISO8601 where

import Prelude
import Data.Array (length)
import Data.Date (canonicalDate)
import Data.Date.ISO8601 (parseDay, parseMonth, parseYear)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), drop, split, take)
import Data.Time (Time(..))
import Data.Time.Component (Hour, Millisecond, Minute, Second)

fromString :: String -> Either String DateTime
fromString s =
  let pieces = split (Pattern "-") s
   in if length pieces == 3
        then do
          y <- parseYear pieces
          m <- parseMonth pieces
          d <- parseDay pieces
          hour <- parseHour s
          min <- parseMinute s
          sec <- parseSecond s
          msec <- parseMillisecond s
          pure $ DateTime (canonicalDate y m d) (Time hour min sec msec)
        else Left $ "expected UTC ISO8601 formatted datetime string" <> s

parseHour :: String -> Either String Hour
parseHour s =
  let n = Int.fromString (take 2 (drop 11 s))
   in case n of
        Nothing -> Left "could not parse int for hour"
        Just i -> maybe (Left "invalid hour") Right (toEnum i)

parseMinute :: String -> Either String Minute
parseMinute s =
  let n = Int.fromString (take 2 (drop 14 s))
   in case n of
        Nothing -> Left "could not parse int for minute"
        Just i -> maybe (Left "invalid minute") Right (toEnum i)

parseSecond :: String -> Either String Second
parseSecond s =
  let n = Int.fromString (take 2 (drop 17 s))
   in case n of
        Nothing -> Left "could not parse int for seconds"
        Just i -> maybe (Left "invalid seconds") Right (toEnum i)

parseMillisecond :: String -> Either String Millisecond
parseMillisecond s =
  let n = Int.fromString (take 3 (drop 20 s))
   in case n of
        Nothing -> maybe (Left "expected milliseconds") Right (toEnum 0)
        Just i -> maybe (Left "invalid milliseconds") Right (toEnum i)
