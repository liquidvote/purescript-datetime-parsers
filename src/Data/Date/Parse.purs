module Data.Date.Parse (fromString, fromUSAString, toString) where

import Prelude
import Data.Array (replicate, (!!))
import Data.Array (length) as A
import Data.Date (Date, Day, Month(..), Year, canonicalDate, day, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), fromCharArray, length, replace, split, take)

-- Parse an ISO8061 date string (example: "2017-01-03").
fromString :: String -> Either String Date
fromString s =
  let pieces = split (Pattern "-") s
   in if A.length pieces == 3
        then do
          y <- parseYear (pieces !! 0)
          m <- parseMonth (pieces !! 1)
          d <- parseDay (pieces !! 2)
          pure $ canonicalDate y m d
        else Left "expected string with format YYYY-MM-DD"

-- Parse a USA formatted date string (example: "04-Jun-2017")
fromUSAString :: String -> Either String Date
fromUSAString s =
  let pieces = split (Pattern "-") s
   in if A.length pieces == 3
        then do
          d <- parseDay (pieces !! 0)
          m <- parseMonthAbbr (pieces !! 1)
          y <- parseYear (take 4 <$> (pieces !! 2))
          pure $ canonicalDate y m d
        else Left "expected string with format DD M YYYY HH:mm"

-- Format a Date as an ISO8061 date string (example: "2017-01-03").
toString :: Date -> String
toString d =
            padZeros 2 (fromEnum (year d))
  <> "-" <> padZeros 2 (fromEnum (month d))
  <> "-" <> padZeros 2 (fromEnum (day d))
  <> "T00:00:00Z"

parseYear :: Maybe String -> Either String Year
parseYear x = case x of
  Nothing -> Left "expected year"
  Just s -> case Int.fromString s of
    Nothing -> Left "expected integer year in format YYYY"
    Just y -> maybe (Left "expected valid year") Right (toEnum y)

parseMonth :: Maybe String -> Either String Month
parseMonth x = case x of
  Nothing -> Left "expected month"
  Just s -> case Int.fromString (replace (Pattern "0") (Replacement "") s) of
    Nothing -> Left "expected integer month in format MM"
    Just y -> maybe (Left ("expected valid month" <> show x)) Right (toEnum y)

parseMonthAbbr :: Maybe String -> Either String Month
parseMonthAbbr x = case x of
  Nothing -> Left "expected month"
  Just s -> case s of
    "Jan" -> Right January
    "Feb" -> Right February
    "Mar" -> Right March
    "Apr" -> Right April
    "May" -> Right May
    "Jun" -> Right June
    "Jul" -> Right July
    "Aug" -> Right August
    "Sep" -> Right September
    "Oct" -> Right October
    "Nov" -> Right November
    "Dec" -> Right December
    _ -> Left "expected valid month abbreviation"

parseDay :: Maybe String -> Either String Day
parseDay x = case x of
  Nothing -> Left "expected day"
  Just s -> case Int.fromString (replace (Pattern "0") (Replacement "") (take 2 s)) of
    Nothing -> Left "expected integer day in format DD"
    Just y -> maybe (Left "expected valid day") Right (toEnum y)

-- | Pad a string with the given character up to a maximum length.
padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = fromCharArray (replicate (len - length str) c)

-- | Pad a number with leading zeros up to the given length.
padZeros :: Int -> Int -> String
padZeros len num | num >= 0  = padLeft '0' len (show num)
                 | otherwise = "-" <> padLeft '0' len (show (-num))
