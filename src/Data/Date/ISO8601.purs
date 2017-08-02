module Data.Date.ISO8601 where

import Prelude
import Data.Array (replicate, (!!))
import Data.Array (length) as A
import Data.Date (Date, Day, Month, Year, canonicalDate, day, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), Replacement(..), fromCharArray, length, replace, split)

fromString :: String -> Either String Date
fromString s =
  let pieces = split (Pattern "-") s
   in if A.length pieces == 3
        then do
          y <- parseYear pieces
          m <- parseMonth pieces
          d <- parseDay pieces
          pure $ canonicalDate y m d
        else Left "expected string with format YYYY-MM-DD"

parseYear :: Array String -> Either String Year
parseYear xs = case xs !! 0 of
  Nothing -> Left "expected year"
  Just s -> case Int.fromString s of
    Nothing -> Left "expected integer year in format YYYY"
    Just y -> maybe (Left "expected valid year") Right (toEnum y)

parseMonth :: Array String -> Either String Month
parseMonth xs = case xs !! 0 of
  Nothing -> Left "expected month"
  Just s -> case Int.fromString (replace (Pattern "0") (Replacement "") s) of
    Nothing -> Left "expected integer month in format MM"
    Just y -> maybe (Left ("expected valid month" <> show y)) Right (toEnum y)

parseDay :: Array String -> Either String Day
parseDay xs = case xs !! 0 of
  Nothing -> Left "expected day"
  Just s -> case Int.fromString (replace (Pattern "0") (Replacement "") s) of
    Nothing -> Left "expected integer day in format DD"
    Just y -> maybe (Left "expected valid day") Right (toEnum y)

toString :: Date -> String
toString d =
            padZeros 2 (fromEnum (year d))
  <> "-" <> padZeros 2 (fromEnum (month d))
  <> "-" <> padZeros 2 (fromEnum (day d))

-- | Pad a string with the given character up to a maximum length.
padLeft :: Char -> Int -> String -> String
padLeft c len str = prefix <> str
  where prefix = fromCharArray (replicate (len - length str) c)

-- | Pad a number with leading zeros up to the given length.
padZeros :: Int -> Int -> String
padZeros len num | num >= 0  = padLeft '0' len (show num)
                 | otherwise = "-" <> padLeft '0' len (show (-num))
