{-# LANGUAGE DerivingStrategies #-}
module Advent.Day4
  ( day4pt1
  , day4pt2
  ) where

import Data.Char (isDigit, isHexDigit)
import Data.Either (rights)
import Data.List (sort)
import Data.List.Split (splitOn, splitOneOf)

maybeField :: String -> String -> Maybe String
maybeField fld xs = if length ys == 2 && head ys == fld
                    then Just (head $ tail ys)
                    else Nothing
  where ys = splitOn ":" xs

eitherField :: String -> String -> Either String String
eitherField fld xs =
  case maybeField fld xs of
    Nothing  -> Left $ "Missing field: " ++ fld
    (Just b) -> Right b

data Passport = Passport
  { byr :: String
  , cid :: Maybe String
  , ecl :: String
  , eyr :: String
  , hcl :: String
  , hgt :: String
  , iyr :: String
  , pid :: String
  } deriving stock Show

passportParser :: [String] -> Either String Passport
passportParser [byrs, cids, ecls, eyrs, hcls, hgts, iyrs, pids] = 
  Passport <$> eitherField "byr" byrs 
           <*> Right (Just cids)
           <*> eitherField "ecl" ecls
           <*> eitherField "eyr" eyrs
           <*> eitherField "hcl" hcls
           <*> eitherField "hgt" hgts
           <*> eitherField "iyr" iyrs
           <*> eitherField "pid" pids
passportParser [byrs, ecls, eyrs, hcls, hgts, iyrs, pids] = 
  Passport <$> eitherField "byr" byrs 
           <*> Right Nothing
           <*> eitherField "ecl" ecls
           <*> eitherField "eyr" eyrs
           <*> eitherField "hcl" hcls
           <*> eitherField "hgt" hgts
           <*> eitherField "iyr" iyrs
           <*> eitherField "pid" pids
passportParser _ = Left "Not enough fields"

day4pt1 :: FilePath -> IO Int
day4pt1 fp = do
  ps <- parsePassportFile fp 
  pure $ length $ rights ps

isNumOfSize :: Int -> String -> Bool
isNumOfSize x xs = length xs == x && all isDigit xs

isNumInRange :: (Int, Int) -> String -> Bool
isNumInRange (a,b) xs = all isDigit xs && y >= a && y <= b
  where
    y = read xs :: Int

parsePassportFile :: FilePath -> IO [Either String Passport]
parsePassportFile fname = readFile fname >>= \xs -> pure $ map (passportParser . sort . splitOneOf " \n") (splitOn "\n\n" xs)

validateNumInRange :: (Int, Int) -> String -> String -> Either String String
validateNumInRange (x,y) caption xs = 
  if isNumInRange (x,y) xs
    then Right xs
    else Left $ "Invalid " ++ caption ++ ": " ++ xs

validateHeight :: String -> Either String String
validateHeight xs | units == "cm" && isNumInRange (150,193) ys = Right xs
                  | units == "in" && isNumInRange (59,76)   ys = Right xs
                  | otherwise = Left $ "Invalid Hgt: " ++ xs
  where
    rs = reverse xs
    units = reverse (take 2 rs)
    ys = reverse (drop 2 rs)

validateHairColour :: String -> Either String String
validateHairColour xs = 
  if length xs == 7 && head xs == '#' && all isHexDigit (tail xs) 
    then Right xs 
    else Left "Invalid Hcl"

validEyeColour :: String -> Either String String
validEyeColour xs = 
  if xs `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    then Right xs
    else Left "Invalid Ecl"

valididatePid :: String -> Either String String
valididatePid xs = 
  if isNumOfSize 9 xs
    then Right xs
    else Left "Invalid Pid"

validatePassport :: Either String Passport -> Either String Passport
validatePassport (Left err) = Left err
validatePassport (Right ps) = do
  _ <- validateNumInRange (1920,2002) "BYR" (byr ps)
  _ <- validateNumInRange (2010,2020) "IYR" (iyr ps)
  _ <- validateNumInRange (2020,2030) "EYR" (eyr ps)
  _ <- validateHeight (hgt ps)
  _ <- validateHairColour (hcl ps)
  _ <- validEyeColour (ecl ps)
  _ <- valididatePid (pid ps)
  Right ps

day4pt2 :: FilePath -> IO Int
day4pt2 fp = do
  ps <- parsePassportFile fp 
  let vs = map validatePassport ps
  pure $ length $ rights vs
