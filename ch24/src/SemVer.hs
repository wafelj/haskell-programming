module SemVer where

import Text.Trifecta
import Control.Applicative

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare (SemVer x1 y1 z1 _ _) (SemVer x2 y2 z2 _ _)
    | x1 < x2 = LT
    | x1 > x2 = GT
    | y1 < y2 = LT
    | y1 > y2 = GT
    | z1 < z2 = LT
    | z1 > z2 = GT
    | otherwise = EQ

-- TODO: don't allow leading zeros
parseSemVer :: Parser SemVer 
parseSemVer = do
  major <- natural
  dot
  minor <- natural
  dot
  patch <- natural
  release <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch release metadata

parseRelease :: Parser Release
parseRelease = do
  symbolic '-'
  ids <- sepBy1 parseNos dot
  return ids

parseMetadata :: Parser Metadata
parseMetadata = do
  symbolic '+'
  ids <- sepBy1 parseNos dot
  return ids

parseNos :: Parser NumberOrString
parseNos =
      (NOSI <$> integer)
  <|> (NOSS <$> some letter)

parseSemVerFromString = parseString parseSemVer mempty 
