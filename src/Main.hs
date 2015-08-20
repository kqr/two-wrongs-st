{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (take)
import Control.Applicative (liftA2)
import Data.Attoparsec.Text (Parser, parseOnly, string, char, endOfInput, takeTill, decimal, (<?>))
import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorianValid)
import System.Directory (getDirectoryContents)


-- Wrapper for slugs so we don't mix 'em up with regular text
data Slug = Slug Text deriving Show

-- This should ensure the text is actually a sluggish value
toSlug :: Text -> Either String Slug
toSlug = return . Slug


data Post = Post
    { title :: Text
    , slug :: Slug
    , timestamp :: Day
    , content :: ()        -- probably some sort of Html-y type to begin with
    , author :: ()         -- for a more civilized age
    , tags :: [()]         -- also for a more civilized age
    }


main = do
    putStrLn "stub"
-- code for a more civilized age:
--    drafts <- getPosts =<< getDirectoryContents "drafts"
--    published <- getPosts =<< getDirectoryContents "published"
-- getPosts :: [FilePath] -> IO [Post]
-- readPost :: FilePath -> IO Post


-- parses a filename in the form of "2015-08-21-this-is-a-slug.txt"
-- into a tuple (2015-04-21,Slug "this-is-a-slug")
parseFilename :: Text -> Either String (Day, Slug)
parseFilename =
    parseOnly (liftA2 (,) dateParser slugParser <* string ".txt" <* endOfInput)


-- parser that consumes something like "2015-08-21" and turns it into a
-- Data.Time.Calendar.Day
dateParser :: Parser Day
dateParser = do
    year <- decimal <?> "failed parsing year"
    char '-'
    month <- decimal <?> "failed parsing month"
    char '-'
    day <- decimal <?> "failed parsing day"
    char '-'
    maybe (fail "invalid date") return (fromGregorianValid year month day)


-- parser that consumes any text up to a period (should probably be something
-- else going forward...) and turns it into a Slug
slugParser :: Parser Slug
slugParser = do
    slug <- fmap toSlug (takeTill (== '.'))
    either fail return slug


