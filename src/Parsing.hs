{-# LANGUAGE OverloadedStrings #-}

module Parsing (parseAll) where

import Control.Applicative (liftA2)
import Data.Attoparsec.Text
import Data.Either (partitionEithers)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time.Calendar (Day, fromGregorianValid)
import System.FilePath.Posix (takeBaseName)

import Types


parseAll :: [File] -> Either [String] [Post]
parseAll files =
    case partitionEithers (map parsePost files) of
        ([], posts) -> Right posts
        (errors, _) -> Left errors


parsePost :: File -> Either String Post
parsePost (filepath, filetext) =
    either (Left . annotate filepath) return $ do
        (datestamp, slug) <- parseFilename (pack (takeBaseName filepath))
        (title, content) <- parseContent filetext
        return Post
            { title = title
            , slug = slug
            , datestamp = datestamp
            , content = content
            , author = ()
            , tags = []
            }


annotate :: FilePath -> String -> String
annotate filepath s =
    "Invalid post '" <> filepath <> "': " <> s


-- Parses a multi-line string where the first line is assumed to be a title
-- and then comes an empty line and then everything else is content
parseContent :: Text -> Either String (Text, Text)
parseContent =
    parseOnly (liftA2 (,) (takeTill isEndOfLine <* endOfLine) takeText)


-- parses a filename in the form of "2015-08-21-this-is-a-slug"
-- into a tuple (2015-04-21,Slug "this-is-a-slug")
parseFilename :: Text -> Either String (Day, Slug)
parseFilename =
    parseOnly (liftA2 (,) dateParser slugParser <* endOfInput <?> "invalid slug")


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


slugParser :: Parser Slug
slugParser =
    fmap Slug (takeWhile1 (inClass "a-z0-9-") <?> "post must have a slug!")

