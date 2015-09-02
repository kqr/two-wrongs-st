{-# LANGUAGE OverloadedStrings #-}

module Parsing (parseAll) where

import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.Text
import Data.Either (partitionEithers)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day, fromGregorianValid)
import System.FilePath.Posix (takeBaseName, splitDirectories, takeDirectory)
import Text.XmlHtml (parseHTML)

import Types


parseAll :: [File] -> Either [String] [Post]
parseAll files =
    case partitionEithers (map parsePost files) of
        ([], posts) -> Right posts
        (errors, _) -> Left errors


parsePost :: File -> Either String Post
parsePost (filepath, filetext) =
    either (Left . annotate filepath) return $ do
        (datestamp, baseSlug) <- parseFilename (pack (takeBaseName filepath))
        let moreSlugBits = map pack (drop 1 (splitDirectories (takeDirectory filepath)))
        parsedBits <- mapM (parseOnly (slugParser <* endOfInput)) moreSlugBits
        let slug = mconcat (intersperse (Slug "-") parsedBits) <> Slug "-" <> baseSlug
        (title, tags, content) <- parseContent filetext
        document <- parseHTML filepath (encodeUtf8 content)
        return Post
            { title = title
            , slug = slug
            , datestamp = datestamp
            , content = document
            , author = ()
            , tags = tags
            }


annotate :: FilePath -> String -> String
annotate filepath s =
    "Invalid post '" <> filepath <> "': " <> s


parseContent :: Text -> Either String (Text, [Slug], Text)
parseContent =
    parseOnly $ do
        title <- takeTill isEndOfLine    -- entire first line is title
        endOfLine                        -- skip past a newline
        tags <- tagParser <|> pure []    -- read in a list of tags
        endOfLine                        -- blank line before content starts
        body <- takeText                 -- the rest of the file is body text
        return (title, tags, body)


tagParser :: Parser [Slug]
tagParser = do
    string "Tags: "
    tags <- sepBy slugParser space
    endOfLine
    return tags


parseFilename :: Text -> Either String (Day, Slug)
parseFilename =
    parseOnly $ do
        day <- dateParser              -- first comes date
        char '-'                       -- followed by a dash
        slug <- slugParser             -- then the slug
        endOfInput <?> "invalid slug"  -- if anything is unparsed, slug was invalid
        return (day, slug)


-- parser that consumes something like "2015-08-21" and turns it into a
-- Data.Time.Calendar.Day
dateParser :: Parser Day
dateParser = do
    year <- decimal <?> "failed parsing year"
    char '-'
    month <- decimal <?> "failed parsing month"
    char '-'
    day <- decimal <?> "failed parsing day"
    maybe (fail "invalid date") return (fromGregorianValid year month day)


slugParser :: Parser Slug
slugParser =
    fmap Slug (takeWhile1 (inClass "a-z0-9-") <?> "slug can not be empty!")

