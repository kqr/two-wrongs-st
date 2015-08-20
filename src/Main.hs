{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Control.Applicative (liftA2)
import Data.Attoparsec.Text
import Data.Either (partitionEithers)
import Data.Monoid ((<>))
import Control.Monad (filterM, (<=<))
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import Data.Time.Calendar (Day, fromGregorianValid)
import System.Directory (getDirectoryContents, doesFileExist)


-- Wrapper for slugs so we don't mix 'em up with regular text
data Slug = Slug Text deriving Show


data Blog = Blog
    { drafts :: [Post]
    , published :: [Post]
    }


data Post = Post
    { title :: Text
    , slug :: Slug
    , datestamp :: Day
    , content :: Text
    , author :: ()         -- for a more civilized age
    , tags :: [()]         -- also for a more civilized age
    }
    deriving Show


main = do
    putStrLn "stub"


getPosts :: FilePath -> IO [Post]
getPosts directory = do
    files <- mapM readPost =<< getFilesIn directory
    let (errors, posts) = partitionEithers (map (uncurry parsePost) files)
    mapM error errors
    return posts


getFilesIn :: FilePath -> IO [FilePath]
getFilesIn directory = do
    -- bajskorv funkar inte med relativa paths och kataloger och sånt >_<
    inodes <- getDirectoryContents directory
    let fullpaths = map (\inode -> directory ++ "/" ++ inode) inodes
    filterM doesFileExist fullpaths



readPost :: FilePath -> IO (FilePath, Text)
readPost filename = do
    filetext <- readFile filename
    return (filename, filetext)




annotate :: FilePath -> String -> String
annotate filename s =
    "Invalid post '" <> filename <> "': " <> s


parsePost :: FilePath -> Text -> Either String Post
parsePost filename filetext =
    either (Left . annotate filename) return $ do
        (datestamp, slug) <- parseFilename (pack filename)
        (title, content) <- parseContent filetext
        return Post
            { title = title
            , slug = slug
            , datestamp = datestamp
            , content = content
            , author = ()
            , tags = []
            }


parseContent :: Text -> Either String (Text, Text)
parseContent =
    parseOnly (liftA2 (,) (takeTill isEndOfLine <* endOfLine) takeText)


-- parses a filename in the form of "2015-08-21-this-is-a-slug.txt"
-- into a tuple (2015-04-21,Slug "this-is-a-slug")
parseFilename :: Text -> Either String (Day, Slug)
parseFilename =
    parseOnly (liftA2 (,) dateParser slugParser)


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

