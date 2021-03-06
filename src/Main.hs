{-# LANGUAGE OverloadedStrings #-}

module Main where

-- The Turtle FilePath is deprecated, but unfortunately necessary in this
-- module where a bunch of I/O happens through Turtle. Only in this module
-- should the standard FilePath be hidden!
import Prelude hiding (FilePath)
import Control.Arrow (first)
import Control.Foldl (list)
import Data.Ini (readIniFile, Ini)
import Data.Text (pack)
import Data.Time.Clock (utctDay, getCurrentTime)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import System.Environment (getArgs)
import Turtle

import Types
import Parsing (parseAll)
import Views (generateBlog)


-- Freak out and crash if there's anything wrong, otherwise
-- just go on and do whatever's next. There should probably be
-- better error handling but meh...
freakout :: Either [String] a -> a
freakout (Left es) = error (unlines es)
freakout (Right a) = a


main = do
    echo "Reading config file... "
    config <- either error return =<< readIniFile "config.ini"

    echo "Reading and parsing blog post files..."
    blog <- getBlog config
    echo "Generating HTML for blog posts..."
    files <- fmap freakout (generateBlog blog)

    echo "Writing out HTML to permanent storage..."
    newExists <- testdir "site_new"
    when newExists (rmtree "site_new")
    mkdir "site_new"
    mapM_ (writeFileTo "site_new") files

    echo "Copying over static content"
    proc "cp" ["-r", "static", "site_new/."] empty

    oldExists <- testdir "site"
    when oldExists (echo "Overwriting old site..." >> rmtree "site")
    mv "site_new" "site"


getBlog :: Ini -> IO Blog
getBlog config = do
    drafts <- fmap freakout (getPostsFrom "drafts")
    published <- fmap freakout (getPostsFrom "published")
    today <- fmap utctDay getCurrentTime
    return (freakout (toBlog config today drafts published))


getPostsFrom :: FilePath -> IO (Either [String] [Post])
getPostsFrom directory = do
    files <- fold (getFilesIn directory) list
    return (parseAll (map (first encodeString) files))


getFilesIn :: FilePath -> Shell (FilePath, Text)
getFilesIn directory =
    let
        directoryName = pack (encodeString directory)
        pathPrefix = text directoryName <> "/"
        fileName = plus (noneOf ".") <> ".txt"
    in do
        filepaths <- find (pathPrefix <> fileName) directory
        filetext <- strict (input filepaths)
        return (filepaths, filetext)


writeFileTo :: FilePath -> File -> IO ()
writeFileTo directory (filename, content) =
    let
        extension = if filename == "atom" then "xml" else "html"
        filepath = directory </> decodeString filename <.> extension
    in
        output filepath (return content)
    
