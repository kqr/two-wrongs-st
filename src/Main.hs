{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile)
import Data.Either (partitionEithers)
import Control.Monad (filterM)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified System.Directory (getDirectoryContents)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))

import Types
import Parsing (parsePost)


main = do
    blog <- getBlog
    print blog


getBlog :: IO Blog
getBlog = do
    drafts <- getPostsFrom "drafts"
    published <- getPostsFrom "published"
    return (Blog drafts published)


getPostsFrom :: FilePath -> IO [Post]
getPostsFrom directory = do
    files <- mapM readPost =<< getFilesIn directory
    let (errors, posts) = partitionEithers (map (uncurry parsePost) files)
    mapM error errors
    return posts


getFilesIn :: FilePath -> IO [FilePath]
getFilesIn directory = do
    filepaths <- getDirectoryContents directory
    filterM doesFileExist filepaths


getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents directory = do
    filenames <- System.Directory.getDirectoryContents directory
    return (map (directory </>) filenames)


readPost :: FilePath -> IO (FilePath, Text)
readPost filepath = do
    filetext <- readFile filepath
    return (filepath, filetext)



