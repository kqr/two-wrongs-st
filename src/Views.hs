{-# LANGUAGE OverloadedStrings #-}

module Views (generateBlog) where

import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text hiding (length, take, head)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(UTCTime))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Heist ((##))
import Heist.Interpreted (Splice, textSplice, mapSplices, runChildrenWithText, runNodeList)
import Text.XmlHtml (docContent, renderHtmlFragment, Encoding(UTF8))

import Types
import Html (makeView)


dayToTimestamp :: Day -> Text
dayToTimestamp day =
    pack (formatTime defaultTimeLocale "%FT%TZ" (UTCTime day 0))


generateBlog :: Blog -> IO (Either [String] [File])
generateBlog blog =
    runEitherT $ do
        index <- indexView blog
        about <- aboutView blog
        atom <- atomView blog
        posts <- mapM (postView blog) (published blog <> unpublished blog)
        return (index : about : atom : posts)


indexView :: Blog -> EitherT [String] IO File
indexView blog =
    makeView blog "index" (Slug "index") $ do
        "pageTitle" ## textSplice "Index"
        "latestPosts" ## latestPosts blog


aboutView :: Blog -> EitherT [String] IO File
aboutView blog =
    makeView blog "about" (Slug "about") $ do
        "pageTitle" ## textSplice "About"
        "count" ## textSplice (pack (show (length (published blog))))


postView :: Blog -> Post -> EitherT [String] IO File
postView blog post =
    makeView blog "detail" (slug post) $ do
        "pageTitle" ## textSplice (title post)
        "content" ## runNodeList (docContent (content post))
        "datestamp" ## textSplice (pack (show (datestamp post)))
        "timestamp" ## textSplice (dayToTimestamp (datestamp post))


latestPosts :: Monad n => Blog -> Splice n
latestPosts blog =
    flip mapSplices (published blog) $ \post ->
        runChildrenWithText $ do
            "entryTitle" ## title post
            "datestamp" ## pack (show (datestamp post))
            "entrySlug" ## fromSlug (slug post)


atomView :: Blog -> EitherT [String] IO File
atomView blog =
    makeView blog "atom" (Slug "atom") $ do
        "lastEntry" ## textSplice (dayToTimestamp (datestamp (head (published blog))))
        "latestPosts" ## flip mapSplices (take 20 (published blog)) $ \post ->
            runChildrenWithText $ do
                "entryTitle" ## title post
                "entrySlug" ## fromSlug (slug post)
                "timestamp" ## dayToTimestamp (datestamp post)
                "escapedContent" ## decodeUtf8 (toStrict (toLazyByteString (renderHtmlFragment UTF8 (docContent (content post)))))


