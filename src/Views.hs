{-# LANGUAGE OverloadedStrings #-}

module Views (generateBlog) where

import Control.Monad (unless)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as B (toStrict)
import Data.Monoid ((<>))
import Data.Text hiding (length, take, head, concatMap, filter, null)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as T (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.Clock (UTCTime(UTCTime))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Heist ((##))
import Heist.Interpreted (Splice, textSplice, mapSplices, runChildrenWith, runChildrenWithText, runNodeList)
import qualified HTMLEntities.Builder as Escape (text)
import Text.XmlHtml (docContent, renderXmlFragment, Encoding(UTF8))
import qualified Data.List as List

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
        taggeds <- mapM (taggedView blog) (concatMap tags (published blog))
        return (index : about : atom : taggeds ++ posts)


indexView :: Blog -> EitherT [String] IO File
indexView blog =
    makeView blog "index" (Slug "index") $ do
        "pageTitle" ## textSplice "Index"
        "popularTags" ## popularTags blog
        "latestPosts" ## latestPosts blog (const True)


aboutView :: Blog -> EitherT [String] IO File
aboutView blog =
    makeView blog "about" (Slug "about") $ do
        "pageTitle" ## textSplice "About"
        "count" ## textSplice (pack (show (length (published blog))))


taggedView :: Blog -> Slug Text -> EitherT [String] IO File
taggedView blog tag =
    makeView blog "tagged" (Slug "tagged_" <> tag) $ do
        "pageTitle" ## textSplice ("Tagged with " <> fromSlug tag)
        "taggedPosts" ## latestPosts blog (\post -> elem tag (tags post))


postView :: Blog -> Post -> EitherT [String] IO File
postView blog post =
    makeView blog "detail" (slug post) $ do
        "pageTitle" ## textSplice (title post)
        "content" ## runNodeList (docContent (content post))
        "datestamp" ## textSplice (pack (show (datestamp post)))
        "timestamp" ## textSplice (dayToTimestamp (datestamp post))
        "isTagged" ##
            if null (tags post) then
                return mempty
            else do
                runChildrenWith $ do
                    "tags" ## flip mapSplices (tags post) $ \tag ->
                        runChildrenWithText $ do
                            "tagName" ## fromSlug tag
                            "tagURL" ## "tagged_" <> fromSlug tag


popularTags :: Monad n => Blog -> Splice n
popularTags blog =
    let
        allTags = concatMap tags (published blog)
        groupedTags = List.group (List.sort allTags)
        byPopularity = List.sortOn (negate . List.length) groupedTags
        flattened = List.map head byPopularity
        limited = take 8 flattened
    in
        flip mapSplices limited $ \tag ->
            runChildrenWithText $ do
                "tagName" ## fromSlug tag
                "tagURL" ## "tagged_" <> fromSlug tag


latestPosts :: Monad n => Blog -> (Post -> Bool) -> Splice n
latestPosts blog include =
    flip mapSplices (filter include (published blog)) $ \post ->
        runChildrenWithText $ do
            "entryTitle" ## title post
            "datestamp" ## pack (show (datestamp post))
            "entrySlug" ## fromSlug (slug post)


atomView :: Blog -> EitherT [String] IO File
atomView blog =
    makeView blog "atom" (Slug "atom") $ do
        "lastEntry" ## textSplice (dayToTimestamp (case published blog of [] -> ModifiedJulianDay 0; (x:_) -> datestamp x))
        "latestPosts" ## flip mapSplices (take 20 (published blog)) $ \post ->
            runChildrenWithText $ do
                "entryTitle" ## T.toStrict (toLazyText (Escape.text (title post)))
                "entrySlug" ## fromSlug (slug post)
                "timestamp" ## dayToTimestamp (datestamp post)
                "escapedContent" ## decodeUtf8 (B.toStrict (toLazyByteString (renderXmlFragment UTF8 (docContent (content post)))))


