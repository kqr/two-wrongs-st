{-# LANGUAGE OverloadedStrings #-}

module Views (generateBlog) where

import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Monoid ((<>))
import Data.Text hiding (length)
import Heist ((##))
import Heist.Interpreted (textSplice, mapSplices, runChildrenWithText, runNodeList)
import Text.XmlHtml (docContent)

import Types
import Html (makeView)



generateBlog :: Blog -> IO (Either [String] [File])
generateBlog blog =
    runEitherT $ do
        index <- indexView blog
        about <- aboutView blog
        posts <- mapM postView (published blog <> unpublished blog)
        return (index : about : posts)


indexView :: Blog -> EitherT [String] IO File
indexView blog =
    makeView "index" (Slug "index") $ do
        "pageTitle" ## textSplice "Index"
        "latestPosts" ## latestPosts blog


aboutView :: Blog -> EitherT [String] IO File
aboutView blog =
    makeView "about" (Slug "about") $ do
        "pageTitle" ## textSplice "About"
        "count" ## textSplice (pack (show (length (published blog))))


postView :: Post -> EitherT [String] IO File
postView post =
    makeView "detail" (slug post) $ do
        "pageTitle" ## textSplice (title post)
        "content" ## runNodeList (docContent (content post))
        "datestamp" ## textSplice (pack (show (datestamp post)))


latestPosts blog =
    flip mapSplices (published blog) $ \post ->
        runChildrenWithText $ do
            "entryTitle" ## title post
            "datestamp" ## pack (show (datestamp post))
            "entryURL" ## fromSlug (slug post)

