{-# LANGUAGE OverloadedStrings #-}

module Views (generateBlog) where

import Heist ((##))
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Text hiding (length)

import Types
import Html (makeView)



generateBlog :: Blog -> IO (Either [String] [File])
generateBlog blog =
    runEitherT $ do
        index <- indexView blog
        about <- indexView blog
        posts <- mapM postView (published blog)
        return (index : about : posts)


indexView :: Blog -> EitherT [String] IO File
indexView blog =
    makeView "index" (Slug "index") $ do
        "pageTitle" ## "Index"


aboutView :: Blog -> EitherT [String] IO File
aboutView blog =
    makeView "about" (Slug "about") $ do
        "pageTitle" ## "About"
        "count" ## pack (show (length (published blog)))


postView :: Post -> EitherT [String] IO File
postView post =
    makeView "detail" (slug post) $ do
        "pageTitle" ## title post
        "content" ## content post
        "datestamp" ## pack (show (datestamp post))


