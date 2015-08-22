{-# LANGUAGE OverloadedStrings #-}

module Views (generateBlog) where

import Heist ((##))
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.Text hiding (length)
import qualified Data.Text as T (unpack)

import Types
import Html (generatePage)



generateBlog :: Blog -> IO (Either [String] [File])
generateBlog blog =
    runEitherT $ do
        index <- generateIndex blog
        about <- generateAbout blog
        posts <- mapM generatePost (published blog)
        return (index : about : posts)


generateIndex :: Blog -> EitherT [String] IO File
generateIndex blog = do
    content <- generatePage "index" $ do
        "pageTitle" ## "Index"
    return ("index", content)


generateAbout :: Blog -> EitherT [String] IO File
generateAbout blog = do
    content <- generatePage "about" $ do
        "pageTitle" ## "About"
        "count" ## pack (show (length (published blog)))
    return ("about", content)


generatePost :: Post -> EitherT [String] IO File
generatePost post = do
    content <- generatePage "detail" $ do
        "pageTitle" ## title post
        "content" ## content post
        "datestamp" ## pack (show (datestamp post))
    return (T.unpack (fromSlug (slug post)), content)


