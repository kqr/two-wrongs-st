{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Ini
import Data.List (group, sort, sortOn)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (Down(Down))
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day)
import Text.XmlHtml (Document)



type File = (FilePath, Text)


data Blog = Blog
    { name :: Text
    , url :: Text
    , unpublished :: [Post]
    , published :: [Post]
    }
    deriving Show

-- toBlog does three really important things:
-- 
--     1. It ensures there are now slug collisions,
--     2. It moves the "published" posts with a future publish date
--        into the "unpublished" list, and
--     3. It sorts the list of published entries on datestamp (in
--        reverse chronological order!)
--
toBlog :: Ini -> Day -> [Post] -> [Post] -> Either [String] Blog
toBlog config today drafts published =
    let
        getDuplicates = mapMaybe (listToMaybe . drop 1) . group . sort
        duplicates = getDuplicates (map slug (drafts ++ published))
        unpublished = filter ((> today) . datestamp) published ++ drafts
        actualPublished = sortOn (Down . datestamp) (filter ((<= today) . datestamp) published)
        coerce = either (Left . (:[])) return
    in do
        blogName <- coerce (lookupValue "general" "title" config)
        blogUrl <- coerce (lookupValue "general" "url" config)

        if null duplicates then
            Right (Blog blogName blogUrl unpublished actualPublished)
        else
            Left (map (("duplicate slug: " ++) . unpack . fromSlug) duplicates)


data Post = Post
    { title :: Text
    , slug :: Slug Text
    , datestamp :: Day
    , content :: Document
    , author :: ()         -- for a more civilized age
    , tags :: [Slug Text]
    }
    deriving Show


-- Wrapper for slugs so we don't mix 'em up with regular text
data Slug a = Slug
    { fromSlug :: a
    }
    deriving (Show, Eq, Ord)

instance Monoid a => Monoid (Slug a) where
    mempty = Slug mempty
    mappend (Slug a) (Slug b) = Slug (mappend a b)

instance Functor Slug where
    fmap f (Slug a) = Slug (f a)

