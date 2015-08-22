module Types where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (group, sort)
import Data.Text (Text, unpack)
import Data.Time.Calendar (Day)
import Text.XmlHtml (Document)



type File = (FilePath, Text)


data Blog = Blog
    { unpublished :: [Post]
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
toBlog :: Day -> [Post] -> [Post] -> Either [String] Blog
toBlog today drafts published =
    let
        getDuplicates = mapMaybe (listToMaybe . drop 1) . group . sort
        duplicates = getDuplicates (map slug (drafts ++ published))
        unpublished = filter ((> today) . datestamp) published ++ drafts
        actualPublished = filter ((<= today) . datestamp) published
    in
        if null duplicates then
            Right (Blog unpublished actualPublished)
        else
            Left (map (("duplicate slug: " ++) . unpack . fromSlug) duplicates)


data Post = Post
    { title :: Text
    , slug :: Slug
    , datestamp :: Day
    , content :: Document
    , author :: ()         -- for a more civilized age
    , tags :: [()]         -- also for a more civilized age
    }
    deriving Show


-- Wrapper for slugs so we don't mix 'em up with regular text
data Slug = Slug
    { fromSlug :: Text
    }
    deriving (Show, Eq, Ord)

