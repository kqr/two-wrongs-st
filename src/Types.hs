module Types where

import Data.Text (Text)
import Data.Time.Calendar (Day)


-- Wrapper for slugs so we don't mix 'em up with regular text
data Slug = Slug Text deriving Show


data Blog = Blog
    { drafts :: [Post]
    , published :: [Post]
    }
    deriving Show


data Post = Post
    { title :: Text
    , slug :: Slug
    , datestamp :: Day
    , content :: Text
    , author :: ()         -- for a more civilized age
    , tags :: [()]         -- also for a more civilized age
    }
    deriving Show


