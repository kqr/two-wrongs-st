{-# LANGUAGE OverloadedStrings #-}

module Html (makeView) where

import Control.Monad.Trans.Either (EitherT, left)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import Heist
import Heist.Interpreted (renderTemplate, bindSplices, Splice, textSplice)
import Lens.Family

import Types



type N = EitherT [String] IO


makeView :: Blog -> ByteString -> Slug Text -> Splices (Splice N) -> N File
makeView blog template slug context =
    let
        slices = extraContext <> context
        extraContext = do
            "blogName" ## textSplice (name blog)
            "blogURL" ## textSplice (url blog)
    in do
        content <- generatePage template slices
        return (T.unpack (fromSlug slug), content)


generatePage :: ByteString -> Splices (Splice N) -> N Text
generatePage template splices = do
    heist <- initHeist heistConfig
    renderResult <- renderTemplate (bindSplices splices heist) template
    (output, _) <- case renderResult of
        Nothing -> left ["invalid template '" <> BS.unpack template <> "'"]
        Just x -> return x
    return (decodeUtf8 (toStrict (toLazyByteString output)))


heistConfig :: HeistConfig N
heistConfig =
    emptyHeistConfig
        & hcTemplateLocations .~ [loadTemplates "templates"]
        & hcInterpretedSplices .~ defaultInterpretedSplices
        & hcNamespace .~ ""

