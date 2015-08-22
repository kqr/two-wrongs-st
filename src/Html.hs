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
import Heist.Interpreted (renderWithArgs)
import Lens.Family

import Types



makeView template slug context = do
    content <- generatePage template context
    return (T.unpack (fromSlug slug), content)


generatePage :: ByteString -> Splices Text -> EitherT [String] IO Text
generatePage template splices = do
    heist <- initHeist heistConfig
    renderResult <- renderWithArgs splices heist template
    (output, _) <- case renderResult of
        Nothing -> left ["invalid template '" <> BS.unpack template <> "'"]
        Just x -> return x
    return (decodeUtf8 (toStrict (toLazyByteString output)))


heistConfig :: HeistConfig (EitherT [String] IO)
heistConfig =
    emptyHeistConfig
        & hcTemplateLocations .~ [loadTemplates "templates"]
        & hcInterpretedSplices .~ defaultInterpretedSplices
        & hcNamespace .~ ""

