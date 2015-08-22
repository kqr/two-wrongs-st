{-# LANGUAGE OverloadedStrings #-}

module Html (generatePage) where

import Control.Monad.Trans.Either (EitherT, left)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Heist
import Heist.Interpreted (renderWithArgs)
import Lens.Family

import Types


heistConfig :: HeistConfig (EitherT [String] IO)
heistConfig =
    emptyHeistConfig
        & hcTemplateLocations .~ [loadTemplates "templates"]
        & hcInterpretedSplices .~ defaultInterpretedSplices
        & hcNamespace .~ ""


generatePage :: ByteString -> Splices Text -> EitherT [String] IO Text
generatePage template splices = do
    heist <- initHeist heistConfig
    renderResult <- renderWithArgs splices heist template
    (output, _) <- case renderResult of
        Nothing -> left ["invalid template '" <> unpack template <> "'"]
        Just x -> return x
    return (decodeUtf8 (toStrict (toLazyByteString output)))


