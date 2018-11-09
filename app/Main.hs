{-# LANGUAGE OverloadedStrings #-}

import           BasicPrelude
import qualified Data.Aeson.Encode.Pretty      as Aeson
                                                ( encodePretty )
import qualified Data.ByteString.Lazy          as B

import           FileUtil                       ( (//) )
import qualified NRSParser                     as NRS


main :: IO ()
main = do
    nrsModel <- NRS.parseFiles $ (//) "/tmp/www.leg.state.nv.us/NRS"
    B.putStr $ Aeson.encodePretty nrsModel
