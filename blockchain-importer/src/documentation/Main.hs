{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | This program builds Swagger specification for BlockchainImporter web API and converts it to JSON.
-- We run this program during CI build.
-- Produced JSON will be used to create online
-- version of wallet web API description at cardanodocs.com website
-- (please see 'update_blockchainImporter_web_api_docs.sh' for technical details).

module Main
    ( main
    ) where

import           Universum

import           Control.Lens (mapped, (?~))
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Fixed (Fixed (..), Micro)
import           Data.Swagger (NamedSchema (..), Swagger, ToParamSchema (..), ToSchema (..),
                               declareNamedSchema, defaultSchemaOptions, description,
                               genericDeclareNamedSchema, host, info, name, title, version)
import           Data.Typeable (Typeable, typeRep)
import           Data.Version (showVersion)
import           Options.Applicative (execParser, footer, fullDesc, header, help, helper,
                                      infoOption, long, progDesc)
import qualified Options.Applicative as Opt
import           Servant ((:>))
import           Servant.Multipart (MultipartForm)
import           Servant.Swagger (HasSwagger (toSwagger))

import qualified Paths_cardano_sl_blockchain_importer as CSLE
import qualified Pos.BlockchainImporter.Web.Api as A
import qualified Pos.BlockchainImporter.Web.ClientTypes as C
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError)


main :: IO ()
main = do
    showProgramInfoIfRequired jsonFile
    BSL8.writeFile jsonFile $ encode swaggerSpecForBlockchainImporterApi
    putStrLn $ "Done. See " <> jsonFile <> "."
  where
    jsonFile = "blockchain-importer-web-api-swagger.json"

    -- | Showing info for the program.
    showProgramInfoIfRequired :: FilePath -> IO ()
    showProgramInfoIfRequired generatedJSON = void $ execParser programInfo
      where
        programInfo = Opt.info (helper <*> versionOption) $
            fullDesc <> progDesc "Generate Swagger specification for BlockchainImporter web API."
                     <> header   "Cardano SL Blockchain Importer web API docs generator."
                     <> footer   ("This program runs during 'cardano-sl' building on CI. " <>
                                  "Generated file '" <> generatedJSON <> "' will be used to produce HTML documentation. " <>
                                  "This documentation will be published at cardanodocs.com using 'update-blockchain-importer-web-api-docs.sh'.")

        versionOption = infoOption
            ("cardano-swagger-" <> showVersion CSLE.version)
            (long "version" <> help "Show version.")

instance HasSwagger api => HasSwagger (MultipartForm a :> api) where
    toSwagger Proxy = toSwagger $ Proxy @api

-- | Instances we need to build Swagger-specification for 'blockchainImporterApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.
instance ToParamSchema C.EpochIndex
instance ToSchema      C.Byte
instance ToSchema      BlockchainImporterError
instance ToSchema      C.CEncodedSTx where
  declareNamedSchema _ = pure $ NamedSchema (Just "EncodedSTx") mempty

deriving instance Generic Micro

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-} (Typeable a, ToSchema a) => ToSchema (Either BlockchainImporterError a) where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped . name ?~ show (typeRep (Proxy @(Either BlockchainImporterError a)))

-- | Build Swagger-specification from 'blockchainImporterApi'.
swaggerSpecForBlockchainImporterApi :: Swagger
swaggerSpecForBlockchainImporterApi = toSwagger A.blockchainImporterApi
    & info . title       .~ "Cardano SL BlockchainImporter Web API"
    & info . version     .~ toText (showVersion CSLE.version)
    & info . description ?~ "This is an API for Cardano SL BlockchainImporter."
    & host               ?~ "cardano-blockchain-importer.com"
