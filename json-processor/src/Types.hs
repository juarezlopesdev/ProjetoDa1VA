{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)

-- Tipo para representar o estado da aplicação
data AppState = AppState
  { lastOperation :: String
  , timestamp     :: String
  , output        :: Maybe Value
  } deriving (Show, Generic)

instance ToJSON AppState
instance FromJSON AppState