{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Expressão de filtro com caminho e valor esperado
data FilterExpr = FilterExpr
  { path  :: [Text]  -- Segmentos do caminho (ex: ["user", "address", "city"])
  , value :: Value   -- Valor esperado para comparação
  } deriving (Show, Eq, Generic)

-- | Tipos de dados suportados na validação
data SchemaType
  = StringType
  | NumberType
  | BooleanType
  | ObjectType Schema
  | ArrayType Schema
  deriving (Show, Eq, Generic)

-- | Propriedade de um campo no esquema
data SchemaProperty = SchemaProperty
  { propType     :: SchemaType  -- Tipo esperado
  , propRequired :: Bool        -- Se o campo é obrigatório
  } deriving (Show, Eq, Generic)

-- | Esquema JSON completo
data Schema = Schema
  { schemaType       :: SchemaType  -- Tipo raiz do documento
  , schemaProperties :: [(Text, SchemaProperty)]  -- Definições dos campos
  , requiredFields   :: [Text]      -- Campos obrigatórios globalmente
  } deriving (Show, Eq, Generic)

-- Instâncias para serialização/desserialização JSON
instance FromJSON FilterExpr
instance ToJSON FilterExpr
instance FromJSON SchemaType
instance ToJSON SchemaType
instance FromJSON SchemaProperty
instance ToJSON SchemaProperty
instance FromJSON Schema
instance ToJSON Schema