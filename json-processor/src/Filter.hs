module Filter where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Types (FilterExpr)

filterJson :: String -> ByteString -> Either String ByteString
filterJson _ _ = Left "Funcionalidade de filtro ainda não implementada"