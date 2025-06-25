module Validation where

import Data.ByteString.Lazy (ByteString)
import Types (Schema)

validateJson :: ByteString -> ByteString -> Either String ()
validateJson _ _ = Left "Funcionalidade de validação ainda não implementada"