module Validation (validateJson) where

import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key, fromText)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import Control.Monad (foldM, unless)

validateJson :: ByteString -> ByteString -> Either String ()
validateJson schemaData jsonData = do
  schema <- eitherDecode schemaData
  json   <- eitherDecode jsonData
  validateAgainstSchema schema json

validateAgainstSchema :: Value -> Value -> Either String ()
validateAgainstSchema (Object schema) (Object obj) = do
  checkRequiredFields schema obj
  foldM (validateField schema) () (KM.toList obj)
validateAgainstSchema _ _ = Left "Esquema e JSON devem ser objetos"

checkRequiredFields :: KeyMap Value -> KeyMap Value -> Either String ()
checkRequiredFields schema obj = 
  case KM.lookup "required" schema of
    Just (Array fields) -> 
      mapM_ (checkFieldExists obj) (V.toList fields)
    _ -> return ()
  where
    checkFieldExists :: KeyMap Value -> Value -> Either String ()
    checkFieldExists obj (String field) =
      unless (KM.member (fromText field) obj) $ 
        Left $ "Campo obrigatório faltando: " ++ unpack field
    checkFieldExists _ _ = return ()

validateField :: KeyMap Value -> () -> (Key, Value) -> Either String ()
validateField schema _ (key, value) = 
  case KM.lookup "properties" schema >>= getFieldSchema key of
    Just fieldSchema -> validateValue fieldSchema value
    Nothing -> return ()

getFieldSchema :: Key -> Value -> Maybe Value
getFieldSchema key (Object props) = KM.lookup key props
getFieldSchema _ _ = Nothing

validateValue :: Value -> Value -> Either String ()
validateValue schema value = 
  case schema of
    Object s -> 
      case KM.lookup "type" s of
        Just (String t) 
          | t == "string"  -> validateString value
          | t == "number"  -> validateNumber value
          | t == "boolean" -> validateBoolean value
          | t == "object"  -> validateObject s value
          | t == "array"   -> validateArray s value
        _ -> return ()
    _ -> return ()

validateString :: Value -> Either String ()
validateString (String _) = return ()
validateString _ = Left "Esperado tipo string"

validateNumber :: Value -> Either String ()
validateNumber (Number _) = return ()
validateNumber _ = Left "Esperado tipo number"

validateBoolean :: Value -> Either String ()
validateBoolean (Bool _) = return ()
validateBoolean _ = Left "Esperado tipo boolean"

validateObject :: KeyMap Value -> Value -> Either String ()
validateObject schema (Object obj) = 
  validateAgainstSchema (Object schema) (Object obj)
validateObject _ _ = Left "Esperado tipo object"

validateArray :: KeyMap Value -> Value -> Either String ()
validateArray schema (Array arr) = 
  case KM.lookup "items" schema of
    Just itemSchema -> 
      mapM_ (validateValue itemSchema) (V.toList arr)
    Nothing -> return ()
validateArray _ _ = Left "Esperado tipo array"