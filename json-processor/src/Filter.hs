module Filter (filterJson) where

import Data.Aeson
import Data.Text (Text, pack, unpack)
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key, fromText)
import Data.ByteString.Lazy (ByteString)
import Data.List.Split (splitOn)

filterJson :: String -> ByteString -> Either String ByteString
filterJson expr input = do
  value <- eitherDecode input
  let (path, expectedValue) = parseExpression expr
  filtered <- applyFilter path expectedValue value
  return $ encode filtered

parseExpression :: String -> ([Key], Value)
parseExpression expr =
  case break (=='=') expr of
    (pathStr, '=':valueStr) -> 
      (map (fromText . pack) (splitOn "." pathStr), String (pack valueStr))
    _ -> error "Expressão inválida. Use formato: campo.subcampo=valor"

applyFilter :: [Key] -> Value -> Value -> Either String Value
applyFilter path expected (Array arr) =
  Array <$> V.mapM (filterObject path expected) arr
applyFilter _ _ _ = Left "O JSON de entrada deve ser um array"

filterObject :: [Key] -> Value -> Value -> Either String Value
filterObject path expected value@(Object o) =
  case getNestedField path value of
    Just v | v == expected -> Right value
    _ -> Left "Objeto não corresponde ao filtro"
filterObject _ _ _ = Left "Elemento inválido"

getNestedField :: [Key] -> Value -> Maybe Value
getNestedField [] value = Just value
getNestedField (k:ks) (Object o) = 
  KM.lookup k o >>= getNestedField ks
getNestedField _ _ = Nothing