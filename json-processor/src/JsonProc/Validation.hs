-- src/JsonProc/Validation.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JsonProc.Validation (
    ValidationOptions(..),
    runValidation
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.List ((\\))
import GHC.Generics

data ValidationOptions = ValidationOptions
  { schemaFile :: FilePath
  , dataFile   :: FilePath
  }

data Schema = Schema
  { required   :: [T.Text]
  , properties :: KM.KeyMap T.Text
  } deriving (Show, Generic)

instance A.FromJSON Schema

runValidation :: ValidationOptions -> IO ()
runValidation opts = do
  schemaContent <- B.readFile (schemaFile opts)
  case A.decode' schemaContent of
    Nothing -> putStrLn "Erro: Não foi possível decodificar o arquivo de esquema JSON."
    Just schema -> do
      dataContent <- B.readFile (dataFile opts)
      case A.decode' dataContent of
        Nothing -> putStrLn "Erro: Não foi possível decodificar o arquivo de dados JSON. Verifique se o formato é válido."
        Just (A.Object dataObj) -> do
          let errors = validateObject schema dataObj
          if null errors
            then putStrLn "Validação bem-sucedida: O arquivo JSON está em conformidade com o esquema."
            else do
              putStrLn "Erros de validação encontrados:"
              mapM_ (putStrLn . ("- " ++)) errors
        -- Captura outros tipos de JSON válidos (Array, String, etc.) e dá uma mensagem clara
        Just other -> do
            putStrLn $ "Erro: O arquivo de dados para validação deve ser um objeto JSON, mas foi encontrado um " ++ getJsonType other ++ "."
            putStrLn "Exemplo de formato esperado: {\"chave\": \"valor\", ...}"


validateObject :: Schema -> A.Object -> [String]
validateObject schema obj =
  let requiredErrors = checkRequired (required schema) obj
      propertyErrors = checkProperties (properties schema) obj
  in requiredErrors ++ propertyErrors

checkRequired :: [T.Text] -> A.Object -> [String]
checkRequired requiredFields obj =
  let presentKeys = map K.toText (KM.keys obj)
      missingKeys = requiredFields \\ presentKeys
  in map (\k -> "Campo obrigatório ausente: " ++ T.unpack k) missingKeys

checkProperties :: KM.KeyMap T.Text -> A.Object -> [String]
checkProperties schemaProps obj =
  concatMap (validateProperty obj) (KM.toList schemaProps)
  where
    validateProperty dataObj (schemaKey, expectedType) =
      case KM.lookup schemaKey dataObj of
        Nothing -> []
        Just val ->
          if checkType val expectedType
            then []
            else ["Tipo incorreto para o campo '" ++ K.toString schemaKey ++ "'. Esperado: " ++ T.unpack expectedType ++ ", encontrado: " ++ getJsonType val]

checkType :: A.Value -> T.Text -> Bool
checkType (A.Object _) "Object" = True
checkType (A.Array _)  "Array"  = True
checkType (A.String _) "String" = True
checkType (A.Number _) "Number" = True
checkType (A.Bool _)   "Bool"   = True
checkType A.Null       "Null"   = True
checkType _ _                   = False

getJsonType :: A.Value -> String
getJsonType (A.Object _) = "Object"
getJsonType (A.Array _)  = "Array"
getJsonType (A.String _) = "String"
getJsonType (A.Number _) = "Number"
getJsonType (A.Bool _)   = "Bool"
getJsonType A.Null       = "Null"