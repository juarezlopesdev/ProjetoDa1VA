-- src/JsonProc/Filter.hs
{-# LANGUAGE OverloadedStrings #-}

module JsonProc.Filter (
    FilterOptions(..),
    runFilter
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import Control.Monad (foldM) -- <-- 1. IMPORTAÇÃO ADICIONADA

-- Opções para o comando de filtro
data FilterOptions = FilterOptions
  { criterion :: String
  , inputFile :: FilePath
  , outputFile :: Maybe FilePath
  }

-- Função principal para executar a operação de filtragem
runFilter :: FilterOptions -> IO ()
runFilter opts = do
  case parseCriterion (criterion opts) of
    Left err -> putStrLn $ "Erro no critério de filtro: " ++ err
    Right (path, value) -> do
      content <- B.readFile (inputFile opts)
      case A.decode' content of
        Nothing -> putStrLn "Erro: Não foi possível decodificar o JSON de entrada."
        Just (A.Array arr) -> do
          let filteredData = A.Array . V.fromList $ mapMaybe (applyFilter path value) (V.toList arr)
          let outputContent = A.encode filteredData
          case outputFile opts of
            Just file -> B.writeFile file outputContent
            Nothing   -> B.putStr outputContent
        Just _ -> putStrLn "Erro: O JSON de entrada para filtragem deve ser um array de objetos."

-- Tenta aplicar o filtro a um único valor JSON (espera-se um objeto)
applyFilter :: [T.Text] -> T.Text -> A.Value -> Maybe A.Value
applyFilter path value jsonValue =
  case jsonValue of
    A.Object obj ->
      if navigateAndCheck obj path value
        then Just jsonValue
        else Nothing
    _ -> Nothing

-- Navega pelo objeto JSON e verifica se o campo no final do caminho corresponde ao valor
navigateAndCheck :: A.Object -> [T.Text] -> T.Text -> Bool
navigateAndCheck obj path targetValue =
  case navigatePath obj path of
    Just (A.String s) -> s == targetValue
    _                 -> False

-- *** 2. LÓGICA DE NAVEGAÇÃO CORRIGIDA ***
-- Função auxiliar para ser usada com foldM
step :: A.Value -> T.Text -> Maybe A.Value
step (A.Object o) key = KM.lookup (K.fromText key) o
step _ _              = Nothing

-- Função de navegação corrigida usando foldM
navigatePath :: A.Object -> [T.Text] -> Maybe A.Value
navigatePath obj path = foldM step (A.Object obj) path

-- Analisa a string de critério "caminho.chave=valor"
parseCriterion :: String -> Either String ([T.Text], T.Text)
parseCriterion s =
  case T.splitOn "=" (T.pack s) of
    [pathStr, value] | not (T.null value) -> Right (T.splitOn "." pathStr, value)
    _ -> Left "Formato inválido. Use 'caminho.chave=valor'."