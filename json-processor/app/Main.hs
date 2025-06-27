module Main where

import CLI (Command(..), parseCommand)
import Filter (filterJson)
import Validation (validateJson)
import Persistence (loadState, updateState)
import Options.Applicative (execParser, info, helper, fullDesc, progDesc, (<**>))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value, decode)
import System.Exit (exitFailure, exitSuccess)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text)
import qualified Data.ByteString as B

-- Função para remover BOM e converter para UTF-8
ensureUtf8 :: BL.ByteString -> BL.ByteString
ensureUtf8 bs
  | B.take 3 (BL.toStrict bs) == B.pack [0xEF, 0xBB, 0xBF] =  -- UTF-8 BOM
      BL.fromStrict $ B.drop 3 (BL.toStrict bs)
  | B.take 2 (BL.toStrict bs) == B.pack [0xFF, 0xFE] ||      -- UTF-16 LE BOM
    B.take 2 (BL.toStrict bs) == B.pack [0xFE, 0xFF] =       -- UTF-16 BE BOM
      BL.fromStrict $ encodeUtf8 (decodeUtf8 (BL.toStrict bs))
  | otherwise = bs

main :: IO ()
main = do
  currentState <- loadState
  cmd <- execParser $ info (parseCommand <**> helper) $
    fullDesc <> progDesc "Ferramenta de Processamento JSON com Estado Persistente"
    
  case cmd of
    Filter expr input output -> do
      inputData <- ensureUtf8 <$> BL.readFile input
      case filterJson expr inputData of
        Left err -> do
          putStrLn $ "Erro na filtragem: " ++ err
          exitFailure
        Right result -> do
          _ <- updateState "filter" (decode result) currentState
          case output of
            Just outFile -> BL.writeFile outFile result
            Nothing -> BL.putStr result
          putStrLn "Filtragem concluída com sucesso!"
          exitSuccess
          
    ValidateSchema schema input -> do
      schemaData <- ensureUtf8 <$> BL.readFile schema
      inputData  <- ensureUtf8 <$> BL.readFile input
      case validateJson schemaData inputData of
        Left err -> do
          putStrLn $ "Erro na validação: " ++ err
          exitFailure
        Right () -> do
          _ <- updateState "validate" Nothing currentState
          putStrLn "Validação bem-sucedida!"
          exitSuccess