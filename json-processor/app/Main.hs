module Main where

import CLI (Command(..), parseCommand)
import Filter (filterJson)
import Validation (validateJson)
import Persistence (loadState, updateState)
import Options.Applicative (execParser, info, helper, fullDesc, progDesc, (<**>))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value, decode)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  currentState <- loadState
  cmd <- execParser $ info (parseCommand <**> helper) $
    fullDesc <> progDesc "Ferramenta de Processamento JSON com Estado Persistente"
    
  case cmd of
    Filter expr input output -> do
      inputData <- BL.readFile input
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
      schemaData <- BL.readFile schema
      inputData  <- BL.readFile input
      case validateJson schemaData inputData of
        Left err -> do
          putStrLn $ "Erro na validação: " ++ err
          exitFailure
        Right () -> do
          _ <- updateState "validate" Nothing currentState
          putStrLn "Validação bem-sucedida!"
          exitSuccess