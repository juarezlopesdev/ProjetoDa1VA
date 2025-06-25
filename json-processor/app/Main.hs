module Main where

import CLI (Command(..), parseCommand)
import Filter (filterJson)
import Validation (validateJson)
import Options.Applicative 
    ( execParser, info, helper, fullDesc, progDesc, (<**>) )
import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  cmd <- execParser $ info (parseCommand <**> helper) $
    fullDesc <> progDesc "Ferramenta de Processamento JSON"
  case cmd of
    Filter expr input output -> do
      inputData <- BL.readFile input
      case filterJson expr inputData of
        Left err -> do
          putStrLn $ "Erro: " ++ err
          exitFailure
        Right result -> do
          case output of
            Just outFile -> BL.writeFile outFile result
            Nothing -> BL.putStr result
          exitSuccess
    ValidateSchema schema input -> do
      schemaData <- BL.readFile schema
      inputData  <- BL.readFile input
      case validateJson schemaData inputData of
        Left err -> do
          putStrLn $ "Erro: " ++ err
          exitFailure
        Right _ -> do
          putStrLn "Validação bem-sucedida!"
          exitSuccess