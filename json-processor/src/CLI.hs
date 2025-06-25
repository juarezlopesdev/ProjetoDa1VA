module CLI where

import Options.Applicative

data Command
  = Filter
      { filterExpr :: String
      , inputFile  :: FilePath
      , outputFile :: Maybe FilePath
      }
  | ValidateSchema
      { schemaFile :: FilePath
      , inputFile  :: FilePath
      }
  deriving (Show)

parseCommand :: Parser Command
parseCommand = subparser
  (  command "filter"
     (info filterOptions
        (progDesc "Filtra JSON baseado em expressão (ex: 'campo.subcampo=valor')"))
  <> command "validate"
     (info validateOptions
        (progDesc "Valida JSON contra um esquema"))
  )

filterOptions :: Parser Command
filterOptions = Filter
  <$> strOption
        (  long "filter"
        <> short 'f'
        <> metavar "EXPR"
        <> help "Expressão de filtro (formato: 'campo.subcampo=valor')")
  <*> argument str
        (  metavar "INPUT"
        <> help "Arquivo de entrada JSON")
  <*> optional (argument str
        (  metavar "OUTPUT"
        <> help "Arquivo de saída JSON (opcional)"))

validateOptions :: Parser Command
validateOptions = ValidateSchema
  <$> strOption
        (  long "schema"
        <> short 's'
        <> metavar "SCHEMA_FILE"
        <> help "Arquivo de esquema JSON")
  <*> argument str
        (  metavar "INPUT"
        <> help "Arquivo JSON para validação")