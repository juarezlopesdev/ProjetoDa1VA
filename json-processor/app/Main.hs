-- app/Main.hs
{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import qualified JsonProc.Filter as F
import qualified JsonProc.Validation as V

-- Define a estrutura de dados para os nossos comandos
data Command
  = Filter F.FilterOptions
  | Validate V.ValidationOptions

-- Parser para as opções de filtragem
filterOptions :: Parser F.FilterOptions
filterOptions = F.FilterOptions
  <$> strOption
      ( long "filter"
     <> metavar "CRITÉRIO"
     <> help "Critério de filtragem no formato 'caminho.chave=valor'" )
  <*> strArgument
      ( metavar "ENTRADA"
     <> help "Arquivo JSON de entrada" )
  <*> optional (strArgument
      ( metavar "SAIDA"
     <> help "Arquivo de saída (opcional, padrão: stdout)" ))

-- Parser para as opções de validação
validationOptions :: Parser V.ValidationOptions
validationOptions = V.ValidationOptions
  <$> strArgument
      ( metavar "ESQUEMA"
     <> help "Arquivo de esquema JSON" )
  <*> strArgument
      ( metavar "ENTRADA"
     <> help "Arquivo de dados JSON para validar" )

-- Parser principal que combina os subcomandos
commands :: Parser Command
commands = subparser
  ( command "filtrar" (info (Filter <$> filterOptions) (progDesc "Filtra um arquivo JSON com base em um critério."))
 <> command "validar" (info (Validate <$> validationOptions) (progDesc "Valida um arquivo JSON contra um esquema."))
  )

-- Função principal que executa o parser e age de acordo com o comando
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Filter opts     -> F.runFilter opts
    Validate opts   -> V.runValidation opts
  where
    opts = info (commands <**> helper)
      ( fullDesc
     <> progDesc "Ferramenta de Processamento JSON em Haskell"
     <> header "json-processor - uma ferramenta para manipular e validar JSON" )