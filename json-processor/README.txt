Título: Ferramenta de Processamento JSON

Descrição:
Ferramenta de linha de comando desenvolvida em Haskell para manipulação e validação de arquivos JSON.

Funcionalidades:
1. Modo de Filtragem de Dados: Permite filtrar um arquivo JSON de entrada com base em uma expressão (ex: "campo.subcampo=valor"), gerando um novo arquivo JSON ou exibindo no console.
2. Modo de Validação de Esquema: Valida um arquivo JSON de entrada contra um esquema JSON pré-definido, indicando se o arquivo é válido ou os erros encontrados.

Como compilar:
1. Certifique-se de ter o Stack instalado (https://docs.haskellstack.org/en/stable/install_and_upgrade/).
2. Execute `stack build` na raiz do projeto.

Como usar:
- Filtragem:
  stack exec json-processor -- filter --filter "expressao" entrada.json [saida.json]

- Validação:
  stack exec json-processor -- validate --schema esquema.json entrada.json

Exemplo:
  stack exec json-processor -- filter --filter "status=ativo" data.json result.json
  stack exec json-processor -- validate --schema schema.json data.json