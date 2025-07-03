# json-processor: Ferramenta de Processamento JSON em Haskell

`json-processor` é uma ferramenta de linha de comando desenvolvida em Haskell, focada na manipulação e validação de arquivos JSON. Ela permite que usuários processem dados JSON de forma eficiente, realizando operações de filtragem e validação de esquema diretamente do terminal.

## Tecnologias Utilizadas

* **Linguagem:** Haskell
* **Build Tool:** Stack
* **Bibliotecas Principais:**
    * `Aeson`: Para parsing e manipulação de dados JSON.
    * `optparse-applicative`: Para a criação de uma interface de linha de comando (CLI) robusta e auto-documentada.
    * `bytestring` e `vector`: Para manipulação eficiente de dados.

---

## Funcionalidades e Regras de Negócio

### 1. Modo de Filtragem de Dados (`filtrar`)

Esta funcionalidade permite que o usuário filtre um arquivo JSON (que contém um array de objetos) com base em um critério de igualdade para um campo específico.

> **Regras de Negócio:**
>
> * Para a filtragem ocorrer, o arquivo de entrada **deve existir** e ser um **JSON válido**.
> * O conteúdo do arquivo de entrada **deve ser um array JSON** `[{...}, {...}]` no nível raiz. A filtragem não funcionará em um único objeto ou outros tipos de dados.
> * O critério de filtro **deve ser fornecido** no formato `caminho.chave=valor`. Por exemplo: `status=ativo` ou `dados.cidade=Recife`.
> * A filtragem busca por **igualdade exata** e diferencia maiúsculas de minúsculas. Atualmente, a comparação é feita apenas com valores do tipo `String`.
> * A operação de filtragem **nunca modifica o arquivo original**. O resultado é exibido no console ou, se um arquivo de saída for especificado, um novo arquivo é criado ou sobrescrito com os dados filtrados.

### 2. Modo de Validação de Esquema (`validar`)

Esta funcionalidade permite que o usuário valide se um arquivo JSON está em conformidade com um esquema pré-definido em outro arquivo JSON.

> **Regras de Negócio:**
>
> * Para a validação ocorrer, tanto o arquivo de dados quanto o arquivo de esquema **devem existir** e ser **JSONs válidos**.
> * O arquivo de dados a ser validado **deve ser um objeto JSON** `{...}` no nível raiz. A validação não é suportada para arrays ou outros tipos de dados.
> * O arquivo de esquema **deve seguir um formato simplificado e customizado** para este projeto, contendo duas chaves principais:
>     1.  `"required"`: Uma lista de chaves (strings) que são obrigatórias no arquivo de dados.
>     2.  `"properties"`: Um objeto que mapeia nomes de chaves aos seus tipos de dados esperados (ex: `"String"`, `"Number"`, `"Bool"`, `"Object"`, `"Array"`).
> * A validação verifica duas condições:
>     1.  **Presença:** Se todos os campos listados em `"required"` existem no arquivo de dados.
>     2.  **Tipo:** Se os campos presentes no arquivo de dados correspondem aos tipos definidos em `"properties"`.
> * Se todas as regras forem satisfeitas, uma mensagem de sucesso é exibida. Caso contrário, **todos os erros encontrados** são listados.

### 3. Interação com Arquivos (Regra Geral)

Esta regra descreve como o sistema interage com o sistema de arquivos.

> **Regras de Negócio:**
>
> * O sistema é **stateless** (não guarda estado). A cada execução, as informações necessárias são lidas diretamente dos arquivos de entrada especificados nos argumentos.
> * As operações **nunca alteram os arquivos de entrada**.
> * Quando uma operação resulta na escrita de um arquivo (como na filtragem com um caminho de saída), o sistema cria um novo arquivo ou **sobrescreve completamente** um arquivo existente com o mesmo nome. Não há atualização parcial de arquivos.

---

## Como Compilar e Executar

**1. Compilar o Projeto**

No diretório raiz do projeto, execute o comando abaixo. O Stack irá baixar as dependências e compilar o código.

```bash
stack build
```
**2. Executar a Ferramenta**

Use ``stack exec json-processor --`` seguido dos comandos e argumentos desejados. Para ver todas as opções disponíveis e suas descrições, use o comando --help:

```bash
stack exec json-processor -- --help
```

# Exemplos de Uso

A seguir estão exemplos práticos para as duas principais funcionalidades da ferramenta.

**Filtragem**

- Filtrar usuários com status "ativo" e exibir no console:

```
stack exec json-processor -- filtrar --filter "status=ativo" entrada.json
```

- Filtrar por um campo aninhado (``dados.cidade``) e salvar em um arquivo de saída:

```
stack exec json-processor -- filtrar --filter "dados.cidade=Olinda" entrada.json saida.json
```

**Validação**

- Validar um arquivo de dados que está em conformidade com o esquema:

```
stack exec json-processor -- validar esquema.json dado_para_validar.json
```

Saída Esperada:
```
Validação bem-sucedida: O arquivo JSON está em conformidade com o esquema
```

- Validar um arquivo de dados que viola as regras do esquema:

```
stack exec json-processor -- validar esquema.json dado_invalido.json
```

Saída Esperada:

```
Erros de validação encontrados:
- Campo obrigatório ausente: usuario
- Tipo incorreto para o campo 'id'. Esperado: Number, encontrado: String
- Tipo incorreto para o campo 'status'. Esperado: String, encontrado: Bool
```