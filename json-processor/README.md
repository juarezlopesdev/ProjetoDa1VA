# Ferramenta de Processamento JSON

## Funcionalidades e Regras de Negócio

### 1. Modo de Filtragem
- **Regras:**
  - **RN01:** O arquivo de entrada deve existir e ser um JSON válido.
  - **RN02:** A expressão de filtro deve seguir o formato `campo[.subcampo]*=valor`.
  - **RN03:** Campos aninhados inexistentes resultam em erro.
  - **RN04:** O sistema recupera o último estado do arquivo `last_state.json` ao iniciar.
  - **RN05:** Toda filtragem bem-sucedida atualiza `last_state.json`.

- **Fluxo:**
  ```mermaid
  graph TD
    A[Início] --> B[Carregar last_state.json]
    B --> C{Arquivo existe?}
    C -->|Sim| D[Processar filtro]
    C -->|Não| E[Criar estado inicial]
    D --> F[Atualizar last_state.json]
### 2. Modo de Validação
- **Regras**:

    - **RN06**: O esquema e o JSON de entrada devem ser válidos.

    - **RN07**: Campos marcados como required no esquema devem existir.

    - **RN08**: Tipos de campos devem coincidir com o esquema.

    - **RN09**: Erros de validação são registrados em last_state.json.

- **Exemplo de Validação**:

  ```haskell
    -- Esquema exige "id" (number) e "nome" (string)
    validateJson schema.json data.json
    -- Falha se: 
    -- 1. "id" não existir ou for string
    -- 2. "nome" estiver ausente
### 3. Persistência de Estado
- **Regras**:

    - **RN10**: Ao iniciar, o sistema carrega:

        - Últimos arquivos processados

        - Estatísticas de operações

        - Erros recentes

    - **RN11**: Toda operação bem-sucedida atualiza:

        - Timestamp da última operação

        - Contador de operações

        - Dados de saída

- **Estrutura de last_state.json**:
    ```json
    {
    "last_operation": "filter",
    "timestamp": "2024-06-10T12:00:00Z",
    "success_count": 5,
    "last_output": {"filter": "status=ativo", "matches": 3}
    }
## Como Usar
```bash
# Filtragem (atualiza estado)
stack exec json-processor -- filter --filter "status=ativo" input.json

# Validação (verifica esquema)
stack exec json-processor -- validate --schema schema.json data.json
