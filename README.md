# Projeto de Agrupamento de Vetores (Clustering)

Este projeto realiza o agrupamento de vetores com base na distância euclidiana entre eles. O script principal (`main.py`) recebe um arquivo com vetores numéricos, o número de grupos desejado (K) e produz um arquivo de saída com os agrupamentos resultantes.

## 📁 Estrutura do Projeto

```
.
├── main.py           # Script principal de agrupamento
├── runner.py         # Executa diferentes abordagens de agrupamento
├── diff.py           # Ferramenta de comparação de saídas
├── tests.py          # Testes automatizados
├── samples/          # Arquivos de amostra para entrada e saída e gabarito
├── README.md         # Este arquivo
```

## 🚀 Como Executar

### Pré-requisitos

- Python 3.7 ou superior

### Execução do script principal

```bash
python main.py
```

Você será solicitado a informar:

1. O nome do arquivo de entrada (com vetores, um por linha separados por vírgula).
2. O nome do arquivo de saída.
3. O número de grupos desejado (K).

### Exemplo de arquivo de entrada

```
1.0,2.0
3.1,4.2
5.3,1.2
...
```

### Exemplo de saída

O arquivo de saída conterá os vetores agrupados de acordo com a lógica implementada (inicialmente um caminho sequencial baseado na menor distância e posterior divisão em K clusters).

## 🧪 Testes

Para executar os testes:

```bash
python runner.py 
```

## 🧪 Comparar respostas

```bash
python diff.py file1 file2 # comparar 2 arquivos
python tests.py folder1 folder2 # comparar 2 pastas
```
