import sys

def ler_arquivo_como_grupos(filepath):
    grupos = set()
    with open(filepath, 'r') as f:
        for linha in f:
            if linha.strip():  # Ignora linhas vazias
                numeros = [int(x.strip()) for x in linha.strip().split(',')]
                grupo_ordenado = tuple(sorted(numeros))
                grupos.add(grupo_ordenado)
    return grupos

def comparar_arquivos(arquivo1, arquivo2):
    grupos1 = ler_arquivo_como_grupos(arquivo1)
    grupos2 = ler_arquivo_como_grupos(arquivo2)
    
    if grupos1 == grupos2:
        print("✅ Os arquivos são equivalentes.")
    else:
        print("❌ Os arquivos são diferentes.")
        somente_em_1 = grupos1 - grupos2
        somente_em_2 = grupos2 - grupos1
        if somente_em_1:
            print("\nGrupos somente no arquivo 1:")
            for grupo in somente_em_1:
                print(grupo)
        if somente_em_2:
            print("\nGrupos somente no arquivo 2:")
            for grupo in somente_em_2:
                print(grupo)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Uso: python comparar_grupos.py <arquivo1.txt> <arquivo2.txt>")
        sys.exit(1)

    arquivo1 = sys.argv[1]
    arquivo2 = sys.argv[2]
    comparar_arquivos(arquivo1, arquivo2)

