import os
import sys

def ler_arquivo_como_grupos(filepath):
    grupos = set()
    with open(filepath, 'r') as f:
        for linha in f:
            if linha.strip():
                numeros = [int(x.strip()) for x in linha.strip().split(',')]
                grupo_ordenado = tuple(sorted(numeros))
                grupos.add(grupo_ordenado)
    return grupos

def comparar_arquivos(path1, path2):
    grupos1 = ler_arquivo_como_grupos(path1)
    grupos2 = ler_arquivo_como_grupos(path2)
    return grupos1 == grupos2, grupos1 - grupos2, grupos2 - grupos1

def comparar_pastas(pasta1, pasta2):
    arquivos1 = set(os.listdir(pasta1))
    arquivos2 = set(os.listdir(pasta2))
    comuns = arquivos1 & arquivos2

    if not comuns:
        print("❌ Nenhum arquivo em comum para comparar.")
        return

    for arquivo in sorted(comuns):
        caminho1 = os.path.join(pasta1, arquivo)
        caminho2 = os.path.join(pasta2, arquivo)

        try:
            iguais, difs1, difs2 = comparar_arquivos(caminho1, caminho2)
            if iguais:
                print(f"✅ {arquivo} é equivalente.")
            else:
                print(f"❌ {arquivo} é diferente.")
                if difs1:
                    print(f"   ➤ Somente em {pasta1}: {difs1}")
                if difs2:
                    print(f"   ➤ Somente em {pasta2}: {difs2}")
        except Exception as e:
            print(f"⚠️ Erro ao comparar {arquivo}: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Uso: python comparar_pastas.py <pasta1> <pasta2>")
        sys.exit(1)

    pasta1 = sys.argv[1]
    pasta2 = sys.argv[2]

    comparar_pastas(pasta1, pasta2)
