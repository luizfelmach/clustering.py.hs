import subprocess

testes = [
    ("samples/in/base1.csv", "samples/myout/base1k1.txt", 1),
    ("samples/in/base1.csv", "samples/myout/base1k2.txt", 2),
    ("samples/in/base1.csv", "samples/myout/base1k3.txt", 3),
    ("samples/in/base1.csv", "samples/myout/base1k4.txt", 4),
    ("samples/in/base1.csv", "samples/myout/base1k8.txt", 8),

    ("samples/in/base2.csv", "samples/myout/base2k2.txt", 2),
    ("samples/in/base2.csv", "samples/myout/base2k3.txt", 3),
    ("samples/in/base2.csv", "samples/myout/base2k4.txt", 4),
    ("samples/in/base2.csv", "samples/myout/base2k5.txt", 5),

    ("samples/in/base3.csv", "samples/myout/base3k2.txt", 2),
    ("samples/in/base3.csv", "samples/myout/base3k3.txt", 3),
    ("samples/in/base3.csv", "samples/myout/base3k5.txt", 5),

    ("samples/in/bat1.txt", "samples/myout/bat1.txt", 3),
    ("samples/in/bat2.txt", "samples/myout/bat2.txt", 3),
    ("samples/in/bat3.txt", "samples/myout/bat3.txt", 6),
    ("samples/in/bat4.txt", "samples/myout/bat4.txt", 9),
]

script_alvo = "main.py"

for filein, fileout, k in testes:
    print(f"🔄 Rodando teste com {filein}, K={k}")

    entrada_simulada = f"{filein}\n{fileout}\n{k}\n"

    resultado = subprocess.run(
        ["python", script_alvo],
        input=entrada_simulada.encode(),
        capture_output=True,
    )

    if resultado.returncode == 0:
        print("✅ Sucesso!")
        print(resultado.stdout.decode())
    else:
        print("❌ Falhou!")
        print(resultado.stderr.decode())
