filein = input("Forneca o nome do arquivo de entrada: ")
fileout = input("Forneca o nome do arquivo de saida: ")
k = int(input("Forneca o número de grupos (K): "))

def euclidean(v1, v2):
    return sum(tuple((a - b)**2 for a, b in zip(v1, v2))) ** 0.5

with open(filein, "r") as file:
    vectors = [ tuple(map(float, line.split(","))) for line in file ]

if __name__ == "__main__":
    path, vis, distances = [0], [True] + [False] * (len(vectors) - 1), {}

    while not all(vis):
        current = vectors[path[-1]]
        idx = min(
            (i for i, v in enumerate(vectors) if not vis[i]),
            key=lambda i: euclidean(current, vectors[i])
        )
        vis[idx] = True
        path.append(idx)
        distances[(path[-2], idx)] = euclidean(current, vectors[idx])

    clusters =  [ path ]

    for _ in range(k-1):
        clusters_idx, cluster_idx, dist_max = max(
            ((i, j, distances[(v1, cluster[j+1])])
             for i, cluster in enumerate(clusters)
             for j, v1 in enumerate(cluster[:-1])),
            key=lambda x: x[2]
        )
        target = clusters.pop(clusters_idx)
        clusters += [target[:cluster_idx+1], target[cluster_idx+1:]]

    print("Agrupamentos:")
    with open(fileout, "w") as file:
        for cluster in clusters:
            line = ", ".join(map(str, (x + 1 for x in cluster))) + "\n"
            print(line, end="")
            file.writelines(line)
