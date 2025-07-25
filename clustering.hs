import System.IO
import Data.List (minimumBy, maximumBy, intercalate)
import Data.Function (on)
import Control.Monad (replicateM)

-- Tipo para representar um vetor (ponto no espaço)
type Vector = [Double]

-- Tipo para representar um cluster (lista de índices)
type Cluster = [Int]

-- Função para calcular distância euclidiana entre dois vetores
euclidean :: Vector -> Vector -> Double
euclidean v1 v2 = sqrt $ sum $ zipWith (\a b -> (a - b) ** 2) v1 v2

-- Função para ler vetores de um arquivo
readVectors :: String -> IO [Vector]
readVectors filename = do
    content <- readFile filename
    let linesContent = lines content
    return $ map (map read . words . map (\c -> if c == ',' then ' ' else c)) linesContent

-- Função para encontrar o índice do vetor mais próximo não visitado
findNearestUnvisited :: [Vector] -> [Bool] -> Vector -> Int
findNearestUnvisited vectors visited current = 
    let unvisitedIndices = [i | (i, vis) <- zip [0..] visited, not vis]
        distances = [(i, euclidean current (vectors !! i)) | i <- unvisitedIndices]
    in fst $ minimumBy (compare `on` snd) distances

-- Função para construir o caminho usando algoritmo do vizinho mais próximo
buildPath :: [Vector] -> [Int]
buildPath vectors = buildPath' [0] (True : replicate (length vectors - 1) False)
  where
    buildPath' path visited
        | all id visited = path
        | otherwise = 
            let current = vectors !! last path
                nextIdx = findNearestUnvisited vectors visited current
                newVisited = take nextIdx visited ++ [True] ++ drop (nextIdx + 1) visited
            in buildPath' (path ++ [nextIdx]) newVisited

-- Função para calcular distância entre dois pontos consecutivos no caminho
calculateDistance :: [Vector] -> Int -> Int -> Double
calculateDistance vectors i j = euclidean (vectors !! i) (vectors !! j)

-- Função para encontrar a maior distância em um cluster
findMaxDistance :: [Vector] -> Cluster -> (Int, Double)
findMaxDistance vectors cluster = 
    let pairs = [(i, calculateDistance vectors (cluster !! i) (cluster !! (i + 1))) 
                | i <- [0..length cluster - 2]]
    in maximumBy (compare `on` snd) pairs

-- Função para dividir um cluster na posição de maior distância
splitCluster :: Cluster -> Int -> [Cluster]
splitCluster cluster pos = [take (pos + 1) cluster, drop (pos + 1) cluster]

-- Função principal de clustering
performClustering :: [Vector] -> Int -> [Cluster]
performClustering vectors k = 
    let initialPath = buildPath vectors
        initialClusters = [initialPath]
    in clusteringLoop vectors initialClusters (k - 1)
  where
    clusteringLoop _ clusters 0 = clusters
    clusteringLoop vectors clusters remaining = 
        let clustersWithDistances = [(i, cluster, findMaxDistance vectors cluster) 
                                   | (i, cluster) <- zip [0..] clusters]
            (maxIdx, maxCluster, (splitPos, _)) = maximumBy (\(_, _, (_, d1)) (_, _, (_, d2)) -> compare d1 d2) clustersWithDistances
            newClusters = take maxIdx clusters ++ 
                         splitCluster maxCluster splitPos ++ 
                         drop (maxIdx + 1) clusters
        in clusteringLoop vectors newClusters (remaining - 1)

-- Função para escrever clusters no arquivo de saída
writeClusters :: String -> [Cluster] -> IO ()
writeClusters filename clusters = do
    let content = unlines [intercalate ", " (map (show . (+1)) cluster) | cluster <- clusters]
    writeFile filename content

-- Função principal
main :: IO ()
main = do
    putStr "Forneça o nome do arquivo de entrada: "
    hFlush stdout
    filein <- getLine
    
    putStr "Forneça o nome do arquivo de saída: "
    hFlush stdout
    fileout <- getLine
    
    putStr "Forneça o número de grupos (K): "
    hFlush stdout
    k <- readLn
    
    vectors <- readVectors filein
    let clusters = performClustering vectors k
    writeClusters fileout clusters
    
    putStrLn "Clustering concluído!"

