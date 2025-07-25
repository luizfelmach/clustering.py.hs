import System.IO
import Data.List (elemIndex, maximumBy)

-- Função para dividir string por vírgula
splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy delimiter str =
    case break (== delimiter) str of
        (before, []) -> [before]
        (before, _:after) -> before : splitBy delimiter after

-- Função para calcular distância euclidiana
euclidean :: [Double] -> [Double] -> Double
euclidean v1 v2 = sqrt $ sum $ zipWith (\a b -> (a - b) ** 2) v1 v2

-- Função para ler vetores do arquivo
readVectors :: String -> IO [[Double]]
readVectors filename = do
    content <- readFile filename
    return (map (map read . splitBy ',') (lines content))

-- Função para encontrar o próximo vértice não visitado mais próximo
findNearest :: [[Double]] -> [Bool] -> [Double] -> Int
findNearest vectors visited current =
    snd $ minimum [(euclidean current (vectors !! i), i) | (i, vis) <- zip [0..] visited, not vis]

-- Função para construir o caminho inicial usando nearest neighbor
buildPath :: [[Double]] -> ([Int], [(Int, Int, Double)])
buildPath vectors = buildPath' [0] (True : replicate (length vectors - 1) False) []
  where
    buildPath' path visited distances
        | all id visited = (path, distances)
        | otherwise =
            case findNearest vectors visited (vectors !! last path) of
                nextIdx -> buildPath'
                    (path ++ [nextIdx])
                    (take nextIdx visited ++ [True] ++ drop (nextIdx + 1) visited)
                    (distances ++ [(last path, nextIdx, euclidean (vectors !! last path) (vectors !! nextIdx))])

-- Função para encontrar a aresta com maior distância
findMaxEdge :: [Int] -> [(Int, Int, Double)] -> (Int, Int, Double)
findMaxEdge cluster distances =
    maximumBy (\(_, _, d1) (_, _, d2) -> compare d1 d2)
    [edge | edge@(v1, v2, _) <- distances,
     v1 `elem` cluster && v2 `elem` cluster &&
     abs (fromJust (elemIndex v1 cluster) - fromJust (elemIndex v2 cluster)) == 1]
  where
    fromJust (Just x) = x
    fromJust Nothing = error "Element not found"

-- Função para dividir cluster na aresta de maior distância
splitCluster :: [Int] -> [(Int, Int, Double)] -> ([Int], [Int])
splitCluster cluster distances =
    case findMaxEdge cluster distances of
        (v1, v2, _) ->
            case (elemIndex v1 cluster, elemIndex v2 cluster) of
                (Just i1, Just i2) ->
                    case (min i1 i2, max i1 i2) of
                        (minIdx, maxIdx) -> (take (minIdx + 1) cluster, drop maxIdx cluster)

-- Função para criar k clusters
createClusters :: Int -> [Int] -> [(Int, Int, Double)] -> [[Int]]
createClusters k path distances = createClusters' (k - 1) [path]
  where
    createClusters' 0 clusters = clusters
    createClusters' remaining clusters =
        case maximumBy (\c1 c2 -> compare (maxEdgeDist c1) (maxEdgeDist c2)) clusters of
            targetCluster ->
                case splitCluster targetCluster distances of
                    (part1, part2) -> createClusters' (remaining - 1)
                        (part1 : part2 : filter (/= targetCluster) clusters)
      where
        maxEdgeDist cluster =
            case findMaxEdge cluster distances of
                (_, _, dist) -> dist

-- Função para converter índices para string (1-indexado)
clusterToString :: [Int] -> String
clusterToString cluster = intercalate ", " (map (show . (+1)) cluster)

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

main :: IO ()
main = do
    putStr "Forneca o nome do arquivo de entrada: "
    hFlush stdout
    filein <- getLine

    putStr "Forneca o nome do arquivo de saida: "
    hFlush stdout
    fileout <- getLine

    putStr "Forneca o número de grupos (K): "
    hFlush stdout
    kStr <- getLine

    vectors <- readVectors filein

    case buildPath vectors of
        (path, distances) ->
            case createClusters (read kStr :: Int) path distances of
                clusters -> do
                    putStrLn "Agrupamentos:"
                    mapM_ (\cluster -> putStrLn (clusterToString cluster)) clusters
                    writeFile fileout (unlines (map clusterToString clusters))
