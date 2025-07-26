import System.IO (hFlush, stdout)
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

type Point = (Int, [Double])
type Path = [Point]
type Cluster = [Path]

euclidean :: [Double] -> [Double] -> Double
euclidean v1 v2 = sqrt $ sum $ zipWith (\a b -> (a - b) ^ 2) v1 v2

nearest :: Point -> [Point] -> Point
nearest target points = minimumBy compareDistance points
    where
    compareDistance p1 p2 = compare (distance target p1) (distance target p2)
    distance (_, coords1) (_, coords2) = euclidean coords1 coords2

path :: [Point] -> Path
path [] = []
path [p] = [p]
path (p:ps) = go p ps
  where
    go current [] = [current]
    go current rest = current : go next newRest
      where
        next = nearest current rest
        newRest = filter (/= next) rest

maxLink :: Path -> (Double, Int)
maxLink ps = maximumBy (comparing fst) distances
    where
    distances = [ (euclidean c1 c2, i)
                | ((_, c1), (_, c2), i) <- zip3 ps (drop 1 ps) [1..] ]

splitPath :: Path -> (Path, Path)
splitPath p = splitAt idx p
    where (_, idx) = maxLink p

selectMaxLink :: Cluster -> (Path, Cluster)
selectMaxLink cs = (maxPath, filter (/= maxPath) cs)
    where
    multiLinkPaths = filter (\p -> length p >= 2) cs
    maxPath = maximumBy (comparing (fst . maxLink)) multiLinkPaths

clustering :: Cluster -> Cluster
clustering cluster = newPaths ++ rest
  where
    (targetPath, rest) = selectMaxLink cluster
    (p1, p2) = splitPath targetPath
    newPaths = filter (not . null) [p1, p2]

clusteringK :: Int -> Cluster -> Cluster
clusteringK 1 cs = cs
clusteringK n cs = clusteringK (n - 1) (clustering cs)

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy delimiter str =
    case break (== delimiter) str of
        (before, []) -> [before]
        (before, _:after) -> before : splitBy delimiter after

readPoints :: String -> IO [Point]
readPoints filename = do
    content <- readFile filename
    let rawVectors = map (map read . splitBy ',') (lines content)
    return (zip [1..] rawVectors)

clustersToString :: Cluster -> String
clustersToString clusters = unlines (map clusterLine clusters)

clusterLine :: Path -> String
clusterLine path = intercalate ", " (map (show . fst) path)

intercalate :: [a] -> [[a]] -> [a]
intercalate _ []     = []
intercalate _ [x]    = x
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

    points <- readPoints filein
    let result = clustersToString $ clusteringK (read kStr :: Int) [path points]

    putStr "Agrupamentos:\n"
    putStr result
    writeFile fileout result
