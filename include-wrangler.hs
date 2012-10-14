import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Control.Monad
import Data.Data
import Data.List
import System.IO
import Text.Printf

-- An includes graph is just a map from verts to list of verts.
-- (Use list instead of Set since we wat to preserve include order!)
data IncludesGraph v = IGraph (Map.Map v [v]) deriving Show

edgeMap (IGraph em) = em
vList graph = Map.keys $ edgeMap graph

edgesFrom graph v = (edgeMap graph) Map.! v
edgePairs graph = Map.foldrWithKey (\ k v a -> (zip (repeat k) v) ++ a) [] $ edgeMap graph

-- Depth first search on an includes graph from v.
-- Follows the same "search" order that C++ preprocessor would. Avoids cycles by
-- recording visited list - so assumes every include is guarded by ifdefs/pragma once.
dfs' graph v visited = next follow
	where follow = filter (\v -> not $ Set.member v visited) $ edgesFrom graph v
	      descend u =  dfs' graph u (Set.insert u visited)
	      next [] = visited
              next (u:us) = dfs' graph v $ descend u
	      
dfs graph v = dfs' graph v (Set.fromList [v])

-- Remove edge (v,u) from an include graph.
removeEdge (IGraph em) (v,u) = IGraph $ Map.adjust (filter ((/=) u)) v em
-- Remove node b from an include graph.
removeNode (IGraph em) v = IGraph $ Map.map (filter ((/=) v)) $ Map.delete v em

-- The "cost" of an include statement w.r.t file w.
icost' graph (u,v) w = (Set.size $ dfs graph w) - (Set.size $ dfs (removeEdge graph (u,v)) w)
-- The "cost" of an include statement w.r.t. to a list of files s - (probably list of .cpp files)
icost graph s (u,v) = sum $ map (icost' graph (u,v)) $ s
-- The "cost" of an include file w.r.t file w.
hcost' graph f w = (Set.size $ dfs graph w) - (Set.size $ dfs (removeNode graph f) w)
-- The "cost" of an include file w.r.t. to a list of files s - (probably list of .cpp files)
hcost graph s f  = sum $ map (hcost' graph f) $ s
-- The "cost" of a source file.
ccost graph f = Set.size $ dfs graph f

endsWith a b = a == drop (length b - length a) b

-- Extract include statements from text string.
extractInclude line = if (take 8 line) == "#include" then extract (drop 8 line) else []
		where extract t = takeWhile (\x -> notElem x ">\"") $ drop 1 $ 
			dropWhile (\x -> notElem x "<\"") t
extractIncludes text = filter (not.null) $ foldr(:) [] $ map extractInclude $ lines text

extractIncludesFromFile f = do text <- readFile f
                               return $ extractIncludes text

joinFN p1 p2 = joinPath [p1, p2]

-- Recursive director listing, from "Real world haskell."
lsr topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then lsr path
      else return [path]
  return (concat paths)

-- Recursive directory listing, relative path.
lsrr path = do listing <- lsr path
               return $ map (drop (length path)) listing

isInclude f = (endsWith ".h" f) || (endsWith ".hpp" f)

getLines file = do s <- readFile file
                   return $ lines s

-- Build a map from includes paths used in .c/.h files to full file paths, given a list of include
-- dirs and list of list of incude file partial paths.
buildIncludesMap includeDirs includes =  Map.fromList $ reverse $ map (\(fn,dir) -> (fn, joinFN dir fn)) 
	$ foldr (++) [] $ map (\(a,b) -> zip a b) $ zip includes $ map repeat includeDirs

-- Convert an include graph to use integer vertices. Returns new grap, a mapping from old to integer 
-- verts and the converse.
-- Doing  this before expensive operations on graphs can speed things up.
-- (String comparasons are more expensive than integer comparasons!)
compress graph = ((IGraph edges), ((Map.!) vmapping, (Map.!) ivmapping))
	where vs = vList graph
	      vmapping = Map.fromList $ zip vs [0..]
	      ivmapping = Map.fromList $ zip [0..] vs
	      vmLookup v = vmapping Map.! v
              edges = Map.mapKeys vmLookup $ Map.map (map vmLookup) $ edgeMap graph

pairMap f (a, b) = (f a, f b)

createIncludeGraph includeDirs sourceFiles = do 
	-- List all accessable include files.
	putStrLn "Finding .h files..."
	let includeDirs' = map addTrailingPathSeparator includeDirs
	fileList <- mapM lsrr includeDirs'
	let includes = map (filter isInclude) fileList
	-- Create short to long path map for include files.
	let iMap = buildIncludesMap includeDirs includes
	let iFullPath = Map.elems iMap
	let iShortPath = Map.keys iMap
	-- Create edge map for includes within .h files.
	putStrLn "Parsing include statements..."
	includesIncludes <- mapM extractIncludesFromFile iFullPath
	-- (remove references to files not found in include dirs)
	let includesIncludesPruned = map (filter (`Map.member` iMap)) includesIncludes
	let iMap = Map.fromList $ zip iShortPath includesIncludesPruned
	-- Create edge map for includes within .cpp files.
	sourceIncludes <- mapM extractIncludesFromFile sourceFiles
	let sourceIncludesPruned = map (filter (`Map.member` iMap)) sourceIncludes
	let sMap = Map.fromList $ zip sourceFiles sourceIncludesPruned
	-- Create final includes graph.
	let edges = Map.union sMap iMap
	return $ IGraph edges

progressPercent n = map fromRational $ [100*i/n | i <- [1..n]] 
zip3Percent a b = zip3 (progressPercent $ fromIntegral $ length a) a b
showProgress nameOfThing (percent, thing, cost) = do 
	printf "Progress: %.1f%%, %s: %s, cost: %s\n" 
		(percent::Float) nameOfThing (show thing) (show cost)

showCost (cost,item) = "Cost: " ++ (show cost) ++ ", from: " ++ (show item) ++ "\n"
showCosts l = foldr (++) [] $ map showCost l

toGraphDot g = "digraph includes { \n" ++
	(foldr (\(v,u) a -> a ++ "\n" ++ "\"" ++ v ++ "\"" ++ "->" ++ "\"" ++ u ++ "\";") [] 
	 $ edgePairs g)
	++ "\n}"
main = do 
	-- Read input files
	putStrLn "Reading input files (source_files and include_dirs)..."
	sourceFiles <- getLines "source_files"
	includeDirs <- getLines "include_dirs"
	-- Create graph
	graph <- createIncludeGraph includeDirs sourceFiles
	-- "Compress" the graph for faster cost analysis.
	let (cgraph, (cvmap, icvmap)) = compress graph
	let cep = edgePairs cgraph
	let ephuman = map (pairMap icvmap) cep
	let csource = map cvmap sourceFiles
	-- Calculate include the costs.
	putStrLn "Calculating include costs..."
	let icosts = map (icost cgraph csource) cep
	mapM (showProgress "include")  $ zip3Percent ephuman icosts
	putStrLn "Writing include costs to file \"include_costs\"..."
	writeFile "include_costs" $ showCosts $ reverse $ sort $ zip icosts ephuman
	-- Calculate header file costs.
	putStrLn "Calculating header costs..."
	let headers = filter isInclude $ vList graph
	let cheaders = map cvmap headers
	let hcosts = map (hcost cgraph csource) $ cheaders
	mapM (showProgress "header") $ zip3Percent headers hcosts
	putStrLn "Writing header file costs to file \"header_costs\"..."
	writeFile "header_costs" $ showCosts $ reverse $ sort $ zip hcosts headers
	-- Calculate CPP file costs.
	putStrLn "Translation unit costs..."
	let ccosts = map (ccost cgraph) csource 
	mapM (showProgress "source") $ zip3Percent sourceFiles ccosts
	putStrLn "Writing source file costs to file \"translation_unit_costs\"..."
	writeFile "translation_unit_costs" $ showCosts $ reverse $ sort $ zip ccosts sourceFiles
	-- Graphvis output
	putStrLn "Outputting graphvis formatted include graph to incude_graph.dot..."
	writeFile "include_graph.dot" $ toGraphDot graph

