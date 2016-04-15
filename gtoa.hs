-- gtoa.hs
-- Utility to convert between different graph file formats

-- Thanks to lazy evaluation, we can read huge files and process them without taking all
-- available memory.


import System.IO
import System.Environment
import Data.List
import qualified Data.Text as T



-- Graphs aren't algebraic data types: we can't construct a Graph type just like we would
-- a Tree type, because if a node A in the graph contains a (reference to) another node B,
-- and B references A, then neither A or B can be created before the other one exists. In
-- view of this, we represent the graph as a list of ndoes and a list of edges. On the
-- bright side, we do not need to manipulate the graph functionally (or algebraically), we
-- need only keep it in memory as a mediator format.

data Node = Node
    {
      uid   :: Int,
      label :: String
    } deriving (Show)


data Edge a = Edge
    {
      source :: Int,   -- ^ uid of a Node object
      target :: Int,   -- ^ uid of a Node object
      weight :: a
    } deriving (Show)


data GeneralGraph a = Graph [Node] [Edge a] deriving (Show)


type Graph = GeneralGraph Float



---- Graph functions

-- Some basic utilities to work with graphs

-- |Returns the list of Nodes
nodes :: GeneralGraph a -> [Node]
nodes (Graph ns _) = ns


-- |Returns the list of Edges
edges :: GeneralGraph a -> [Edge a]
edges (Graph _ es) = es



---- Text helper functions

-- Some additions to Data.Text


-- |Deletes all occurrences of a Char from a Text.
deleteOccurrences :: T.Text -> Char -> T.Text
deleteOccurrences text del = T.filter (/=del) text

-- |Deletes all occurrences of every Char in <chars>
deleteAll :: T.Text -> String -> T.Text
deleteAll text chars = T.foldl deleteOccurrences text (T.pack chars)



---- Parser functions

-- These functions pre-process the input files and return a Text with only the graph
-- data.p For example, they strip away comments, unnecessary whitespace, etc.


-- |Parser for GML format.
parseGML :: T.Text -> T.Text
parseGML contents = T.strip $ ((T.splitOn (T.pack "graph") contents) !! 1)



---- Converter functions

-- These functions receive graphs as text, usually from the output of a parser function
-- and output a value of type Graph.


-- |Builds a graph from a file. -- <text> should be the output of parseGML
convertGML :: T.Text -> Graph
convertGML text = Graph (map convertNodeGML $ splitIntoNodesGML text) (map convertEdgeGML $ splitIntoEdgesGML text)


-- |Splits text into chunks on every occurrence of <str>.
splitIntoChunkGML :: String -> T.Text -> [T.Text]
splitIntoChunkGML str text = map cleanChunkGML $ tail $ T.splitOn (T.pack str) text


-- |Aliases for splitting into nodes and egdes
splitIntoNodesGML = splitIntoChunkGML "node"
splitIntoEdgesGML = splitIntoChunkGML "edge"


-- |Cleans up a chunk: removes some undesired chars and whitespace.
cleanChunkGML :: T.Text -> T.Text
cleanChunkGML text = T.strip . T.unlines . map T.strip $ T.lines (deleteAll text "[]")

-- |Gets a chunk of cleaned up text and returns a Node
convertNodeGML :: T.Text -> Node
convertNodeGML text = Node {uid=uid, label=lbl}
    where
      strings = T.lines text
      uid = read (T.unpack $ T.drop 3 (head strings)) :: Int
      lbl = T.unpack $ deleteOccurrences (T.drop 6 $ last strings) '"'


-- |Gets a chunk of cleaned up text and returns an Edge
convertEdgeGML :: T.Text -> Edge Float
convertEdgeGML text = Edge {source=src, target=tgt, weight=val}
    where
      strings = T.lines text
      src = read (T.unpack $ T.drop 7 (strings !! 0)) :: Int
      tgt = read (T.unpack $ T.drop 7 (strings !! 1)) :: Int
      val = read (T.unpack $ T.drop 6 (strings !! 2)) :: Float



---- General-purpose convert function

-- |This function receives the whole content of a file and just dispatches the right
-- specialized converter function.
convert :: T.Text -> Graph
convert contents = convertGML $ parseGML contents



---- Convert to Mathematica Graph[] format.

-- |Receives a Graph value and outputs a Text with a valid Mathematica representation of
-- the same graph.
toMath :: Graph -> T.Text
toMath (Graph ns es) = T.concat [T.pack "Graph[",
                                 mathNodes ns,
                                 T.pack ", ",
                                 mathEdges es,
                                 T.pack ", VertexLabels -> ",
                                 mathLabelRules ns,
                                 T.pack "]"
                                ]


-- |Receives a list of Nodes and outputs a valid Mathematica representation.
mathNodes :: [Node] -> T.Text
mathNodes ns = T.pack $ "{" ++ nodes ++ "}" where
    nodes = intercalate ", " $ map (show . uid) ns
    

-- |Receives a list of Edges and outputs a valid Mathematica representation.
mathEdges :: [Edge a] -> T.Text
mathEdges es = T.pack $ "{" ++ edges ++ "}" where
    edges = intercalate ", " $ map edgeAsStr es
    edgeAsStr e = "DirectedEdge[" ++ (show $ source e) ++ ", " ++ (show $ target e) ++ "]"


-- |Receives a list of Nodes and outputs a list of Mathematica rules assigning them their
-- respective labels.
mathLabelRules :: [Node] -> T.Text
mathLabelRules ns = T.pack $ "{" ++ labels ++ "}" where
    labels = intercalate ", " $ map labelRule ns
    labelRule n = show (uid n) ++ " -> " ++ show (label n)



---- Main
  
main = do
  args <- getArgs  
  let filename = head args
  let outfilename = (T.unpack $ head $ T.splitOn (T.pack ".") (T.pack filename)) ++ ".m"

  inHandle <- openFile filename ReadMode
  contents <- hGetContents inHandle
           
  let math = T.unpack $ toMath $ convert $ T.pack contents

  outHandle <- openFile outfilename WriteMode
  hPutStrLn outHandle math

  hClose outHandle
  hClose inHandle
