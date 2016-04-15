-- gtoa.hs
-- Utility to convert between different grahp file formats

-- Thanks to lazy evaluation, we can read huge files and process them without taking all
-- available memory.


import System.IO
import Data.List
import qualified Data.Text as Text



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
      source :: Node,
      target :: Node,
      weight :: a
    } deriving (Show)


data GeneralGraph a = Graph [Node] [Edge a] deriving (Show)


type Graph = GeneralGraph Float



---- Graph functions

-- Some basic utilities to work with graphs

nodes :: GeneralGraph a -> [Node]
nodes (Graph ns _) = ns

edges :: GeneralGraph a -> [Edge a]
edges (Graph _ es) = es



---- Parser functions

-- These functions pre-process the input files and return a Text with only the graph
-- data.p For example, they strip away comments, unnecessary whitespace, etc.


parseGML :: Text.Text -> Text.Text
parseGML contents = Text.strip $ (Text.splitOn (Text.pack "graph") contents) !! 1



---- Converter functions

-- These functions receive graphs as text, usually from the output of a parser function
-- and output a value of type Graph.


convertGML :: Text.Text -> Graph
convertGML text = makeGraph $ map convertNodeGML $ splitIntoNodesGML text


splitIntoNodesGML :: Text.Text -> [Text.Text]
splitIntoNodesGML text = map cleanNodeGML $ tail $ Text.splitOn (Text.pack "node") text


cleanNodeGML :: Text.Text -> Text.Text
cleanNodeGML text = Text.strip $ Text.unlines $ map cleanLine (Text.lines text)
    where
      cleanLine line = Text.strip $ Text.foldl deleteAll line (Text.pack "[]")


deleteAll :: Text.Text -> Char -> Text.Text
deleteAll text del = Text.filter (/=del) text

                         
convertNodeGML :: Text.Text -> Node
convertNodeGML text = Node {uid=uid, label=lbl}
    where
      strings = Text.lines text
      uid = read (Text.unpack $ Text.drop 3 (head strings)) :: Int
      lbl = Text.unpack $ deleteAll (Text.drop 6 $ last strings) '"'



---- Graph building functions


makeGraph :: [Node] -> Graph
makeGraph nodes = Graph nodes []



---- convert function

-- This function receives the whole content of a file and just dispatches the right
-- converter function.


convert :: Text.Text -> Graph
convert contents = convertGML $ parseGML contents



---- Main

-- Read data and format types from command line

  
main = do
  handle <- openFile "celegansneural.gml" ReadMode
  contents <- hGetContents handle

  putStrLn $ show $ convert $ Text.pack contents

  hClose handle
