module Graph where

import Types
import Parse
import Parser
import qualified Data.Map as M (fromList, toList, empty, insertWith)
import Data.List (foldl')

emptyGraph = Graph M.empty

-- sets count to be larger than any Ref present in web
graphCount :: Graph -> Int
graphCount (Graph edges) = 1 + maximum (concatMap to pairs)
  where
    pairs = concat . map snd $ (M.toList edges)
    to (a, b) = to' a ++ to' b
    to' (ERef (R i)) = [i]
    to' _ = []

webFile_ :: Parser Graph
webFile_ = do
    rows <- sepBy (char '\n') arrow_
    ws
    return $ arr2g rows

arr2g :: [Arrow] -> Graph
arr2g xs = Graph $ foldl' step M.empty xs
  where
    step m (Arrow s p t) = M.insertWith (++) p [(s, t)] m


loadGraph filename = do
  f <- readFile filename
  return $ runParser webFile_ f


g2arr :: Graph -> [Arrow]
g2arr (Graph emap) = concatMap fixR . M.toList $ emap
  where
    fix p (a, b) = Arrow a p b
    fixR (a, bs) = map (fix a) bs
