module Web where

import Types
import Parse
import Parser
import qualified Data.Map as M (fromList, toList)

arr_ = token

ref_ = char '#' *> (R <$> int_)
value_ :: Parser Value
value_ = (VRef <$> ref_) <|> (VLit <$> lit_)

edge_ :: Parser Edge
edge_ = (,) <$> value_ <* ws <*> value_

webRow_ :: Parser (Symbol, [Edge])
webRow_ = (,) <$> arr_ <* ws <*> sepBy1 ws edge_

-- sets count to be larger than any Ref present in web
updateCount :: Web -> Web
updateCount (Web count edges) = Web (1 + maximum (concatMap to pairs)) edges
  where
    pairs = concat . map snd $ (M.toList edges)
    to (a, b) = to' a ++ to' b
    to' (VRef (R i)) = [i]
    to' _ = []

webFile_ :: Parser Web
webFile_ = do
  rows <- sepBy (char '\n') webRow_
  ws
  let web = Web 0 (M.fromList rows)
  return $ updateCount web


loadWeb filename = do
  f <- readFile filename
  return $ runParser webFile_ f
