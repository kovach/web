-- todo
-- mutation / rule RHS
-- validate queries
module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Data.Either (partitionEithers)
import Data.List (foldl', sortOn, maximumBy)
import Data.List
import Data.Function (on)
import Control.Arrow (second)

import Interpreter

-- Interface
toWeb :: Int -> [(String, [(Int, Int)])] -> Web
toWeb c = Web c . M.fromList . map (second (map (\(a, b) -> (R a, R b))))

testWeb = toWeb 8 $
  [ ("x", [(0, 2), (1, 2)])
  , ("y", [(2, 3), (2, 4)])
  , ("z", [(3, 5)])
  , ("id", [(0, 1), (1, 0), (2, 3)])
  , ("A", [(-1, 6)])
  , ("B", [(-1, 7)])
  , ("names", [ (6, 0), (6, 3)
              , (7, 0), (7, 1), (7, 2)])
  ]

smallWeb = toWeb 3 $
  [ ("x", [(0, 2), (1, 2)])
  ]

mp a@(Just _) _ = a
mp _ a@(Just _) = a
mp _ _ = Nothing

-- TODO use a parser
parseP :: String -> Maybe Pattern
parseP str | [".", p, "."] <- words str = Just $ P NHole p NHole
parseP str | [".", p, t] <- words str = Just $ P NHole p (NSym t)
parseP str | [s, p, "."] <- words str = Just $ P (NSym s) p NHole
parseP str | [s, p, t] <- words str = Just $ P (NSym s) p (NSym t)
parseP _ = Nothing

parseF :: String -> Maybe Fold
parseF s  | ["fold", p] <- words s = Just (Fold p)
parseF _ = Nothing

parseC :: String -> Maybe Symbol
parseC s  | ["count", p] <- words s = Just p
parseC _ = Nothing

parseM :: String -> Maybe Max
parseM s | [base, "max", v] <- words s = Just (Max base v)
parseM _ = Nothing

parseD :: String -> Maybe Symbol
parseD s | ["drop", v] <- words s = Just v
parseD _ = Nothing

parseOp :: String -> Maybe Operation
parseOp s =
  -- `mp` (OF <$> parseF s)
  (OC <$> parseC s)
  `mp` (OM <$> parseM s)
  `mp` (OD <$> parseD s)
  `mp` (OP <$> parseP s)

parseDel s | ["del", v] <- words s = Just (EDel v)
parseDel _ = Nothing

parseEdge s | [s, p, t] <- words s = Just (EAssert s p t)
parseEdge _ = Nothing

parseFresh s | ["new", o] <- words s = Just (EFresh o)
parseFresh _ = Nothing

parseEff :: String -> Maybe Effect
parseEff s =
  (parseFresh s)
  `mp` (parseEdge s)
  `mp` (parseDel s)

split = lines . map fix
  where
    fix ',' = '\n'
    fix x = x

parseSub :: String -> Maybe ([Operation], [Effect])
parseSub s =
  let (os, es) = span (/= '~') s
      es' = if null es then es else tail es
  in do
    ops <- mapM parseOp $ split os
    effs <- mapM parseEff $ split es'
    return (ops, effs)

run :: String -> Maybe [(Context, Web)]
run prog = do
  (ops, effs) <- parseSub prog
  let web = smallWeb
  let cs = foldl' (step web) [[]] ops
  return $ map (\c -> foldl' stepEff (c, web) effs) cs

showCtxt :: Context -> Context
showCtxt = sortOn fst

chk prog = do
  putStrLn $ "\n" ++ prog
  case run prog of
    Just cs -> do
      mapM_ (\(c, w) -> print (showCtxt c) >> print w ) $ cs
    _ -> putStrLn "error"

main = do
  let p1 = "a x b, b y c, c z d"
      p2 = "a id b, b id a"
      p5 = "repo names a, count a"
      -- "repo with most elements"
      p6 = "repo names a, count a, repo max a"
      -- "elements of largest repo"
      p7 = "repo names a, count a, repo max a, repo names b, drop repo"

  chk p1
  chk p2
  chk p5
  chk p6
  chk p7
