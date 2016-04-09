-- TODO
--  - dot chaining
--  - clean up this file
--
--  - fix/remove . , drop unbound names in rule
--    ? change to *
--
--  ? rule checking
--
--  typed relations?
--    control mutability
--    e.g. + is closed, free relations are open, could encode 1-1 or 1-many
--    for unique properties (1 to 1) new pair overwrites previous
--
--    or, relation can have a type conditioned on one of the sides
--    e.g. for some X, X pos * is single-valued
--
--  syntax:
--    no ref, only Sym
--
--    del ok
--    named ok
--
--    ? folds
--  concrete free context:
--    no Sym
--
--    del ok
--    named ok
--  fully reduced context:
--    no Sym
--
--    del as deletion
--    all names reduced
--
--  ? context
--    name -> value
--    to reduce Sym
--  ? rule context
--    name -> clauses
--    to reduce Named

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import Data.List (foldl', sortOn, maximumBy)
import Data.List
import Data.Function (on)
import Control.Arrow (second)

import Types
import Interpreter

import Parse (runParser)
import Parser

--import Rewrite
import Sub
import Nameless

import Graph


-- Interface
toGraph :: [(String, [(Int, Int)])] -> Graph
toGraph = arr2g . concatMap fix
  where
    fix (a, bs) = map (\(s,t) -> Arrow (ERef (R s)) a (ERef (R t))) bs

testGraph = toGraph $
  [ ("x",     [(0, 2), (1, 2)])
  , ("y",     [(2, 3), (2, 4)])
  , ("z",     [(3, 5)])
  , ("id",    [(0, 1), (1, 0), (2, 3)])
  , ("A",     [(-1, 6)])
  , ("B",     [(-1, 7)])
  , ("names", [(6, 0), (6, 3)
              ,(7, 0), (7, 1), (7, 2)])
  ]

smallGraph = toGraph $
  [ ("x", [(0, 2), (1, 2)]) ]

runRule :: Graph -> Rule -> [(Context, Graph)]
runRule web (ops, effs) = 
  let matches = foldl' (step web) [[]] ops
  in map (\c -> foldl' stepEff (c, web) effs) matches

runFirst :: Graph -> [Rule] -> Graph
runFirst web rules =
  case concatMap (runRule web) rules of
    [] -> web
    (_, w) : _ -> w

toMaybe (Right v) = Just v
toMaybe (Left _) = Nothing

run :: Graph -> String -> Maybe [(Context, Graph)]
run web prog = do
  --rule <- parseRule prog
  (rule', "") <- toMaybe $ runParser prule prog
  let rule = normalizeRule [] rule'
  return $ runRule web rule

showCtxt :: Context -> Context
showCtxt = reverse

tc p = do
  mweb <- loadGraph "test.web"
  case mweb of
    Right (web, "") -> testCase web p
    Left err -> putStrLn $ "error loading web:\n" ++ err

-- TODO print modifications
testCase web prog = do
  putStrLn $ "\n" ++ prog
  case run web prog of
    Just cs -> do
      mapM_ (\(c, _) -> print (showCtxt c)) $ cs
    _ -> putStrLn "error"

testEff prog = do
  putStrLn $ "\n" ++ prog
  case run smallGraph prog of
    Just cs -> mapM_ print cs
    _ -> error "error"

main0 = do
  let tests =
        [ "a x b, b y c, c z d"
        , "a id b, b id a"
        --, "repo names a, count a"
        --, "repo names a, count a, repo max a"
        --, "repo names a, count a, repo max a, repo names b, drop repo"
        --, "!repo names a"
        , "repo names a"

        , "'B names a, 'B names b, a size as, b size bs, @> bs as, @+ as bs sum"
        , "'A names a, 'B names b, a size s, b size s"
        ]

  mweb <- loadGraph "test.web"

  case mweb of
    Right (web, "") -> do
      mapM_ (testCase web) tests
    Left err -> putStrLn $ "error loading web:\n" ++ err

  -- unbound name 'c' creates new object
  testEff "a x b ~ c to a, c to 22"

-- whole file parsing
main = do
  f <- readFile "prog.res"
  case parseFile f of
    Right (Prog defs main) ->
      print $ normalizeRule defs main
    Left str -> putStrLn str
