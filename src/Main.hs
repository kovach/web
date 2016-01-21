-- TODO
-- (for all)
--  - arithmetic
--  - dot chaining
--  - symbols
--
--  - serialize/parse web
--  - fix . , drop unbound names in rule
--
--  - rule checking
--
--  - need to mark rule arguments with signs?
--    - special case arithmetic for now
--      - (must have two arguments already bound)
--
-- (for games)
--  - need named rhs rules
--
-- (for notes)
--   - rules for editing a blob
--   - rules for committing a blob
--
--  - unary predicates?
--  - skein
--
-- idea: more specific rule ~ smaller pattern graph
--   count nodes or just edges? count binders

module Main where

import qualified Data.Map as M
import Data.List (foldl', sortOn, maximumBy)
import Data.List
import Data.Function (on)
import Control.Arrow (second)

import Types
import Web
import Interpreter

import Parse
import Parser

import Rewrite

-- Interface
toWeb :: Int -> [(String, [(Int, Int)])] -> Web
toWeb c = Web c . M.fromList
          . map (second (map (\(a, b) -> (VRef $ R a, VRef $ R b))))

testWeb = toWeb 8 $
  [ ("x",     [(0, 2), (1, 2)])
  , ("y",     [(2, 3), (2, 4)])
  , ("z",     [(3, 5)])
  , ("id",    [(0, 1), (1, 0), (2, 3)])
  , ("A",     [(-1, 6)])
  , ("B",     [(-1, 7)])
  , ("names", [(6, 0), (6, 3)
              ,(7, 0), (7, 1), (7, 2)])
  ]

smallWeb = toWeb 3 $
  [ ("x", [(0, 2), (1, 2)])
  ]

-- 0 is the element of a unit object
emptyWeb = Web 1 M.empty

runRule :: Web -> Rule -> [(Context, Web)]
runRule web (ops, effs) = 
  let matches = foldl' (step web) [[]] ops
  in map (\c -> foldl' stepEff (c, web) effs) matches

runFirst :: Web -> [Rule] -> Web
runFirst web rules =
  case concatMap (runRule web) rules of
    [] -> web
    (_, w) : _ -> w

toMaybe (Right v) = Just v
toMaybe (Left _) = Nothing

run :: Web -> String -> Maybe [(Context, Web)]
run web prog = do
  --rule <- parseRule prog
  (rule, "") <- toMaybe $ runParser prule prog
  return $ runRule web rule

showCtxt :: Context -> Context
showCtxt = reverse

-- TODO print modifications
testCase prog = do
  let web = testWeb
  putStrLn $ "\n" ++ prog
  case run web prog of
    Just cs -> do
      mapM_ (\(c, _) -> print (showCtxt c)) $ cs
    _ -> putStrLn "error"

testEff prog = do
  putStrLn $ "\n" ++ prog
  chk' prog

runProg prog =
  case run smallWeb prog of
    Just cs -> cs
    _ -> error "error"

chk' = mapM_ print . runProg
main = do
  let p1 = "a x b, b y c, c z d"
      p2 = "a id b, b id a"
      p5 = "repo names a, count a"
      -- "repo with most elements"
      p6 = "repo names a, count a, repo max a"
      -- "elements of largest repo"
      p7 = "repo names a, count a, repo max a, repo names b, drop repo"
      p8 = "!repo names a"
      p8' = "repo names a"

  testCase p1
  --testCase p2
  testCase p5
  testCase p6
  testCase p7
  --testCase p8'
  testCase p8

  testEff "a x b ~ new c, c to a, c to 22"

-- whole file parsing
chk = do
  f <- readFile "prog.cog"
  case parseFile f of
    Right (Prog defs main) ->
      print $ normalize defs main
    Left str -> putStrLn str
