-- TODO
-- (for all)
--  - dot chaining
--  - clean up this file, make better webs
--
--  - fix/remove . , drop unbound names in rule
--    ? change to *
--
--  - rule checking
--    ?
--
-- (for games)
--  - need named rhs rules
--
-- (for notes)
--   - rules for editing a blob
--   - rules for committing a blob
--
--  - skein
--
--  'linear' (?) rule can be asserted on RHS to create edges?

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

import Rewrite

import Web

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
  (rule', "") <- toMaybe $ runParser prule' prog
  let rule = normalize [] rule'
  return $ runRule web rule

showCtxt :: Context -> Context
showCtxt = reverse

tc p = do
  mweb <- loadWeb "test.web"
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
  case run smallWeb prog of
    Just cs -> mapM_ print cs
    _ -> error "error"

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

      p9 = "'B names a, 'B names b, a size as, b size bs, @> bs as, @+ as bs sum"
      p10 = "'A names a, 'B names b, a size s, b size s"

  mweb <- loadWeb "test.web"
  case mweb of
    Right (web, "") -> do
      testCase web p1
      --testCase p2
      testCase web p5
      testCase web p6
      testCase web p7
      --testCase p8'
      testCase web p8
      testCase web p9
      testCase web p10
    Left err -> putStrLn $ "error loading web:\n" ++ err

  testEff "a x b ~ new c, c to a, c to 22"

-- whole file parsing
chk = do
  f <- readFile "prog.res"
  case parseFile f of
    Right (Prog defs main) ->
      print $ normalize defs main
    Left str -> putStrLn str
