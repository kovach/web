import Data.Map (Map)
import qualified Data.Map as M

import Types
import Run
import Parse
import Parser
import Graph

main = chk

program2env :: Program -> Map Name (Pattern, [Name])
program2env p = foldr fix M.empty (commands p)
  where
    fix (CBinding (Binding name params pattern)) =
      M.insert name (pattern, params)
    fix _ = id

--printCommand :: Env -> Command -> IO ()
--printCommand env (CBinding (Binding name _ _)) =
--  putStrLn $ "Binding " ++ name
--printCommand env (CQuery pattern) = do
--  putStrLn ""
--  print pattern
--  mapM_ print $ solve env pattern emptyContext

chk = do
  f <- readFile "category.res"
  g <- readFile "cat.web"
  case (parseFile f, parseGraph g) of
    (Right p, Right g) -> do
      let (msgs, s) = execProgram (Graph M.empty) p
      mapM_ print (commands p)
      putStrLn "~~~~~~~~~~~~~~~~~~~~~"
      mapM_ putStrLn msgs
      print $ s_graph s
    (Left err, _) -> ppErr err
    (_, Left err) -> ppErr err

ppErr :: SyntaxError -> IO ()
ppErr (Incomplete str) = putStrLn $ "Incomplete parse:\n\n" ++ str
ppErr (ParseError str) = putStrLn $ "Parse Error:\n" ++ str
