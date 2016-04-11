import Data.Map (Map)
import qualified Data.Map as M

import Types
import Run
import Parse
import Parser

main = chk

program2env :: Program -> Map Name (Pattern, [Name])
program2env p = foldr fix M.empty (bindings p)
  where
    fix (Binding name params pattern) = M.insert name (pattern, params)

printCommand env comm = do
  putStrLn ""
  print comm
  mapM_ print $ solve env comm emptyContext

chk = do
  f <- readFile "test.res1"
  g <- readFile "cat.web"
  case (parseFile f, parseGraph g) of
    (Right p, Right g) -> do
      let env = (program2env p, g)
      putStrLn "bindings:"
      print $ map bindingName (bindings p)
      putStrLn $ "commands to run: " ++ show (length (commands p))
      -- putStrLn "\ncommands:" >> mapM_ print (commands p)
      -- putStrLn "\ngraph:" >> print g
      putStrLn "\nCommand results:"
      mapM_ (printCommand env) (commands p)
    (Left err, _) -> ppErr err
    (_, Left err) -> ppErr err

ppErr :: SyntaxError -> IO ()
ppErr (Incomplete str) = putStrLn $ "Incomplete parse:\n\n" ++ str
ppErr (ParseError str) = putStrLn $ "Parse Error:\n" ++ str
