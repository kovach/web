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
  print $ solve env comm emptyContext

chk = do
  g <- dropComments <$> readFile "cat.web"
  f <- dropComments <$> readFile "test.res1"
  case (runParser program_ f, runParser graph_ g) of
    (Right (p, ""), Right (g, "")) -> do
      let env = (program2env p, g)
      putStrLn "bindings:" >> mapM_ print (bindings p)
      putStrLn "\ncommands:" >> mapM_ print (commands p)
      putStrLn "\ngraph:" >> print g
      putStrLn "\nresults:"
      mapM_ (printCommand env) (commands p)
    (Right (_, str), _) -> putStrLn $ "Unparsed input:\n" ++ str
    (Left err, _) -> putStrLn $ "Program parse error:\n" ++ err
    --(_, Left err) -> putStrLn $ "Graph parse error:\n" ++ err
