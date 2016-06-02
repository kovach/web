import Types

import qualified Data.Map as M
import qualified Syntax as S

emptyContext :: C
emptyContext = M.empty

data Sign = OK [Frame] | Bounce
          | Fail -- not currently used
  deriving (Eq, Ord, Show)

instance Monoid Sign where
  OK as `mappend` OK bs = OK (as++bs)
  Fail `mappend` _ = Fail
  _ `mappend` Fail = Fail
  Bounce `mappend` _ = Bounce
  _ `mappend` Bounce = Bounce
  --mappend a b = error $ "mappend error: " ++ show a ++ ", " ++ show b
  mempty = OK []

freshFrame m (Leaf c) = Branch c m [Leaf c]

emptyF (Branch _ _ []) = True
emptyF _ = False

step :: Token -> Frame -> Sign
step t b@(Branch c m bs) =
  case mconcat (map (step t) bs) of
    Bounce -> case t of
      CloseFrame ->
        case m of
          MM None -> OK bs
          MM Unique -> if length bs == 1 then OK bs else OK []
          Must -> Bounce
          All -> if any emptyF bs then OK [] else OK [Leaf c]
      Implies ->
        -- TODO check mode?
        case m of
          _ -> OK $ [Branch c All (map (freshFrame Must) bs)]
          -- _ -> error "Arrow must appear inside universal frame"
    Fail -> error "shouldn't happen"
    OK bs' -> OK $ [Branch c m bs']
step t l@(Leaf c) = case t of
  OpenFrame m ->
    case m of
      _ -> OK $ [Branch c (MM m) [l]]
  --Bind n v -> OK $ [Leaf $ M.insert n v c]
  --Split a b -> OK $ [Leaf (c`mappend`a), Leaf (c`mappend`b)]
  --Eq m n -> if M.lookup m c == M.lookup n c then OK [l] else OK []
  CloseFrame -> Bounce
  Implies -> Bounce

fromLeaf (Leaf x) = x

reduce f [] = [f]
reduce f (t:ts) =
  case step t f of
    OK [x] -> f : reduce x ts
    OK result -> [Done $ map fromLeaf result]
    x -> error $ "hey: " ++ show x

main =
  let f = Branch emptyContext (MM None) [Leaf c0, Leaf c1]
      c0 = M.fromList [("a", A), ("b", B)]
      c1 = M.fromList [("a", A), ("b", A)]
      f0 = Leaf emptyContext
      f1 = Branch emptyContext (MM None) [f0]

      p0 = ""
      --p1 = [Split c0 c1, Open All, Eq "a" "b", Arrow, Eq "a" "b", Close]
      p1 = "[ -> ]"
      ex0 = [p0, p1]
      ex1 = map S.tokens ex0

      doex e = do
        mapM_ print $ reduce f0 e -- f1?
        putStrLn "------------"

  in mapM_ doex ex1

