import Base
import qualified Data.Map as M
import qualified Syntax as S

emptyContext :: C
emptyContext = M.empty

data Mode = None | Unique | Assert | Remove | Must | All | AllC C
  deriving (Eq, Ord, Show)

data Frame = Branch Mode [Frame] | Leaf C | Done [C]
  deriving (Eq, Ord, Show)

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

freshFrame m (Leaf c) = Branch m [Leaf c]

emptyF (Branch _ []) = True
emptyF _ = False

--flatten = map fix
--  where fix (Branch (Must c) fs) = Leaf c
--toMust (Leaf c) = Branch (Must c) [Leaf c]

step :: Token Mode -> Frame -> Sign
step t b@(Branch m bs) =
  case mconcat (map (step t) bs) of
    Bounce -> case t of
      Close ->
        case m of
          None -> OK bs
          Unique -> if length bs == 1 then OK bs else OK []
          Must -> Bounce
          AllC c -> if any emptyF bs then OK [] else OK [Leaf c]
          All -> error "uninitialized All bug"
      Arrow ->
        -- TODO check mode?
        case m of
          AllC c -> OK $ [Branch (AllC c) (map (freshFrame Must) bs)]
          _ -> error "Arrow must appear inside universal frame"
    Fail -> error "shouldn't happen"
    OK bs' -> OK $ [Branch m bs']
step t l@(Leaf c) = case t of
  Open m ->
    case m of
      All -> OK $ [Branch (AllC c) [l]]
      _ -> OK $ [Branch m [l]]
  Bind n v -> OK $ [Leaf $ M.insert n v c]
  Split a b -> OK $ [Leaf (c`mappend`a), Leaf (c`mappend`b)]
  Eq m n -> if M.lookup m c == M.lookup n c then OK [l] else OK []
  Close -> Bounce
  Arrow -> Bounce

fromLeaf (Leaf x) = x

reduce f [] = [f]
reduce f (t:ts) =
  case step t f of
    OK [x] -> f : reduce x ts
    OK result -> [Done $ map fromLeaf result]
    x -> error $ "hey: " ++ show x

main =
  let f = Branch None [Leaf c0, Leaf c1]
      c0 = M.fromList [("a", A), ("b", B)]
      c1 = M.fromList [("a", A), ("b", A)]
      f0 = Leaf emptyContext
      f1 = Branch None [f0]

      p0 = []
      p1 = [Split c0 c1, Open All, Eq "a" "b", Arrow, Eq "a" "b", Close]

  in mapM_ print $ reduce f1 p1

