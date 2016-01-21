-- modules for defining derived relations
module Extern where

import Control.Monad (foldM)

import Types

-- Marks whether parameter is 'input' or 'output'
-- cf mercury-lang
data Sign = N | P
  deriving (Show, Eq, Ord)

type Mode = [Sign]

type Binding = ([Value], [Symbol])
type Fn = Web -> Binding -> [Context]
type ModeFn = (Mode, Fn)
type Extern = [ModeFn]

data ExtBind = ExtBind Extern [(Symbol, Symbol)]

guard False = Nothing
guard _ = return ()

matchMode :: Context -> Mode -> [Expr] -> Maybe Binding
matchMode ctxt mode args = do
    guard (length mode == length args)
    let pairs = zip args mode
    fmap rev $ foldM step ([], []) pairs
  where
    rev (a, b) = (reverse a, reverse b)
    step :: Binding -> (Expr, Sign) -> Maybe Binding
    step (a, b) (ELit l, N) = return (VLit l : a, b)
    step (a, b) (ESym sym, N) = do
      v <- lookup sym ctxt
      return (v:a, b)
    step (a, b) (ESym sym, P) = return (a, sym:b)
    step (a, b) (_, P) = error "TODO"

matchExtern :: Web -> Context -> [Expr] -> Extern -> Maybe [Context]
matchExtern _ _ _ [] = Nothing
matchExtern w ctxt args ((mode, fn) : es) =
  case matchMode ctxt mode args of
    Just b -> Just $ map (++ ctxt) $ fn w b
    Nothing -> matchExtern w ctxt args es

-- Actual definitions
plusPNNfn, plusNPNfn, plusNNPfn :: Fn
plusNNPfn _ ([l, r], [out]) = [[(out, l+r)]]
plusNPNfn _ ([l, out], [r]) = [[(r, out-l)]]
plusPNNfn _ ([r, out], [l]) = [[(l, out-r)]]

plus :: Extern
plus = [([N,N,P], plusNNPfn)
       ,([N,P,N], plusNPNfn)
       ,([P,N,N], plusPNNfn)]

-- prelude
std_lib = [("plus", plus)]
