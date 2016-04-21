module Context where

import Data.Map (Map)
import qualified Data.Map as M

import Types

initial, empty :: Context
initial = []
empty = [M.empty]

walk :: Context -> SRef -> Either Name SRef
walk [] _ = error "empty context"
walk (c:cs) (SName name) =
  case M.lookup name c of
    Nothing          -> Left name
    Just n@(SName _) -> walk cs n
    Just r           -> Right r
walk _ x = Right x

walkOn :: Context -> SRef -> SRef -> (SRef, Context)
walkOn [] _ _ = error "empty context"
walkOn ctxt@(c:cs) (SName name) val =
  case M.lookup name c of
    Nothing -> (val, insert name val ctxt)
    Just n@(SName _) ->
      let (v, cs') = walkOn cs n val
      in (v, c : cs')
    Just r -> (r, ctxt)
walkOn c x _ = (x, c)

insert k v (c:cs) = M.insert k v c : cs
delete k (c:cs) = M.delete k c : cs
lookup k (c:_) = M.lookup k c
push cs = M.empty : cs
pop (c : cs) = cs
