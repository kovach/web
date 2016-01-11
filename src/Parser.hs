module Parser where

import Data.Char (isUpper, toLower)

import Types
import Parse
import Check

tok s = wsl *> s <* wsl
tsep s p = sepBy (tok s) p
spaceSep p = sepBy wsl p

ppattern :: Parser [Operation]
ppattern = tsep (char ',') poperation
poperation = pcount <|> pmax <|> pdrop <|> patom 
pnode =
  (NSym <$> token) <|>
  (char '!' *> whitespace *> (NRoot <$> token)) <|>
  (char '.' *> return NHole)
patom, pcount, pmax, pdrop :: Parser Operation
patom = OMatch <$> (P <$> (pnode <* ws) <*> (token <* ws) <*> pnode)
pcount = string "count" *> ws *> (OCount <$> token)
pmax = OMax <$> (Max <$> (token <* ws) <* (string "max" <* ws) <*> token)
pdrop = string "drop" *> ws *> (ODrop <$> token)

poperation' :: Parser Operation'
poperation' = (ONamed <$> pnamed) <|> (OOperation <$> poperation)
pnamed = do
  t <- token <* wsl
  assert (isUpper (head t)) "Expecting upper char"
  args <- spaceSep token
  return $ App (map toLower t) args

prhs :: Parser [Effect]
prhs = tsep (char ',') peffect
peffect = pfresh <|> pdel <|> passert
pfresh, passert, pdel :: Parser Effect
pfresh = string "new" *> ws *> (EFresh <$> token)
passert = EAssert <$> (token <* ws) <*> (token <* ws) <*> token
pdel = string "del" *> ws *> (EDel <$> token)

prule :: Parser Rule
prule = do
  l <- ppattern
  r <- (tok (char '~') *> prhs) <|> (whitespace *> return [])
  return (l, r)

prule' :: Parser Rule'
prule' = do
  l <- tsep (char ',') poperation'
  r <- (tok (char '~') *> prhs) <|> (whitespace *> return [])
  return (l, r)

psig :: Parser (Symbol, [Symbol])
psig = do
  name <- token <* ws
  params <- spaceSep token
  return (name, params)

pdef :: Parser (Symbol, (Rule', [Symbol]))
pdef = do
  (name, ps) <- psig
  whitespace
  rule <- prule'
  return (name, (rule, ps))

pfile :: Parser Program
pfile = do
  main <- prule'
  defs <- many pdef
  return $ Prog defs main
