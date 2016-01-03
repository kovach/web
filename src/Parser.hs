module Parser where

import Types
import Parse

tok s = whitespace *> s <* whitespace
tsep s p = sepBy (tok s) p

ppattern :: Parser [Operation]
ppattern = tsep (char ',') pclause
pclause = pcount <|> pmax <|> pdrop <|> patom 
pnode =
  (NSym <$> token) <|>
  (char '!' *> whitespace *> (NRoot <$> token)) <|>
  (char '.' *> return NHole)
patom, pcount, pmax, pdrop :: Parser Operation
patom = OMatch <$> (P <$> (pnode <* ws) <*> (token <* ws) <*> pnode)
pcount = string "count" *> ws *> (OCount <$> token)
pmax = OMax <$> (Max <$> (token <* ws) <* (string "max" <* ws) <*> token)
pdrop = string "drop" *> ws *> (ODrop <$> token)

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
