module Parser where

import Data.Char (isUpper, toLower)

import Types
import Parse

tok s = wsl *> s <* wsl
tsep s p = sepBy (tok s) p
spaceSep p = sepBy wsl p
spaceSep1 p = sepBy1 wsl p

dropComments :: String -> String
dropComments = unlines . concatMap fixLine . lines
  where
    fixLine ('#' : _) = []
    fixLine x = [x]

symbol_ = token
sym_ = char '\'' *> (token) -- TODO allow empty symbol ' ?
lit_ = (LInt <$> int_) <|> (LSym <$> Sym <$> sym_)

ref_ = char '#' *> (R <$> int_)

expr_ :: Parser Expr
expr_ = (ERef <$> ref_) <|> (ELit <$> lit_) <|> (ESym <$> symbol_)

--TODO remove
--poperation = pcount <|> pmax <|> pdrop <|> patom <|> (ONamed <$> onamed_)
--pnode =
--  (NSym <$> token) <|>
--  (NLit <$> lit_) <|>
--  (char '!' *> whitespace *> (NRoot <$> token)) <|>
--  (char '.' *> return NHole)
--patom, pcount, pmax, pdrop :: Parser Operation
--patom = OMatch <$> (Atom <$> (pnode <* ws) <*> (token <* ws) <*> pnode)
--pcount = string "count" *> ws *> (OCount <$> token)
--pmax = OMax <$> (Max <$> (token <* ws) <* (string "max" <* ws) <*> token)
--pdrop = string "drop" *> ws *> (ODrop <$> token)

pfnname = token <|> ((:[]) <$> anyc "+*-/><")

onamed_ = do
  char '@' <* wsl
  t <- pfnname <* wsl
  args <- spaceSep expr_
  return $ App t args

arrow_ :: Parser Arrow
arrow_ = Arrow <$> expr_ <* wsl <*> symbol_ <* wsl <*> expr_

pattern_ = tsep (char ',') peffect
lhs_ :: Parser [Effect]
lhs_ = pattern_
rhs_ :: Parser [Effect]
rhs_ = pattern_

peffect = pdel <|> passert <|> (ENamed <$> onamed_)
passert, pdel :: Parser Effect
passert = Assert <$> arrow_
pdel = string "del" *> wsl *> (Del <$> expr_)

prule :: Parser Rule
prule = do
  l <- lhs_
  r <- (tok (char '~') *> rhs_) <|> (whitespace *> return [])
  return (l, r)

psig :: Parser (Symbol, [Symbol])
psig = do
  name <- token <* ws
  params <- spaceSep token
  return (name, params)

pdef :: Parser (Symbol, (Pattern, [Symbol]))
pdef = do
  (name, ps) <- psig
  whitespace
  rule <- pattern_
  whitespace; char '.'; whitespace
  return (name, (rule, ps))

pfile :: Parser Program
pfile = do
  main <- prule
  defs <- many pdef
  return $ Prog defs main

parseFile :: String -> Either Error Program
parseFile str =
  case runParser pfile . dropComments $ str of
    Left e -> Left e
    Right (p, "") -> Right p
    Right (p, str) -> Left $ "! unparsed input:\n" ++ str ++ "\ngot:\n\n" ++ show p
