module Parser where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isUpper, toLower)

import Types
import Parse
import Graph

dropComments :: String -> String
dropComments = unlines . map fixLine . lines
  where
    fixLine ('#' : _) = []
    fixLine (x:xs) = x : fixLine xs
    fixLine [] = []

-- ------ --
-- Syntax --
-- ------ --
name_ = identifier
rel_  = char '.' *> identifier

symbol_ = Symbol <$> (char '\'' *> identifier)
-- TODO move this?
ref_ = R <$> (char '#' *> int_)

lit_ = (LInt <$> int_) <|> (LSymbol <$> symbol_)

sub_ = error "sub not implemented"

sref_ =
  (Lit <$> lit_) <|>
  (Ref <$> ref_) <|>
  (SName <$> name_)
  -- (sub_)

arrow_ = Arrow <$> token sref_ <*> token rel_ <*> token sref_

application_ = do
  -- char '@'
  h <- token name_
  args <- many1 (token sref_)
  return $ App h args

clause_ =
  (Assert <$> arrow_)
  <|> (string "del" *> ws *> (Del <$> many (token sref_)))
  <|> (Named <$> application_)
  <|> (All <$> token pattern_ <*> token pattern_)
  <|> (SubPattern <$> token pattern_)

clauses_ =
  token (char '[') *>
  (sepBy (token $ char ',') (clause_ <* flex))
  <* char ']'

pattern_ =
  (Pattern <$> clauses_)
  <|> (UniquePattern <$> (char '!' *> clauses_))

assertPattern_ =
  (AssertPattern <$> (clauses_))

binding_ = Binding
  <$> token name_ <*> many1 (token name_)
  <*> token pattern_

command_ =
  (CBinding <$> binding_)
  <|> (CQuery <$> pattern_)
  <|> (token (char '~') *> (CAssert <$> token pattern_ <*> token assertPattern_))

program_ = flex *> (Program
  <$> token (many (token command_)))

-- TODO need better error reporting
data SyntaxError = ParseError String | Incomplete String
  deriving (Eq, Ord, Show)

parseFile :: String -> Either SyntaxError Program
parseFile file =
  case runParser program_ . dropComments $ file of
    Right (p, s) ->
      case s of
        "" -> return p
        rest -> Left (Incomplete rest)
    Left err -> Left (ParseError err)


-- ----- --
-- Graph --
-- ----- --

graph_ = do
    rows <- many (token arrow_)
    return $ Graph { edges = index rows }
  where
    index = foldr step M.empty
    step (Arrow s p t) m = M.insertWith (++) p [(s,t)] m

parseGraph :: String -> Either SyntaxError Graph
parseGraph file =
  case runParser graph_ . dropComments $ file of
    Right (p, s) ->
      case s of
        "" -> return p
        rest -> Left (Incomplete rest)
    Left err -> Left (ParseError err)
