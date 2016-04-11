module Parser where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isUpper, toLower)

import Types
import Parse

dropComments :: String -> String
dropComments = unlines . map fixLine . lines
  where
    fixLine = takeWhile (/= '!')

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

pattern_ =
  token (char '[') *>
  (sepBy1 (token $ char ',') (clause_ <* flex))
  <* char ']'

binding_ = Binding
  <$> token name_ <*> many1 (token name_)
  <*> token pattern_

program_ = flex *> (Program
  <$> token (many (binding_ )) -- <* token (char '.')))
  <*> token (many (token pattern_)))

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
