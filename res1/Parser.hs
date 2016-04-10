module Parser where

import Data.Char (isUpper, toLower)

import Types
import Parse

dropComments :: String -> String
dropComments = unlines . map fixLine . lines
  where
    fixLine = takeWhile (/= '!')

name_ = identifier
rel_  = identifier

symbol_ = Symbol <$> identifier
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
  char '@'
  h <- token name_
  args <- many1 (token sref_)
  return $ App h args

clause_ =
  (Assert <$> arrow_)
  <|> (string "del" *> ws *> (Del <$> sref_))
  <|> (Named <$> application_)
  <|> (All <$> token pattern_ <*> token pattern_)

pattern_ =
  token (char '[') *>
  (sepBy1 (token $ char ',') (clause_ <* flex))
  <* char ']'

chk s =
  case runParser pattern_ s of
    Left e -> putStrLn e
    Right (result, "") -> mapM_ print result

-- next:
--   parse graph
--   parse file with definitions
