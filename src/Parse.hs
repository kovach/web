-- TODO combined pretty-printing?
module Parse where

import Control.Arrow (first)
import Data.List (foldl')

import Types

data ParseEither str e a =
  ParseEither { runParser :: str -> Either e (a, str) }

type Error = String
type Parser a = ParseEither String Error a

instance Functor (ParseEither str e) where
  fmap f p = ParseEither $ (fmap $ first f) . runParser p

instance Applicative (ParseEither str e) where
  pure x = ParseEither (\s -> return (x, s))
  f <*> x = ParseEither $ \s0 -> do
    (f', s1) <- runParser f s0
    (x', s2) <- runParser x s1
    return $ (f' x', s2)

instance Monad (ParseEither str e) where
  return = pure
  m >>= f = ParseEither $ \s0 -> do
    (val, s1) <- runParser m s0
    runParser (f val) s1

-- full backtracking
(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = ParseEither $ \s ->
  case runParser a s of
    Left _ -> runParser b s
    Right v -> return v

ch' c (c' : cs) | c == c' = return (c, cs)
ch' c _ = Left $ "expected char: " ++ [c]

char :: Char -> Parser Char
char c = ParseEither (ch' c)

string :: String -> Parser String
string = mapM char

many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> return []
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

failure :: String -> Parser a
failure str = ParseEither (\s -> Left str)

anyc :: String -> Parser Char
anyc str = foldr (<|>) (failure $ "expected one of: " ++ str) (map char str)

alpha :: Parser Char
alpha = anyc $ ['a'..'z'] ++ ['A'..'Z']

num :: Parser Char
num = anyc $ ['0'..'9']

token :: Parser String
token = (:) <$> alpha <*> many (num <|> alpha <|> anyc "-_")

whitespace, whitespace1 :: Parser ()
whitespace = return () <* (many $ anyc " \t\n")
whitespace1 = return () <* (many1 $ anyc " \t\n")
ws = whitespace1
wsl = many $ anyc " \t"

sepBy, sepBy1 :: Parser s -> Parser a -> Parser [a]
sepBy1 sep p =
   (:) <$> p <*> ((sep *> sepBy1 sep p)  <|> return [])
sepBy sep p = sepBy1 sep p <|> return []

assert :: Bool -> String -> Parser ()
assert bool str =
  if bool then return () else ParseEither $ \_ -> Left str

int :: Parser Int
int = ParseEither (list2either "expected integer" . reads)

list2either :: e -> [a] -> Either e a
list2either e [] = Left e
list2either _ (x : _) = Right x
