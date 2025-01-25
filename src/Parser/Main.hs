module Parser.Main where

import Control.Applicative (Alternative (..))
import Data.Char (isLowerCase, isNumber, isSpace, isUpperCase)
import Debug.Trace (trace)

data ParserLog = ParserLog String
  deriving (Show, Eq)

type Trace = [ParserLog]

type Parsed a = (Maybe a, String, Trace)

newtype Parser a = Parser
  { runParser :: String -> Trace -> Parsed a
  }

-- to monad maybe?
toParsed :: String -> Parsed String
toParsed input = (Just input, "", [])

logEndOfInput :: ParserLog
logEndOfInput = ParserLog "end_of_input"

logCannotParseNothing :: ParserLog
logCannotParseNothing = ParserLog "cannot_parse_nothing"

logParseChar :: Char -> ParserLog
logParseChar c = ParserLog $ "parse_char_" ++ [c]

logParseSatisfyFailed :: Char -> ParserLog
logParseSatisfyFailed c = ParserLog $ "parse_satisfy_failed_" ++ [c]

parse :: Parser Char
parse = Parser $ \input ts -> case input of
  (c : cs) -> (Just c, cs, logParseChar c : ts)
  [] -> (Nothing, input, logEndOfInput : ts)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- parse
  if (p c)
    then return c
    else Parser $ \a ts -> (Nothing, a, logParseSatisfyFailed c : ts)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

digit :: Parser Char
digit = satisfy isNumber

lowerChar :: Parser Char
lowerChar = satisfy isLowerCase

upperChar :: Parser Char
upperChar = satisfy isUpperCase

letter :: Parser Char
letter = lowerChar <|> upperChar

alphaNum :: Parser Char
alphaNum = letter <|> digit

spaces :: Parser String
spaces = many (satisfy (`elem` " \t\n"))

validEnvChar :: Parser Char
validEnvChar = alphaNum <|> char '_' <|> char '-' <|> char '.'

comment :: Parser String
comment = do
  _ <- char '#'
  _ <- many (satisfy (/= '\n'))
  return ""

envKeyValues :: Parser [(String, String)]
envKeyValues = many $ do
  _ <- comment
  _ <- spaces
  ks <- many validEnvChar
  _ <- spaces
  _ <- char '='
  _ <- spaces
  vs <- many validEnvChar
  return $ (ks, vs)

-- INSTANCES
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ts -> case runParser p input ts of
    (Just a, rest, ts') -> (Just (f a), rest, ts')
    (Nothing, rest, ts') -> (Nothing, rest, ts')

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input ts -> (Just a, input, ts)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser $ \input ts -> case runParser pf input ts of
    (Just f, rest, ts') -> runParser (fmap f pa) rest ts'
    (Nothing, rest, ts') -> (Nothing, rest, ts')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = Parser $ \input ts -> case runParser pa input ts of
    (Just a, rest, ts') -> runParser (fpb a) rest ts'
    (Nothing, rest, ts') -> (Nothing, rest, ts')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ ts -> (Nothing, [], logEndOfInput : ts)

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ts ->
    case runParser p1 input ts of
      (Just a, rest, ts') -> (Just a, rest, ts')
      (Nothing, _, _) -> runParser p2 input ts
