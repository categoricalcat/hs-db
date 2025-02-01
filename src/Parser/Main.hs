module Parser.Main where

import Control.Applicative (Alternative (..))
import Data.Char (isLowerCase, isNumber, isSpace, isUpperCase)
import Log
import Task

data ParsedData a = Parsed a String | Failed String deriving (Show)

type Parsed a = Task String (ParsedData a)

newtype Parser a = Parser
  { runParser :: String -> TraceString -> Parsed a
  }

-- to monad maybe?
toParsed :: String -> Parsed String
toParsed input = Task [] (Just $ Parsed input "")

logEndOfInput :: LogString
logEndOfInput = Log LogInfo "end_of_input"

logCannotParseNothing :: LogString
logCannotParseNothing = Log LogError "cannot_parse_nothing"

logParseChar :: Char -> LogString
logParseChar c = Log LogInfo $ "parse_char_" ++ [c]

logParseSatisfyFailed :: Char -> LogString
logParseSatisfyFailed c = Log LogWarning $ "parse_satisfy_failed_" ++ [c]

parse :: Parser Char
parse = Parser $ \input (Trace ts) -> case input of
  (c : cs) -> Task (logParseChar c : ts) (Just $ Parsed c cs)
  [] -> Task (logEndOfInput : ts) (Just $ Failed "end_of_input")

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- parse
  if (p c)
    then
      return c
    else Parser $ \a (Trace ts) ->
      Task (logParseSatisfyFailed c : ts) (Just $ Failed a)

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
spaces = many (satisfy isSpace)

validEnvChar :: Parser Char
validEnvChar = alphaNum <|> char '_' <|> char '-' <|> char '.'

comment :: Parser String
comment = do
  _ <- char '#'
  _ <- many (satisfy (/= '\n'))
  return ""

envKeyValues :: Parser [(String, String)]
envKeyValues =
  many $
    (,)
      <$> (spaces *> many validEnvChar <* spaces <* char '=' <* spaces)
      <*> many validEnvChar

instance Functor ParsedData where
  fmap :: (a -> b) -> ParsedData a -> ParsedData b
  fmap f (Parsed a rest) = Parsed (f a) rest
  fmap _ (Failed a) = Failed a

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ts -> case runParser p input ts of
    Task logs ma -> Task logs (fmap (fmap f) ma)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input (Trace ts) ->
    Task ts (Just $ Parsed a input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser $ \input ts -> case runParser pf input ts of
    Task logs mf -> case mf of
      Just (Parsed f rest) -> runParser (fmap f pa) rest ts
      _ -> Task logs (Just $ Failed input)

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = Parser $ \input ts -> case runParser pa input ts of
    Task logs ma -> case ma of
      Just (Parsed a rest) -> runParser (fpb a) rest ts
      _ -> Task logs (Just $ Failed input)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \i (Trace ts) -> Task (logEndOfInput : ts) (Just $ Failed i)

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ts ->
    case runParser p1 input ts of
      Task logs ma -> case ma of
        Just (Parsed a rest) -> Task logs (Just $ Parsed a rest)
        _ -> runParser p2 input ts
