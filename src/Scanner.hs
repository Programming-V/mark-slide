module Scanner
    ( scanTokens
    ) where

import Token
import Data.Char (isAlphaNum)
import Utils 

scanTokens :: Input -> [Token]
scanTokens xs = scanTokens' xs 1 1

scanTokens' :: Input -> Line -> Col -> [Token]
scanTokens' [] _ _ = []
scanTokens' ('#' : xs) l c =
  let (count, rest) = span (== '#') xs
      keyword = replicate (length count + 1) '#'
   in Token Keyword keyword l c : scanTokens' rest l (c + length count + 1)
scanTokens' content@(x:xs) l c
  | x == ' ' = scanTokens' xs l (c + 1)
  | x == '\n' = scanTokens' xs (l + 1) 1
  | x == ';' = scanTokens' (dropWhile (/='\n') content) l c
  | x == '!' = Token Keyword [x] l c : scanTokens' xs l (c + 1)
  | x == '{' = Token OpenBlock [x] l c : scanTokens' xs (l + 1) c
  | x == '}' = Token EndBlock [x] l c : scanTokens' xs (l + 1) c
  --   | x == '-' = Token Keyword [x] l c : scanTokens' xs l (c + 1)
  | x == '-' && head xs == '-' && head (tail xs) == '-' = Token EndSlide "---" l c : scanTokens' (drop 2 xs) l (c + 2)
  | x == '_' = parseUnderscores xs l c
  | x == '*' = parseAsterisks xs l c 
  | x == '~' = parseStrikethrough xs l c
--   | x == '>' =
--         let (toks,rest) = span (/= '>') xs
--             newBlockquotes x = x == '>'
--             in
--                 Token Keyword (processString newBlockquotes (x:xs)) l c : scanTokens' (dropWhile newBlockquotes xs) l (c + 1)
  | isAlphaNum x =
      let (word, rest) = span isAlphaNumOrSpace xs
       in Token String (x : word) l c : scanTokens' rest l (c + length word + 1)
  | otherwise = Token Error [x] l c : scanTokens' xs l (c + 1)
  where
    isAlphaNumOrSpace = (`elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ']))

parseSpecial :: Char -> Input -> Line -> Col -> [Token]
parseSpecial symbol content l c
  | length formatted >= 2 && head content == symbol && last formatted == symbol = 
      Token Keyword [symbol] l c : Token String (init $ tail formatted) l (c + 1) : Token Keyword [symbol] l (c + length formatted) : scanTokens' (drop (length formatted) content) l (c + length formatted)
  | otherwise = Token Keyword [symbol] l c : scanTokens' content l c
  where
    formatted = takeWhile (`elem` (symbol : ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' '])) content

parseUnderscores :: Input -> Line -> Col -> [Token]
parseUnderscores = parseSpecial '_'

parseAsterisks :: Input -> Line -> Col -> [Token]
parseAsterisks = parseSpecial '*'

parseStrikethrough :: Input -> Line -> Col -> [Token]
parseStrikethrough = parseSpecial '~'

processString :: (Char -> Bool) -> String -> String
processString _ [] = ""
processString f (x:xs)
    | f x = x : processString f xs
    | otherwise = ""
