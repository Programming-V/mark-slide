module Utils 
    ( isWhiteSpace ) where

import AbstractGrammar

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
