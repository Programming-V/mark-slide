{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Token where

type Line = Int 
type Col = Int
type Value = String
type Input = String

data Token = Token Type Value Line Col 
        -- deriving(Eq)

instance Show Token where
    show (Token t v l c) = show t ++ show v ++ " " ++ show l ++ " " ++ show c ++ "\n"

data Type = String 
          | OpenBlock
          | EndBlock
          | Keyword
          | EndSlide
          | Error
          | Comment
        deriving(Eq, Ord)

instance Show Type where
    show String = "String: "
    show OpenBlock = "OpenBlock: "
    show EndBlock = "EndBlock: "
    show Keyword = "Keyword: "
    show Error = "Error: "
    show EndSlide = "EndSlide: "
    show Comment = "Comment: "

instance (Eq Type) => (Eq Token) where
  -- (Token String s1 _ _) == (Token String s2 _ _) = s1 == s2
  (Token String _ _ _) == (Token String _ _ _) = True
  (Token OpenBlock _ _ _) == (Token OpenBlock _ _ _) = True
  (Token EndBlock _ _ _) == (Token EndBlock _ _ _) = True
  (Token Keyword k1 _ _) == (Token Keyword k2 _ _) = k1 == k2
  (Token Error e1 _ _) == (Token Error e2 _ _) = e1 == e2
  (Token EndSlide _ _ _) == (Token EndSlide _ _ _) = True
  (Token t1 s1 _ _) == (Token t2 s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token t1 s1 _ _) <= (Token t2 s2 _ _) = t1 < t2 || (t1 == t2 && s1 <= s2)
