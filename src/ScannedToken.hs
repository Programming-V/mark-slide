module ScannedToken 
    ( ScannedToken 
    ) where

import qualified Token as T

data ScannedToken = ScannedToken 
  {
    _token :: T.Token,
    _type :: T.Type,
    _value :: T.Value,
    _line :: T.Line,
    _col :: T.Col
  } 
  deriving (Eq)

instance Show ScannedToken where
  show (ScannedToken _token _type _value _line _col) = 
    (show _token) ++ " " ++ (show _value) ++ (show _line) ++ " " ++ (show _col) ++ "\n"
