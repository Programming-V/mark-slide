{-# LANGUAGE PackageImports #-}

module Main 
    ( main ) where

import Interpreter (generateHTML, writeHTMLFile)
import Scanner
import Parser
import UU.Parsing


main :: IO ()
main = do 
          input <- readFile "slide.p5"
          let tokens = scanTokens input
          putStrLn (show tokens)
          ast <- parseIO pSlides tokens
          putStrLn (show ast)
          let slides = generateHTML ast
          writeHTMLFile "Presentation.html" slides
