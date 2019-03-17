module Main where

import Lexer (lexAros)
import Parser (parseAros)

import Text.Show.Pretty (ppShow, pPrint)

main :: IO ()
main = do
  putStr "\n\n"
  putStr "Code to parse:"
  putStr "\n"
  print code
  putStr "\n\n"
  putStr "Tree:"
  putStr "\n"
  let ast = sampleCompilation
  pPrint ast

code = "shape myShape = { Point at (1,1) } \
               \ grid [5,5] { myShape at (1,2) }"

sampleCompilation = parseAros $ lexAros $ code
