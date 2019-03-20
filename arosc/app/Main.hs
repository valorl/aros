module Main where

import System.Environment

import Lexer (lexAros)
import Parser (parseAros)

import Text.Show.Pretty (ppShow, pPrint)


main :: IO ()
main = do
  args <- getArgs
  code <- case args of
        (fp:_) -> readFile fp
        [] -> getContents

  let ast = (parseAros . lexAros) code
  pPrint ast



parseArgs :: [String] -> String
parseArgs [] = error "Missing argument with path of file to compile."
parseArgs (fp:_) = fp
