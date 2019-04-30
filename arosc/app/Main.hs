module Main where

import System.Environment

import Parser (parseAros)
import Checker (checkProgram)

import Text.Show.Pretty (ppShow, pPrint)


main :: IO ()
main = do
  args <- getArgs
  parsed <- case args of
              []  -> fmap (parseAros "<stdin>") getContents
              [f] -> fmap (parseAros f) (readFile f)
              _   -> error $ "Expected 0 or 1 arguments, but got " ++ (show $ length $ args)
  case parsed of
    (Left error) -> putStrLn error
    (Right program) -> do
      programOk <- checkProgram mempty program
      print ("Typecheck " <> if programOk then "OK" else "ERROR")



  -- let ast = (parseAros . lexAros) code
  -- pPrint ast
