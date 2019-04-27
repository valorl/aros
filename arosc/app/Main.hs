module Main where

import System.Environment

-- import Parser (parseAros)

import Text.Show.Pretty (ppShow, pPrint)


main :: IO ()
main = print "abc"
-- main = do
--   args <- getArgs
--   result <- case args of
--               []  -> fmap (parseAros "<stdin>") getContents
--               [f] -> fmap (parseAros f) (readFile f)
--               _   -> error $ "Expected 0 or 1 arguments, but got " ++ (show $ length $ args)
--   either putStrLn pPrint result
--   -- let ast = (parseAros . lexAros) code
  -- pPrint ast
