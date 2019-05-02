module Main where

import System.Environment

import Parser (parseAros)
-- import Checker (checkProgram)

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
      putStrLn "==================== AST ====================="
      pPrint program
      putStrLn "================ END OF AST =================="
      putStrLn ""
      putStrLn "================== TYPECHECK ================="
      -- programOk <- checkProgram mempty program
      let programOk = True -- TODO
      putStrLn ("Typecheck " <> if programOk then "OK" else "ERROR")
      putStrLn "============== END OF TYPECHECK =============="



  -- let ast = (parseAros . lexAros) code
  -- pPrint ast
