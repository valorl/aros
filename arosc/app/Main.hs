module Main where

import System.Environment

import Evaluator (evalTree)
import Parser (parseAros)
import Checker (runChecker)

import Text.Show.Pretty (pPrint)


main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parseAros "<stdin>") getContents
              [f] -> fmap (parseAros f) (readFile f)
              _   -> error $ "Expected 0 or 1 arguments, but got " ++ (show $ length $ args)
  case result of
    (Left err) -> putStrLn err
    (Right program) -> do
      putStrLn "==================== AST ====================="
      pPrint program
      putStrLn "================ END OF AST =================="
      putStrLn ""
      putStrLn "================== TYPECHECK ================="
      let (programOk, proglog) = runChecker program
      mapM_ print proglog
      putStrLn ("Typecheck " <> if programOk then "OK" else "ERROR")
      putStrLn "============== END OF TYPECHECK =============="
      putStrLn $ evalTree $ result

