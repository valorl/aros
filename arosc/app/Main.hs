module Main where

import System.Environment
import System.Exit

import Evaluator (evalTree, pprintResult)
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
      putStrLn ("Typecheck " <> if programOk then "OK" else "NOT OK")
      mapM_ print proglog
      putStrLn "============== END OF TYPECHECK =============="
      if not programOk
      then exitFailure
      else do
        putStrLn ""
        putStrLn "=================== RESULT ==================="
        case (evalTree result) of
           (Right res) -> do
              putStrLn $ pprintResult res
              exitSuccess
           (Left err) -> do
              putStrLn err
              exitSuccess
