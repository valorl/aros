{
module Lexer where

import Prelude
import Control.Monad (liftM)

import Syntax
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "//".*                        ;
  int                           { lexTkn TokenInt }
  vec                           { lexTkn TokenVec }
  shape                         { lexTkn TokenShape }
  at                            { lexTkn TokenAt }
  grid                          { lexTkn TokenGrid }
  Point                         { lexTkn TokenPoint }
  $digit+                       { lexInputTkn (TokenIntLit . read) }
  \=                            { lexTkn TokenEq }
  \+                            { lexTkn TokenPlus }
  \-                            { lexTkn TokenMinus }
  \*                            { lexTkn TokenTimes }
  \/                            { lexTkn TokenDiv }
  \(                            { lexTkn TokenLParen }
  \)                            { lexTkn TokenRParen }
  \{                            { lexTkn TokenLBrace }
  \}                            { lexTkn TokenRBrace }
  \[                            { lexTkn TokenLBracket }
  \]                            { lexTkn TokenRBracket }
  \,                            { lexTkn TokenComma }
  $alpha [$alpha $digit \_ \']* { lexInputTkn TokenIdent }

{

unlex :: Token        -> String
unlex (TokenIntLit i) = show i
unlex TokenInt        = "int"
unlex TokenVec        = "vec"
unlex TokenShape      = "shape"
unlex TokenAt         = "at"
unlex TokenGrid       = "grid"
unlex TokenPoint      = "Point"
unlex TokenEq         = "="
unlex TokenPlus       = "+"
unlex TokenMinus      = "-"
unlex TokenTimes      = "*"
unlex TokenDiv        = "/"
unlex TokenLParen     = "("
unlex TokenRParen     = ")"
unlex TokenLBrace     = "{"
unlex TokenRBrace     = "}"
unlex TokenLBracket   = "["
unlex TokenRBracket   = "]"
unlex TokenComma      = ","
unlex (TokenIdent s)  = show s
unlex TokenEOF        = "<EOF>"

data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

data TokenState = TokenState AlexPosn Token
     deriving ( Show )

alexEOF :: Alex TokenState
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ TokenState p TokenEOF

lexInputTkn :: (String -> Token) -> AlexAction TokenState
lexInputTkn f = \(p,_,_,s) i -> return $ TokenState p (f (take i s))

lexTkn :: Token -> AlexAction TokenState
lexTkn = lexInputTkn . const


alexMonadScan' :: Alex TokenState
alexMonadScan' = do
  input <- alexGetInput
  sc <- alexGetStartCode
  case alexScan input sc of
    AlexEOF -> alexEOF
    AlexError (p,_,_,s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes input) len


alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)


runLexAros :: Alex a -> FilePath -> String -> Either String a
runLexAros a fp input = runAlex input (setFilePath fp >> a)

}
