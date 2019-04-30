{
module Lexer where

import Prelude
import Control.Monad (liftM)

import Syntax
}

%wrapper "monadUserState"

tokens :-
  $white+                              ;
  "//".*                            ;
  [\-]?(0|[1-9][0-9]*)    { lexInputTkn (TokenIntLit . read) }
  true|false          { lexInputTkn ( TokenBoolLit . (== "true")) }
  not                 { lexTkn TokenNot }
  head                { lexTkn TokenHead }
  tail                { lexTkn TokenTail }
  vecx                { lexTkn TokenVecx }
  vecy                { lexTkn TokenVecy }
  int                 { lexTkn TokenInt }
  vec                 { lexTkn TokenVec }
  bool                { lexTkn TokenBool }
  grid                { lexTkn TokenGrid }
  route               { lexTkn TokenRoute }
  crop                { lexTkn TokenCrop }
  and                 { lexTkn TokenAnd }
  or                  { lexTkn TokenOr }
  if                  { lexTkn TokenIf }
  else                { lexTkn TokenElse }
  cond                { lexTkn TokenCond }
  otherwise           { lexTkn TokenOtherwise }
  "->"                { lexTkn TokenArrow }
  "+"                 { lexTkn TokenPlus }
  "-"                 { lexTkn TokenMinus }
  "/"                 { lexTkn TokenDiv }
  "*"                 { lexTkn TokenTimes }
  ":"                 { lexTkn TokenColon }
  "++"                { lexTkn TokenDoublePlus }
  "<>"                { lexTkn TokenUnion }
  "><"                { lexTkn TokenIntersection }
  ">>"                { lexTkn TokenShift }
  ">="                { lexTkn TokenGte }
  "<="                { lexTkn TokenLte }
  ">"                 { lexTkn TokenGt }
  "<"                 { lexTkn TokenLt}
  "=="                { lexTkn TokenEq }
  "!="                { lexTkn TokenNeq }
  "="                 { lexTkn TokenAssign }
  "("                 { lexTkn TokenLParen }
  ")"                 { lexTkn TokenRParen }
  "{"                 { lexTkn TokenLBrace }
  "}"                 { lexTkn TokenRBrace }
  "["                 { lexTkn TokenLBracket }
  "]"                 { lexTkn TokenRBracket }
  ","                 { lexTkn TokenComma }
  ";"                 { lexTkn TokenSemiColon }
  [a-z][a-zA-Z0-9_]*  { lexInputTkn TokenIdent }

{

unlex :: Token        -> String
unlex (TokenIntLit i)    = show i
unlex (TokenBoolLit i)   = show i
unlex TokenNot           = "not"
unlex TokenHead          = "head"
unlex TokenTail          = "tail"
unlex TokenVecx          = "vecx"
unlex TokenVecy          = "vecy"
unlex TokenInt           = "int"
unlex TokenVec           = "vec"
unlex TokenBool          = "bool"
unlex TokenGrid          = "grid"
unlex TokenRoute         = "route"
unlex TokenCrop          = "crop"
unlex TokenAnd           = "and"
unlex TokenOr            = "or"
unlex TokenIf            = "if"
unlex TokenElse          = "else"
unlex TokenCond          = "cond"
unlex TokenOtherwise     = "otherwise"
unlex TokenArrow         = "->"
unlex TokenPlus          = "+"
unlex TokenMinus         = "-"
unlex TokenTimes         = "*"
unlex TokenDiv           = "/"
unlex TokenColon         = ":"
unlex TokenDoublePlus    = "++"
unlex TokenUnion         = "<>"
unlex TokenIntersection  = "><"
unlex TokenShift         = ">>"
unlex TokenGt            = ">"
unlex TokenLt            = "<"
unlex TokenGte           = ">="
unlex TokenLte           = "<="
unlex TokenEq            = "=="
unlex TokenNeq           = "!="
unlex TokenAssign        = "="
unlex TokenLParen        = "("
unlex TokenRParen        = ")"
unlex TokenLBrace        = "{"
unlex TokenRBrace        = "}"
unlex TokenLBracket      = "["
unlex TokenRBracket      = "]"
unlex TokenComma         = ","
unlex TokenSemiColon     = ";"
unlex TokenEOF           = "<EOF>"
unlex (TokenIdent s)     = show s

data AlexUserState = AlexUserState  { filePath :: FilePath }

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

stringToBool :: String -> Bool
stringToBool b = b == "true"

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
