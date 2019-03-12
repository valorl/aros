{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "//".*                        ;
  int                           { \s -> TokenInt }
  vec                           { \s -> TokenVec }
  shape                         { \s -> TokenShape }
  at                            { \s -> TokenAt }
  grid                          { \s -> TokenGrid }
  Point                         { \s -> TokenPoint }
  $digit+                       { \s -> TokenIntLit (read s) }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLBrace }
  \}                            { \s -> TokenRBrace }
  \[                            { \s -> TokenLBracket }
  \]                            { \s -> TokenRBracket }
  \,                            { \s -> TokenComma }
  \;                            { \s -> TokenSemiColon }
  $alpha [$alpha $digit \_ \']* { \s -> TokenIdent s }

{

-- The token type:
data Token = TokenVar
           | TokenAt
           | TokenGrid
           | TokenPoint
           | TokenInt Int
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenComma
           | TokenIdent String
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
