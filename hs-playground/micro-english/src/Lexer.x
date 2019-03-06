{
module Lexer(
scanTokens
) where

import Language

}

%wrapper "basic"

tokens :-

  -- Syntax
  $white+                     ;
  I                           { \s -> TokenI }
  a                           { \s -> TokenA }
  the                         { \s -> TokenThe }
  me                          { \s -> TokenMe }
  cat                         { \s -> TokenCat }
  mat                         { \s -> TokenMat }
  rat                         { \s -> TokenRat }
  like                        { \s -> TokenLike }
  is                          { \s -> TokenIs }
  see                         { \s -> TokenSee }
  sees                        { \s -> TokenSees }
  "."                         { \s -> TokenDot }

{
scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
