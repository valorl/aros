{
module GParser where
import Syntax
}

%name parseGenAros
%tokentype { Token }
%error { parseError }

%token
    id          {TokenIdent $$}
    intLiteral  {TokenIntLit $$}
    int         {TokenInt}
    vec         {TokenVec}
    grid        {TokenGrid}
    '='         {TokenEq}
    '+'         {TokenPlus}
    '-'         {TokenMinus}
    '*'         {TokenTimes}
    '/'         {TokenDiv}
    '>>'        {TokenMove}
    '++'        {TokenAppend}
    '<>'        {TokenUnion}
    '><'        {TokenIntersect}
    'crop'      {TokenCrop}
    '('         {TokenLParen}
    ')'         {TokenRParen}
    '{'         {TokenLBrace}
    '}'         {TokenRBrace}
    '['         {TokenLBracket}
    ']'         {TokenRBracket}
    '<'         {TokenLPointy}
    '>'         {TokenRPointy}
    ','         {TokenComma}
    ';'         {TokenSemiColon}
    '->'        {TokenArrow}
%%

Program : DeclList GridDef                           {}
        | GridDef                                    {}

GridDef : grid Vector Set                            {}

Type :  int                                          {}
     |  vec                                          {}
     | '[' int ']'                                   {}
     | '[' vec ']'                                   {}
     | '{' int '}'                                   {}
     | '(' TypeList '->'Type ')'                     {}

TypeList : Type                                      {}
         | TypeList ',' Type                         {}

BinOp : '+'                                          {}
      | '-'                                          {}
      | '++'                                         {}
      | '<>'                                         {}
      | '><'                                         {}
      | '>>'                                         {}
      | 'crop'                                       {}

ExpTerm : ExpFactor                                  {}
        | ExpTerm '*' ExpFactor                      {}
        | ExpTerm '/' ExpFactor                      {}

ExpFactor : intLiteral                               {}
          | id                                       {}
          | '[' ExpList ']'                          {}
          | '[' ']'                                  {}
          | Set                                      {}
          | Vector                                   {}
          | '(' Exp ')'                              {}
          | id '(' ExpList ')'                       {}
          | id '(' ')'                               {}


Vector : '<' Exp ',' Exp '>'                         {}
Set : '{' ExpList '}'                                {}
    | '{' '}'                                        {}


ExpList : Exp                                        {}
        | ExpList ',' Exp                            {}

Exp : ExpTerm                                        {}
    | Exp BinOp ExpTerm                              {}

Decl : Type id '=' Exp ';'                           {}
     | FunDecl                                       {}

FunDecl : Type id '=' Args '->' '{' DeclList Exp '}' {}
        | Type id '=' Args '->' '{' Exp '}'          {}

Args : '(' ')'                                       {}
     | id                                            {}
     | ArgsList                                      {}

ArgsList : id ',' id                                 {}
         | ArgsList ',' id                           {}

DeclList : Decl                                      {}
         | DeclList Decl                             {}




{
  parseError :: [Token] -> a
  parseError _ = error "Parse error"

-- unwraps an unsized shape so we can parse a sized shape directly
--unwrapUS :: Shape -> [ShapeMan]
--unwrapUS (UShape mans) = mans

-- Happy stuff
--lexwrap :: (TokenState -> Alex a) -> Alex a
--lexwrap = (alexMonadScan' >>=)

--happyError :: TokenState -> Alex a
--happyError (TokenState p t) =
--    alexError' p ("parse error at token '" ++ unlex t ++ "'")

--parseAros = runLexAros parse
}
