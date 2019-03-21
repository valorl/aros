{
module Parser where
import Syntax
import Lexer
}

%name parse
%tokentype { TokenState }
%monad { Alex }
%lexer { lexwrap } { TokenState _ TokenEOF }
%error { happyError }

%token
    int         { TokenState _ TokenInt }
    vec         { TokenState _ TokenVec }
    shape       { TokenState _ TokenShape }
    at          { TokenState _ TokenAt }
    grid        { TokenState _ TokenGrid }
    Point       { TokenState _ TokenPoint }
    intLiteral  { TokenState _ (TokenIntLit $$) }
    '='         { TokenState _ TokenEq }
    '+'         { TokenState _ TokenPlus }
    '-'         { TokenState _ TokenMinus }
    '*'         { TokenState _ TokenTimes }
    '/'         { TokenState _ TokenDiv }
    '('         { TokenState _ TokenLParen }
    ')'         { TokenState _ TokenRParen }
    '{'         { TokenState _ TokenLBrace }
    '}'         { TokenState _ TokenRBrace }
    '['         { TokenState _ TokenLBracket }
    ']'         { TokenState _ TokenRBracket }
    ','         { TokenState _ TokenComma }
    id          { TokenState _ (TokenIdent $$) }


%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Program :: { Program }
Program : VarDecls GridDef                      { Program $1 $2 }
        | GridDef                               { Program [] $1 }

GridDef :: { GridDef }
GridDef : grid SShape                           { GridDef $2 }

Op :: { Op }
Op : '+'                                        { Add }
   | '-'                                        { Sub }
   | '*'                                        { Mul }
   | '/'                                        { Div }

IExp :: { IExp }
IExp : intLiteral                               { ILit $1 }
     | id                                       { IIdent $1 }
     | '(' intLiteral Op IExp ')'               { IOp (ILit $2) $3 $4 }
     | '(' id Op IExp ')'                       { IOp (IIdent $2) $3 $4 }

Vector :: { Vector }
Vector : '(' IExp ',' IExp ')'                  { Vector $2 $4}

VExp :: { VExp }
VExp : Vector                                   { VVec $1 }
     | id                                       { VIdent $1 }
     | '(' Vector Op VExp ')'                   { VOp (VVec $2) $3 $4 }
     | '(' IExp '*' VExp ')'                    { VScaMul $2 $4 }

ShapeManifest :: { ShapeMan }
ShapeManifest : id at VExp                      { SMIdent $1 $3 }
              | Shape at VExp                   { SMShape $1 $3 }
              | Point at VExp                   { SMPoint $3 }


ShapeManifests :: { [ShapeMan] }
ShapeManifests : ShapeManifest                  { [$1] }
         | ShapeManifest ShapeManifests         { $1 : $2 }

UShape :: { Shape }
UShape : '{' ShapeManifests '}'                 { UShape $2 }

SVector :: { SVector }
SVector : '[' IExp ',' IExp ']'                 { SVector $2 $4 }

SShape :: { Shape }
SShape : SVector UShape                         { SShape $1 (unwrapUS $2) }

Shape :: { Shape }
Shape : UShape                                  { $1 }
      | SShape                                  { $1 }

VarDecl :: { VarDecl }
VarDecl : int id '=' IExp                       { IDecl $2 $4 }
        | vec id '=' VExp                       { VDecl $2 $4 }
        | shape id '=' Shape                    { SDecl $2 $4 }

VarDecls :: { [VarDecl] }
VarDecls : VarDecl                              { [$1] }
         | VarDecl VarDecls                     { $1 : $2 }


{

-- unwraps an unsized shape so we can parse a sized shape directly
unwrapUS :: Shape -> [ShapeMan]
unwrapUS (UShape mans) = mans

-- Happy stuff
lexwrap :: (TokenState -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: TokenState -> Alex a
happyError (TokenState p t) =
    alexError' p ("parse error at token '" ++ unlex t ++ "'")

parseAros :: FilePath -> String -> Either String Program
parseAros = runLexAros parse
}
