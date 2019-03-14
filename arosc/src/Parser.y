{
module Parser where
import Syntax
}

%name parseAros
%tokentype { Token }
%error { parseError }

%token
    int { TokenInt }
    vec { TokenVec }
    shape { TokenShape }
    at  { TokenAt }
    grid { TokenGrid }
    Point { TokenPoint }
    intLiteral { TokenIntLit $$ }
    '=' { TokenEq }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '{' { TokenLBrace }
    '}' { TokenRBrace }
    '[' { TokenLBracket }
    ']' { TokenRBracket }
    ';' { TokenSemiColon }
    ',' { TokenComma }
    id  { TokenIdent $$ }


%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

GridDef :: { GridDef }
GridDef : VarDecls grid SShape                  { GridDef $1 $3 }

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

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
