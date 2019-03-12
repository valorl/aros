{
module Grammar where
import Tokens
}

%name arosParser
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

Op : '+'  { TokenPlus }
   | '-'  { TokenMinus }
   | '*'  { TokenTimes }
   | '/'  { TokenDiv }

IExp : intLiteral                   { intLiteral $1 }
     | id                           { id $1 }
     | '(' intLiteral Op IExp ')'   { IExpInt $2 $3 $4 }
     | '(' id Op IExp ')'           { IExpId  $2 $3 $4 }

Vector : '(' IExp ',' IExp ')'      { VectorLiteral $2 $4}

VExp : Vector                       { VExpVec $1 }
     | id                           { VExpId  $1 }
     | '(' IExp '*' VExp ')'        { VExpMult $2 $4 }

ShpDecl : id at VExp                { ShpDeclId  $1 $3 }
        | Shape at VExp             { ShpDeclShp $1 $3 }
        | Point at VExp             { ShpDeclPoint $3 }


MShpDecl : ShpDecl ';'              { MShpDeclEnd  $1 }
         | ShpDecl ',' MShpDecl     { MshpDeclCont $1 $3 }

UShape : '{' MShpDecl '}'           { UShape $2 }

SVector : '[' IExp ',' IExp ']'     { SVector $2 $4 }

SShape : SVector UShape             { SShape $1 $2 }

Shape : UShape                      { SUShape $1 }
      | SShape                      { SSShape $1 }

Declaration : int id '=' IExp       { DeclInt  $2 $4 }
            | vec id '=' VExp       { DecVec   $2 $4 }
            | shape id '=' Shape    { DecShape $2 $4 }

MDeclaration : Declaration ';'      { MDeclEnd $1 }
             | Declaration ',' MDeclaration { MDeclCont $1 $3 }


GridDef : MDeclaration grid SShape  { GridDef $1 $3 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
