{
module Grammar where
import Tokens
}

%name arosParser
%tokentype { Token }
%error { parseError }

%token
    var { TokenVar }
    at  { TokenAt }
    grid { TokenGrid }
    Point { TokenPoint }
    int { TokenInt $$ }
    ident  { TokenIdent $$ }
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
    ',' { TokenComma }

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

operator : '+' | '-' | '*' | '/'

IntExp : int
    | "(" IntExp ")"
    | int operator IntExp

Vector : "(" IntExp "," IntExp ")"

VecExp :  Vector
    | "(" VecExp ")"
    | VecExp operator VecExp
    | IntExp "*" VecExp
    | VecExp "*" IntExp

UShape : '{' ( (ident | Shape | point) at (VecExp | ident) )+ '}'

SVector : '[' IntExp ',' IntExp ']'

Shape : UShape
    | SVector UShape

Declaration :
    var identifier '=' ( IntExp | VecExp | Shape )

OptRecDecl :

GridDef : grid SShape
    | Declaration grid SShape


Factor : int                        { IntegerLiteral $1 }
    | var ident                     { VarDecl $2 }
    | Point                         { Vector 0 0 }
    | '(' int ',' int ')'           { Vector $2 $4 }




{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
