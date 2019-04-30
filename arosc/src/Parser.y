{
module Parser where
import Syntax
import Lexer
}

%name parse
%tokentype                                                    { TokenState }
%monad                                                        { Alex }
%lexer                                                        { lexwrap } { TokenState _ TokenEOF }
%error                                                        { happyError }

%token
    intLiteral                                                { TokenState _ (TokenIntLit $$) }
    boolLiteral                                               { TokenState _ (TokenBoolLit $$) }
    not                                                       { TokenState _ TokenNot }
    head                                                      { TokenState _ TokenHead }
    tail                                                      { TokenState _ TokenTail }
    vecx                                                      { TokenState _ TokenVecx }
    vecy                                                      { TokenState _ TokenVecy }
    int                                                       { TokenState _ TokenInt }
    vec                                                       { TokenState _ TokenVec }
    bool                                                      { TokenState _ TokenBool }
    grid                                                      { TokenState _ TokenGrid }
    crop                                                      { TokenState _ TokenCrop }
    and                                                       { TokenState _ TokenAnd }
    or                                                        { TokenState _ TokenOr }
    'if'                                                      { TokenState _ TokenIf }
    'else'                                                    { TokenState _ TokenElse }
    cond                                                      { TokenState _ TokenCond }
    otherwise                                                 { TokenState _ TokenOtherwise }
    route                                                     { TokenState _ TokenRoute }
    '->'                                                      { TokenState _ TokenArrow }
    '+'                                                       { TokenState _ TokenPlus }
    '-'                                                       { TokenState _ TokenMinus }
    '*'                                                       { TokenState _ TokenTimes }
    '/'                                                       { TokenState _ TokenDiv }
    ':'                                                       { TokenState _ TokenColon }
    '++'                                                      { TokenState _ TokenDoublePlus }
    '<>'                                                      { TokenState _ TokenUnion }
    '><'                                                      { TokenState _ TokenIntersection }
    '>>'                                                      { TokenState _ TokenShift }
    '>='                                                      { TokenState _ TokenGte }
    '<='                                                      { TokenState _ TokenLte }
    '>'                                                       { TokenState _ TokenGt }
    '<'                                                       { TokenState _ TokenLt }
    '=='                                                      { TokenState _ TokenEq }
    '!='                                                      { TokenState _ TokenNeq }
    '='                                                       { TokenState _ TokenAssign }
    '('                                                       { TokenState _ TokenLParen }
    ')'                                                       { TokenState _ TokenRParen }
    '{'                                                       { TokenState _ TokenLBrace }
    '}'                                                       { TokenState _ TokenRBrace }
    '['                                                       { TokenState _ TokenLBracket }
    ']'                                                       { TokenState _ TokenRBracket }
    ','                                                       { TokenState _ TokenComma }
    ';'                                                       { TokenState _ TokenSemiColon }
    id                                                        { TokenState _ (TokenIdent $$) }


%right in
%nonassoc '>' '<'
%left ':' '++' '<>' '><' '>>' crop and or
%left '+' '-'
%left '*' '/'
%left NEG

%%

Program ::                                                    { Program }
Program : GridDef RobotRoute                                  { Program [] $1 $2 }
        | DeclarationList GridDef RobotRoute                  { Program $1 $2 $3 }

RobotRoute ::                                                 { RobotRoute }
RobotRoute : route Exp                                        { RobotRoute $2}

Uop ::                                                        { UnaryOp }
Uop : not                                                     { Not }
  | head                                                      { Head }
  | tail                                                      { Tail }
  | vecx                                                      { Vecx }
  | vecy                                                      { Vecy }


Bop ::                                                        { BinaryOp }
Bop : '+'                                                     { Plus }
    | '-'                                                     { Minus }
    | ':'                                                     { Cons }
    | '++'                                                    { Append }
    | '<>'                                                    { Union }
    | '><'                                                    { Intersection }
    | '>>'                                                    { Shift }
    | crop                                                    { Crop }
    | and                                                     { And }
    | or                                                      { Or }



TypeList ::                                                   { [DeclType] }
TypeList : Type                                               { [$1] }
         | Type ',' TypeList                                  { $1 : $3 }

Type ::                                                       { DeclType }
Type : int                                                    { TypeInt }
     | vec                                                    { TypeVec }
     | bool                                                   { TypeBool }
     | '[' Type ']'                                           { TypeList $2 }
     | '{' Type '}'                                           { TypeSet $2 }
     | '(' '->' Type ')'                                      { TypeLambda [] $3 }
     | '(' TypeList '->' Type ')'                             { TypeLambda $2 $4 }


ExpList ::                                                    { [Exp] }
ExpList : Exp                                                 { [$1] }
        | Exp ',' ExpList                                     { $1 : $3 }


ExpBlock : Exp '{' Block '}'                                  { ($1 , $3) }

ExpBlockList ::                                               { [(Exp, Block)] }
ExpBlockList : ExpBlock                                       { [$1] }
             | ExpBlock ExpBlockList                          { $1 : $2 }

Declaration : Type id '=' Exp ';'                             { Decl $1 $2 $4 }

DeclarationList ::                                            { [Declaration] }
DeclarationList : Declaration                                 { [$1] }
                | Declaration DeclarationList                 { $1 : $2 }

Block ::                                                      { Block }
Block : Exp                                                   { Block [] $1 }
      | Declaration Block                                     { let (Block l e) = $2 in (Block ( $1 : l ) e)}


Exp ::                                                        { Exp }
Exp : ExpA                                                    { $1 }
    | ExpA Bop Exp                                            { BinaryExp $1 $2 $3 }

ExpA ::                                                       { Exp }
ExpA : ExpB                                                   { $1 }
     | ExpB '*' ExpA                                          { BinaryExp $1 Times $3 }
     | ExpB '/' ExpA                                          { BinaryExp $1 Div $3 }

ExpB ::                                                       { Exp }
ExpB : ExpC                                                   { $1 }
     | Uop ExpC                                               { UnaryExp $1 $2 }

IdList ::                                                     { [String] }
IdList : id ',' id                                            { [$1, $3] }
       | id ',' IdList                                        { $1 : $3 }

Lambda ::                                                     { Exp }
Lambda : '(' ')' '->' '{' Block '}'                           { LambdaExp [] $5 }
       | id '->' '{' Block '}'                                { LambdaExp [$1] $4 }
       | '(' IdList ')' '->' '{' Block '}'                    { LambdaExp $2 $6 }

ExpC ::                                                       { Exp }
ExpC : id                                                     { VariableExp $1 }
    | intLiteral                                              { IntegerExp $1 }
    | boolLiteral                                             { BooleanExp $1 }
    | '<' Exp ',' Exp '>'                                     { VectorExp $2 $4 }
    | '[' ExpList ']'                                         { ListExp $2 }
    | '[' ']'                                                 { ListExp [] }
    | '{' ExpList '}'                                         { SetExp $2 }
    | '{' '}'                                                 { SetExp [] }
    | '(' Exp ')'                                             { ParenExp $2 }
    | id '(' ExpList ')'                                    { ApplicationExp $1 $3 }
    | Lambda                                                  { $1 }
    | 'if' Exp '{' Block '}' 'else' '{' Block '}'             { IfExp $2 $4 $8 }
    | cond '{' ExpBlockList otherwise '{' Block '}' '}'       { CondExp $3 $6 }
    | '(' Exp '<' Exp ')'                                     { BinaryExp $2 Lt $4  }
    | '(' Exp '>' Exp ')'                                     { BinaryExp $2 Gt $4 }
    | '(' Exp '<=' Exp ')'                                    { BinaryExp $2 Lte $4 }
    | '(' Exp '>=' Exp ')'                                    { BinaryExp $2 Gte $4 }
    | '(' Exp '==' Exp ')'                                    { BinaryExp $2 Equal $4 }
    | '(' Exp '!=' Exp ')'                                    { BinaryExp $2 NotEqual $4 }

GridDef ::                                                    { GridDef }
GridDef : grid Exp ',' Exp                                    { GridDef $2 $4 }

{


-- Happy stuff
lexwrap :: (TokenState -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: TokenState -> Alex a
happyError (TokenState p t) =
    alexError' p ("parse error at token '" ++ unlex t ++ "'")

parseAros :: FilePath -> String -> Either String Program
parseAros = runLexAros parse
}
