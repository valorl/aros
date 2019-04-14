{
module Parser where
import Syntax
import Lexer
}

%name parse
%tokentype       { TokenState }
%monad           { Alex }
%lexer           { lexwrap } { TokenState _ TokenEOF }
%error           { happyError }

%token
    intLiteral   { TokenState _ (TokenIntLit $$) }
    boolLiteral  { TokenState _ (TokenBoolLit $$) }
    not          { TokenState _ TokenNot }
    head         { TokenState _ TokenHead }
    tail         { TokenState _ TokenTail }
    vecx         { TokenState _ TokenVecx }
    vecy         { TokenState _ TokenVecy }
    int          { TokenState _ TokenInt }
    vec          { TokenState _ TokenVec }
    bool         { TokenState _ TokenBool }
    grid         { TokenState _ TokenGrid }
    crop         { TokenState _ TokenCrop }
    and          { TokenState _ TokenAnd }
    or           { TokenState _ TokenOr }
    'if'         { TokenState _ TokenIf }
    'else'       { TokenState _ TokenElse }
    cond         { TokenState _ TokenCond }
    otherwise    { TokenState _ TokenOtherwise }
    '->'         { TokenState _ TokenArrow }
    '+'          { TokenState _ TokenPlus }
    '-'          { TokenState _ TokenMinus }
    '*'          { TokenState _ TokenTimes }
    '/'          { TokenState _ TokenDiv }
    ":"          { TokenState _ TokenColon }
    '++'         { TokenState _ TokenDoublePlus }
    '<>'         { TokenState _ TokenUnion }
    '><'         { TokenState _ TokenIntersection }
    '>>'         { TokenState _ TokenShift }
    '>='         { TokenState _ TokenGte }
    '<='         { TokenState _ TokenLte }
    '>'          { TokenState _ TokenGt }
    '<'          { TokenState _ TokenLt }
    '=='         { TokenState _ TokenEq }
    '!='         { TokenState _ TokenNeq }
    '='          { TokenState _ TokenAssign }
    '('          { TokenState _ TokenLParen }
    ')'          { TokenState _ TokenRParen }
    '{'          { TokenState _ TokenLBrace }
    '}'          { TokenState _ TokenRBrace }
    '['          { TokenState _ TokenLBracket }
    ']'          { TokenState _ TokenRBracket }
    ','          { TokenState _ TokenComma }
    ';'          { TokenState _ TokenSemiColon }
    id           { TokenState _ (TokenIdent $$) }


%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Program : GridDef                                           {}

Uop ::                                                      {}
Uop : not                                                   {}
  | head                                                    {}
  | tail                                                    {}
  | vecx                                                    {}
  | vecy                                                    {}


Bop ::                                                      {}
Bop : '+'                                                   {}
    | '-'                                                   {}
    | '*'                                                   {}
    | '/'                                                   {}
    | ":"                                                   {}
    | '++'                                                  {}
    | '<>'                                                  {}
    | '><'                                                  {}
    | '>>'                                                  {}
    | crop                                                  {}
    | and                                                   {}
    | or                                                    {}


TypeList ::                                                 {}
TypeList : Type                                             {}
         | Type ',' TypeList                                {}

Type ::                                                     {}
Type : int                                                  {}
     | vec                                                  {}
     | bool                                                 {}
     | '[' Type ']'                                         {}
     | '{' Type '}'                                         {}
     | '(' '->' Type ')'                                    {}
     | '(' TypeList '->' Type ')'                           {}


ExpList ::                                                  {}
ExpList : Exp                                               {}
        | Exp ',' ExpList                                   {}


ExpAndBlock ::                                              {}
ExpAndBlock : Exp '{' Block '}'                             {}

ExpAndBlockList ::                                          {}
ExpAndBlockList : ExpAndBlock                               {}
                | ExpAndBlock ExpAndBlockList               {}

Declaration ::                                              {}
Declaration : Type id '=' Exp ';'                           {}

DeclarationList ::                                          {}
DeclarationList : Declaration                               {}
                | Declaration DeclarationList               {}

Block ::                                                    {}
Block : Exp                                                 {}
      | Declaration Block                                   {}


FunctionApplication ::                                      {}
FunctionApplication : ExpT '(' ExpList ')'                  {}


Exp : ExpU                                                  {}
    | ExpU Bop Exp                                          {}

ExpU : ExpT                                                 {}
     | Uop ExpT                                             {}

IdList : id                                                 {}
       | IdList ',' id                                      {}

ExpT ::                                                     {}
ExpT : id                                                   {}
    | intLiteral                                            {}
    | boolLiteral                                           {}
    | '<' Exp ',' Exp '>'                                   {}
    | '[' ExpList ']'                                       {}
    | '[' ']'                                               {}
    | '{' ExpList '}'                                       {}
    | '{' '}'                                               {}
    | '(' IdList ')' '->' '{' Block '}'                     {}
    | '(' ')' '->' '{' Block '}'                            {}
    | FunctionApplication                                   {}
    | 'if' Exp '{' Block '}' 'else' '{' Block '}'           {}
    | cond '{' ExpAndBlockList otherwise '{' Block '}' '}'  {}
    | '(' Exp '<' Exp ')'                                   {}
    | '(' Exp '>' Exp ')'                                   {}
    | '(' Exp '<=' Exp ')'                                  {}
    | '(' Exp '>=' Exp ')'                                  {}
    | '(' Exp '==' Exp ')'                                  {}
    | '(' Exp '!=' Exp ')'                                  {}

GridDef ::                                                  {}
GridDef : grid Exp ',' Exp                                  {}
        | DeclarationList grid Exp ',' Exp                  {}

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
