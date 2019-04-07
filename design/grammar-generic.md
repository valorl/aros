## Terminals
### Integers
```ebnf
boolean     ::= "true" | "false"
integer     ::= [-]?[1-9][0-9]*
identifier  ::= [a-z][a-zA-Z0-9_]*

operator    ::= "+" | "-" | "/" | "*" | "++" | "<>" | "><" | ">>" | "crop" 
            |   "and" | "or" | ">" | "<" | ">=" | "<=" | "==" | "!=" 
```

## Types

```ebnf
Type ::= 
          "int" 
        | "vec"
        | "bool"
        | "[" Type "]" 
        | "{" Type "}" 
        | "(" [Type ","]* Type "->" Type ")"
        | "(" "->" Type ")"
```

## Expressions

```ebnf
Exp ::=
          identifier
        | integer
        | boolean
        | Exp ? Exp : Exp
        | "<" Exp "," Exp ">"
        | "[" [Exp ","]* Exp "]"
        | "[" "]"
        | "{" [Exp ","]* Exp "}"
        | "{" "}"
        | "(" Exp ")"
        | Exp operator Exp
        | "!" Exp
        | Lambda
        | FunctionApplication
```

### Expression helpers
```ebnf
Args    ::= [identifier ","]* identifier | λ
Params  ::= [Exp ","]* Exp | λ

Lambda              ::= Args "->" "{" Declaration* Exp "}"
FunctionApplication ::= [identifier | Lambda] "(" Params ")"
```

## Declarations
```ebnf
Declaration ::= Type identifier "=" Exp ";"
```

## Grid
```ebnf
GridDef ::= (Declaration | FuncDeclaration)* "grid" Exp Exp
```

## Progam (root)
```ebnf
Program ::= GridDef
```
