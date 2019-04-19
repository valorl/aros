## Terminals
```ebnf
boolean     ::= "true" | "false"
integer     ::= [-]?[1-9][0-9]*
identifier  ::= [a-z][a-zA-Z0-9_]*
```

## Operators
### Unary
```ebnf
Uop     ::= "not" | "head" | "tail" | "vecx" | "vecy"
```

### Binary
```ebnf
Bop     ::= "+" | "-" | "/" | "*" | ":" | "++" | "<>" | "><" | ">>" | "crop" 
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
        | "<" Exp "," Exp ">"
        | "[" [Exp ","]* Exp "]"
        | "[" "]"
        | "{" [Exp ","]* Exp "}"
        | "{" "}"
        | "(" Exp ")"
        | Exp Bop Exp
        | Uop Exp
        | Lambda
        | FunctionApplication
        | "if" "(" Exp ")" Block "else" Block
        | "cond" "{" ["(" Exp ")" Block]+ "otherwise" Block "}" 
```

### Expression helpers
```ebnf
Args    ::= [identifier ","]* identifier | λ
Params  ::= [Exp ","]* Exp | λ
Block   ::= "{" Declaration* Exp "}"

Lambda              ::= Args "->" Block
FunctionApplication ::= [identifier | Lambda] "(" Params ")"
```

## Declarations
```ebnf
Declaration ::= Type identifier "=" Exp ";"
```

## Grid
```ebnf
GridDef     ::= Declaration* "grid" Exp Exp
```

## Progam (root)
```ebnf
Program ::= GridDef
```
