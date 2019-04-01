## Terminals
### Integers
```ebnf
integer ::= [-]?[1-9][0-9]*
```

### Identifiers
```ebnf
identifier ::= [a-z][a-zA-Z0-9_]*
```

### Operators
```ebnf
op ::= "+" | "-" | "/" | "*"
```

## Types

```ebnf
Type ::= 
          "int" 
        | "vec" 
        | "[" int "]" 
        | "[" vec "]" 
        | "{" int "}" 
        | "{" vec "}" 
        | "(" [Type ","]* Type "->" Type ")"
        | "(" "->" Type ")"
```

## Expressions

```ebnf
Exp ::=
          identifier
        | integer
        | "<" Exp "," Exp ">"
        | "[" [Exp ","]* Exp "]"
        | "[" "]"
        | "{" [Exp ","]* Exp "}"
        | "{" "}"
        | "(" Exp ")"
        | Exp op     Exp
        | Exp "++"   Exp
        | Exp "<>"   Exp
        | Exp "><"   Exp
        | Exp ">>"   Exp
        | Exp "crop" Exp
        | identifier "(" [Exp* ","] Exp ")"
        | identifier "(" ")"
```

## Declarations
```ebnf
Declaration     ::= Type identifier "=" Exp

FuncDeclaration ::= Type identifier "=" "(" [identifier ","]* identifier ")" "->" "{" "let" Declaration* "in" Exp "}"
                |   Type identifier "=" "(" ")" "->" "{" "let" Declaration* "in" Exp "}"
```

## Grid
```ebnf
GridDef ::= (Declaration | FuncDeclaration)* "grid" Exp Exp
```

## Progam (root)
```ebnf
Program ::= GridDef
```
