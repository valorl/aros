## Integers
```ebnf
integer ::= [-]?[1-9][0-9]*
```

## Identifiers
```ebnf
identifier ::= [a-z][a-zA-Z0-9_]*
```

## Operators
```ebnf
op ::= "+" | "-" | "/" | "*"
```

## Expressions

### Integer expressions
```ebnf
IExp ::= 
    | integer
    | identifier
    | "(" IExp ")"
    | IExp op IExp
```

### Vector expressions
```ebnf
VExp ::= 
    | Vector
    | identifier
    | "(" VExp ")"
    | VExp op VExp
    | IExp "*" VExp
```

### Shape expressions
```ebnf
SExp   ::= Shape
        | identifier
        | SExp "move" VExp
        | SExp "join" SExp
        | SExp "crop" VExp
        | "(" SExp ")"
```

## Vectors
```ebnf
Vector ::= "(" IExp "," IExp ")"
```

## Shapes
```ebnf
Shape ::= "[" Vectors VExp "]"
```

```ebnf
Vectors ::= VExp "," Vectors 
        | VExp ","
        | λ
```

## Variable declaration
```ebnf
Declaration ::= "var" identifier "=" ( IExp | VExp | SExp )
```

## Grid
### Grid shape (forced to have size vector)
```ebnf
GridDef ::= (Declaration)* | (Declaration)* "grid" VExp SExp
```

## Progam (root)
```ebnf
Program ::= GridDef
```
