## Integers
```ebnf
integer ::= [-]?[1-9][0-9]*
```

## Identifiers
```ebnf
identifier ::= [a-z][a-zA-Z0-9_]*
```

## The `point` keyword
```ebnf
point ::= "point"
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
    | VExp "*" IExp
```

## Vectors
```ebnf
Vector ::= "(" IExp "," IExp ")"
```

## Shapes
### Unsized shape
```ebnf
UShape ::= "{" ( (identifier | Shape | point) "at" VExp )+ "}"
```

### Size vector
```ebnf
SVector ::= "[" IExp "," IExp "]"
```

### Sized shape
```ebnf
SShape ::= SVector UShape
```

### General shape
```ebnf
Shape ::= UShape | SShape
```

## Variable declaration
```ebnf
Declaration ::= "var" identifier "=" ( IExp | VExp | Shape )
```

## Grid
### Grid shape (forced to have size vector)
```ebnf
GridDef ::= (Declaration)* | (Declaration)* "grid" SShape
```

## Progam (root)
```ebnf
Program ::= GridDef
```

#### Note
*`Shape` and `Declaration`do **not*** allow brace-less declarations of one-line
shapes in this grammar (I vote let's move this nice-to-have for later)*
