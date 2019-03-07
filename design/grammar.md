## Integers
`int ::= [-]?[1-9][0-9]*`

## Identifiers
`identifier ::= [a-z][a-zA-Z0-9_]*`

## The `point` keyword
`point -> "point"`

## Operators
`op ::= "+" | "-" | "/" | "*"`

## Expressions

### Integer expressions
```
IntExp ::= 
    | int
    | "(" IntExp ")"
    | IntExp op IExp
```

### Vector expressions
```
VecExp ::= 
    | Vector
    | "(" VecExp ")"
    | VecExp op VExp
    | IntExp "*" VecExp
    | VecExp "*" IntExp
```

## Vectors
```
Vector ::= "(" IntExp "," IntExp ")"
```

## Shapes
### Unsized shape
`UShape ::= "{" ( (identifier | Shape | point) "at" VecExp )+ "}"`

### Size vector
`SVector ::= "[" IntExp "," IntExp "]"`

### Sized shape
`SShape ::= SVector UShape`

### General shape
`Shape ::= UShape | SShape`


## Variable declaration
```
Declaration ::= "var" identifier "=" ( IntExp | VecExp | Shape )
```


## Grid
### Grid shape (forced to have size vector)
`GridDef ::= (Declaration)* "grid" SShape`

## Progam (root)
`Program -> GridDef`

#### Note
*`Shape` and `Declaration`do **not*** allow brace-less declarations of one-line
shapes in this grammar (I vote let's move this nice-to-have for later)*
