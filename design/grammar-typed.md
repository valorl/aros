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

### Sets
```ebnf
emptySet ::= "{" "}"
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
    | "(" IExp "," IExp ")"
    | identifier
    | "(" VExp ")"
    | VExp op VExp
    | IExp "*" VExp
```

### Integer set expressions
```ebnf
ISetExp ::=  
         "{" [IExp ","]* IExp "}"
        | identifier
        | emptySet
        | "(" ISetExp ")"
        | ISetExp "join"    ISetExp
        | ISetExp "merge"   ISetExp
        | ISetExp "shift"   IExp
        | ISetExp "crop"    IExp 
```

### Vector set expressions
```ebnf
VSetExp ::=  
         "{" [VExp ","]* VExp "}"
        | identifier
        | emptySet
        | "(" VSetExp ")"
        | VSetExp "join"    VSetExp
        | VSetExp "merge"   VSetExp
        | VSetExp "shift"   VExp
        | VSetExp "crop"    VExp
```

## Variable declaration
```ebnf

Declaration ::= 
              "int"     identifier "=" IExp
            | "vec"     identifier "=" VExp
            | "[int]"   identifier "=" ISetExp
            | "[vec]"   identifier "=" VSetExp
```

## Grid
### Grid shape (forced to have size vector)
```ebnf
GridDef ::= (Declaration)* | (Declaration)* "grid" [VExp]? VSetExp
```

## Progam (root)
```ebnf
Program ::= GridDef
```
