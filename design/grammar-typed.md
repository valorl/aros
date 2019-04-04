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
emptyList   ::= "[" "]"
```

## Expressions

### Integer expressions

```ebnf
IExp ::=
    | integer
    | identifier
    | "(" IExp op IExp ")"
```

### Integer list expressions

```ebnf
IListExp ::=
        "[" [IExp ","]* IExp  "]"
        | identifier
        | emptyList
        | "(" IListExp "++" IListExp ")"
```

### Integer set expressions

```ebnf
ISetExp ::=
         "{" [IExp ","]* IExp "}"
        | identifier
        | emptySet
        | "(" ISetExp "<>"  ISetExp ")"
        | "(" ISetExp "><"  ISetExp ")"
```

### Vector expressions

```ebnf
VExp ::=
    | "<" IExp "," IExp ">"
    | identifier
    | "(" VExp op VExp ")"
    | "(" IExp "*" VExp ")"
```

### Vector list expressions

```ebnf
VListExp ::=
        "[" [VExp ","]* VExp  "]"
        | identifier
        | emptyList
        | "(" VListExp "++" VListExp ")"
```

### Vector set expressions

```ebnf
VSetExp ::=
         "{" [VExp ","]* VExp "}"
        | identifier
        | emptySet
        | "(" VSetExp "<>"    VSetExp ")"
        | "(" VSetExp "><"    VSetExp ")"
        | "(" VSetExp ">>"    VExp ")"
        | "(" VSetExp "*"     VExp ")"
        | "(" VSetExp "crop"  VExp ")"
```

## Variable declaration

```ebnf

Declaration ::=
              "int"     identifier "=" IExp
            | "vec"     identifier "=" VExp
            | "{int}"   identifier "=" ISetExp
            | "{vec}"   identifier "=" VSetExp
            | "[int]"   identifier "=" IListExp
            | "[vec]"   identifier "=" VListExp
```

## Grid

### Grid shape (forced to have size vector)

```ebnf
GridDef ::= (Declaration)* | (Declaration)* "grid" VExp? VSetExp
```

## Progam (root)

```ebnf
Program ::= GridDef
```
