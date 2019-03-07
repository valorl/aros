## Integers
```python
integer ::= [-]?[1-9][0-9]*
```

## Identifiers
```python
identifier ::= [a-z][a-zA-Z0-9_]*
```

## The `point` keyword
```python
point ::= "point"
```

## Operators
```python
op ::= "+" | "-" | "/" | "*"
```

## Expressions

### Integer expressions
```python
IntExp ::= 
    | integer
    | "(" IntExp ")"
    | IntExp op IExp
```

### Vector expressions
```python
VecExp ::= 
    | Vector
    | "(" VecExp ")"
    | VecExp op VExp
    | IntExp "*" VecExp
    | VecExp "*" IntExp
```

## Vectors
```python
Vector ::= "(" IntExp "," IntExp ")"
```

## Shapes
### Unsized shape
```python
UShape ::= "{" ( (identifier | Shape | point) "at" VecExp )+ "}"
```

### Size vector
```python
SVector ::= "[" IntExp "," IntExp "]"
```

### Sized shape
```python
SShape ::= SVector UShape
```

### General shape
```python
Shape ::= UShape | SShape
```

## Variable declaration
```python
Declaration ::= "var" identifier "=" ( IntExp | VecExp | Shape )
```


## Grid
### Grid shape (forced to have size vector)
```python
GridDef ::= (Declaration)* "grid" SShape
```

## Progam (root)
```python
Program ::= GridDef
```

#### Note
*`Shape` and `Declaration`do **not*** allow brace-less declarations of one-line
shapes in this grammar (I vote let's move this nice-to-have for later)*
