```
int -> [-]?[1-9][0-9]*

//Point is a reserved keyword equiv. to (0, 0)
identifier ->
  [a-z][a-zA-Z0-9_]*
| "Point"


// can do 7 + 5 or x + y or (1,2)+(3,4)
Factor = int | identifier | Vector

operator -> "+" | "-" | "/" | "*"

AOperation ->
           Factor
           | MOperation
           | Factor ('+'|'-') (AOperation | MOperation)
           | Factor ('+'|'-') '('(AOperation | MOperation)')'

MOperation ->
           Factor
           | Factor ('*'|'/') MOperation
           | Factor ('*'|'/') '('MOperation')'


// (5+3, 6/2) or so that we can do hor_line at (3,2) + (1,4)
Vector ->
  "(" AOperation "," AOperation ")"
| AOperation

// I thought about defining arrays but I think that doesn't make sense
// you just define the shape and that's that
// it makes sense to fold on a shape
// enables anonymous shapes inside the shape
Shape -> "[" ((identifier | Shape) "at" Vector)+ "]"

Definition -> "var" identifier "=" (
 (Vector | identifier)? Shape
| AOperation
)

Griddef -> Definition | Definition Griddef


//temporarily until we start adding more functionality
Program -> Griddef
```