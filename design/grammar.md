```
int -> [-]?[1-9][0-9]*

//Point is a reserved keyword equiv. to [(0, 0)]
identifier ->
  [a-z][a-zA-Z0-9_]*

pointIdent -> "Point"

Factor = int | identifier | Vector | pointIdent

operator -> "+" | "-" | "/" | "*"

Expression ->
           Factor
           | Factor operator Expression
           | Factor operator '('Expression')'

Vector ->
         "(" AOperation "," AOperation ")"
         | AOperation

Shape -> "{" ((identifier | Shape) "at" Vector)+ "}"

Definition -> "var" identifier "=" (
           (Vector | identifier)? Shape
           | AOperation
)

Griddef -> Definition | Definition Griddef

Program -> Griddef
```
