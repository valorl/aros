# this is a comment
Program -> GridDef .

Op -> + .
Op -> - .
Op -> * .
Op -> / .

IExp -> intLiteral .
IExp -> iid .
IExp -> ( intLiteral Op IExp ) .
IExp -> ( id Op IExp ) .

Vector -> ( IExp , IExp ) .

VExp -> Vector .
VExp -> vid .
VExp -> ( IExp x VExp ) .

Shpdecl -> sid at VExp .
Shpdecl -> Shape at VExp .
Shpdecl -> point at VExp .

Mshpdecl ->  Shpdecl .
Mshpdecl -> Shpdecl Mshpdecl .

UShape -> { Mshpdecl } .

SVector -> [ IExp , IExp ] .

SShape -> SVector UShape .

Shape -> UShape .
Shape -> SShape .

iid -> int id .
vid -> vec id .
sid -> shape id .

Declaration -> iid = IExp .
Declaration -> vid = VExp .
Declaration -> sid = Shape .

mDeclaration -> Declaration .
mDeclaration -> Declaration mDeclaration .

GridDef -> mDeclaration grid SShape .
