Program -> GridDef .

Op -> + .
Op -> - .
Op -> * .
Op -> / .

Operand -> int .
Operand -> id .

IExp -> Operand .
IExp -> ( Operand Op IExp ) .

Vector -> ( IExp , IExp ) .

VExp -> Vector .
VExp -> id .
VExp -> ( IExp * VExp ) .

Shpdecl -> id at VExp .
Shpdecl -> Shape at VExp .
Shpdecl -> point at VExp .

Mshpdecl ->  Shpdecl ; .
Mshpdecl -> Shpdecl , Mshpdecl .

UShape -> { Mshpdecl } .

SVector -> [ IExp , IExp ] .

SShape -> SVector UShape .

Shape -> UShape .
Shape -> SShape .

Declaration -> var id = OIExp .
Declaration -> var id = VExp .
Declaration -> var id = Shape .


mDeclaration -> Declaration ; .
mDeclaration -> Declaration , mDeclaration .

GridDef -> mDeclaration grid SShape .