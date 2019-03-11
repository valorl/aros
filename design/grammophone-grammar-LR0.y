Program -> GridDef .

Op -> + .
Op -> - .
Op -> * .
Op -> / .

IExp -> intLiteral .
IExp -> id .
IExp -> ( intLiteral Op IExp ) .
IExp -> ( id Op IExp ) .

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

Declaration -> int id = IExp .
Declaration -> vec id = VExp .
Declaration -> shape id = Shape .

mDeclaration -> Declaration ; .
mDeclaration -> Declaration , mDeclaration .

GridDef -> mDeclaration grid SShape .