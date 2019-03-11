Program -> GridDef .

Op -> + .
Op -> - .
Op -> * .
Op -> / .

IExp -> int .
IExp -> id .
IExp -> ( IExp ) .
IExp -> IExp Op IExp .

Vector -> ( IExp , IExp ) .

VExp -> Vector .
VExp -> id .
VExp -> ( VExp ) .
VExp -> IExp * VExp .

shpdecl -> id at VExp .
shpdecl -> Shape at VExp .
shpdecl -> point at VExp .

mshpdecl -> shpdecl .
mshpdecl -> shpdecl mshpdcl .

UShape -> { mshpdecl } .

SVector -> [ IExp , IExp ] .

SShape -> SVector UShape .

Shape -> UShape .
Shape -> SShape .

Declaration -> var id = IExp .
    Declaration -> var id = VExp .
    Declaration -> var id = Shape .

    mDeclaration -> Declaration .
    mDeclaration -> Declaration mDeclaration .

    GridDef -> mDeclaration .
    GridDef -> mDeclaration grid SShape .

