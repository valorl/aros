
Program -> GridDef .

Op -> + .
Op -> - .
Op -> * .
Op -> / .

IExp -> int .
IExp -> id .
IExp -> ( IExp ) .

OIexp -> IExp .
OIexp -> IExp Op OIexp .

Vector -> ( OIExp , OIExp ) .

VExp -> Vector .
VExp -> id .
VExp -> ( VExp ) .
VExp -> OIExp * OVExp .

Shpdecl -> id at VExp .
Shpdecl -> Shape at VExp .
Shpdecl -> point at VExp .

Mshpdecl -> Shpdecl .
Mshpdecl -> Shpdecl Mshpdcl .

UShape -> { Mshpdecl } .

SVector -> [ OIExp , OIExp ] .

SShape -> SVector UShape .

Shape -> UShape .
Shape -> SShape .

Declaration -> var id = OIExp .
Declaration -> var id = VExp .
Declaration -> var id = Shape .

mDeclaration -> Declaration .
mDeclaration -> Declaration mDeclaration .

GridDef -> mDeclaration .
GridDef -> mDeclaration grid SShape .
