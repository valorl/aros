# Type a grammar here:
Prog -> GridDef .

Op -> + .
Op -> - .
Op -> * .
Op -> / .

EmptySet -> { } .

EmptyList -> [ ] .

IExp -> integer .
IExp -> identifier .
IExp -> ( IExp Op IExp ) .

#Helper to simulate IExp comma list
IExpList -> IExp .
IExpList -> IExpList , IExp .

IListExp -> [ IExpList ] .
IListExp -> identifier .
IListExp -> EmptyList .
IListExp -> ( IListExp ++ IListExp ) .

ISetExp -> { IExpList } .
ISetExp -> identifier .
ISetExp -> EmptySet .
ISetExp -> ( ISetExp <> ISetExp ) .
ISetExp -> ( ISetExp >< ISetExp ) .

VExp -> < IExp , IExp > .
VExp -> identifier .
VExp -> ( VExp Op VExp ) .
VExp -> ( VExp ** IExp ) .

#Helper to simulate VExp comma list
VExpList -> VExp .
VExpList -> VExpList , VExp .

VListExp -> [ VExpList ] .
VListExp -> identifier .
VListExp -> EmptyList .
VListExp -> ( VListExp ++ VListExp ) .

VSetExp -> { VExpList } .
VSetExp -> identifier .
VSetExp -> EmptySet .
VSetExp -> ( VSetExp <> VSetExp ) .
VSetExp -> ( VSetExp >< VSetExp ) .
VSetExp -> ( VSetExp * VExp ) .
VSetExp -> ( VSetExp crop VExp ) .

Declaration -> int identifier = IExp .
Declaration -> vec identifier = VExp .
Declaration -> {int} identifier = ISetExp .
Declaration -> {vec} identifier = VSetExp .
Declaration -> [int] identifier = IListExp .
Declaration -> [vec] identifier = VListExp .

DeclList -> Declaration .
DeclList -> DeclList Declaration .

GridDef -> DeclList .
GridDef -> DeclList grid VExp VSetExp .