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
IExp -> ( IExp ) .
IExp -> IExp Op integer .
IExp -> IExp Op identifier .

#Helper to simulate IExp comma list
IExpList -> IExp .
IExpList -> IExpList , IExp .

BasicIListExp -> [ IExpList ] .
BasicIListExp -> identifier .
BasicIListExp -> EmptyList .

IListExp -> BasicIListExp .
IListExp -> ( IListExp ) .
IListExp -> IListExp ++ BasicIListExp .

BasicISetExp -> { IExpList } .
BasicISetExp -> identifier .
BasicISetExp -> EmptySet .

ISetExp -> BasicISetExp .
ISetExp -> ( ISetExp ).
ISetExp -> ISetExp <> BasicISetExp .
ISetExp -> ISetExp >< BasicISetExp .


Vector -> < IExp , IExp > .
Vector -> identifier .

VExp -> Vector .
VExp -> ( VExp ) .
VExp -> VExp Op Vector .
#VExp -> VExp ** IExp .

#Helper to simulate VExp comma list
VExpList -> VExp .
VExpList -> VExpList , VExp .

BasicVListExp -> [ VExpList ] .
BasicVListExp -> identifier .
BasicVListExp -> EmptyList .

VListExp -> BasicVListExp .
VListExp -> ( VListExp ) .
VListExp -> VListExp ++ BasicVListExp .

BasicVSetExp -> { VExpList } .
BasicVSetExp -> identifier .
BasicVSetExp -> EmptySet .

VSetExp -> BasicVSetExp .
VSetExp -> ( VSetExp ) .
VSetExp -> VSetExp <> BasicVSetExp .
VSetExp -> VSetExp >< BasicVSetExp .
VSetExp -> VSetExp ** VExp .
VSetExp -> VSetExp crop VExp .

Declaration -> int identifier = IExp .
Declaration -> vec identifier = VExp .
Declaration -> {int} identifier = ISetExp .
Declaration -> {vec} identifier = VSetExp .
Declaration -> [int] identifier = IListExp .
Declaration -> [vec] identifier = VListExp .

DeclList -> Declaration .
DeclList -> DeclList Declaration .

GridDef -> DeclList eof .
GridDef -> DeclList grid VExp VSetExp eof .