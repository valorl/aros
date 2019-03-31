# Type a grammar here:
Prog -> GridDef .

EmptySet -> { } .

EmptyList -> [ ] .



IExp -> IExpTerm .
IExp -> IExp + IExpTerm .
IExp -> IExp - IExpTerm .

IExpTerm -> IExpFactor .
IExpTerm -> IExpTerm * IExpFactor .
IExpTerm -> IExpTerm / IExpFactor .

IExpFactor -> integer .
IExpFactor -> identifier .
IExpFactor -> ( IExp ) .



#Helper to simulate IExp comma list
IExpList -> IExp .
IExpList -> IExpList , IExp .

BasicIListExp -> [ IExpList ] .
BasicIListExp -> identifier .
BasicIListExp -> EmptyList .
IListExp -> BasicIListExp .
IListExp -> IListExp ++ BasicIListExp .

BasicISetExp -> { IExpList } .
BasicISetExp -> identifier .
BasicISetExp -> EmptySet .
ISetExp -> BasicISetExp .
ISetExp -> ISetExp <> BasicISetExp .
ISetExp -> ISetExp >< BasicISetExp .



VExp -> VExpTerm .
VExp -> VExp + VExpTerm .
VExp -> VExp - VExpTerm .

VExpTerm -> VExpFactor .
VExpTerm -> VExpTerm * VExpFactor .
VExpTerm -> VExpTerm / VExpFactor .

VExpFactor -> < IExp , IExp > .
VExpFactor -> identifier .
VExpFactor -> ( VExp ) .

####    VExp -> VExp ** IExp .



#Helper to simulate VExp comma list
VExpList -> VExp .
VExpList -> VExpList , VExp .

BasicVListExp -> [ VExpList ] .
BasicVListExp -> identifier .
BasicVListExp -> EmptyList .
VListExp -> BasicVListExp .
VListExp -> VListExp ++ BasicVListExp .

BasicVSetExp -> { VExpList } .
BasicVSetExp -> identifier .
BasicVSetExp -> EmptySet .
VSetExp -> BasicVSetExp .
VSetExp -> VSetExp <> BasicVSetExp .
VSetExp -> VSetExp >< BasicVSetExp .
VSetExp -> VSetExp scale VExp .
VSetExp -> VSetExp crop VExp .

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