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

VExp -> VExp ** IExpFactor .


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
VSetExp -> VSetExp >> VExp .
VSetExp -> VSetExp scale VExp .
VSetExp -> VSetExp crop VExp .


IdList -> identifier .
IdList -> IdList , identifier .

IFuncDef -> def func int ( \ IdList in IExp ) .
VFuncDef -> def func vec ( \ IdList in VExp ) .
ISFuncDef -> def func {int} ( \ IdList in ISetExp ) .
VSFuncDef -> def func {vec} ( \ IdList in VSetExp ) .
ILFuncDef -> def func [int] ( \ IdList in IListExp ) .
VLFuncDef -> def func [vec] ( \ IdList in VListExp ) .


Declaration -> int identifier = IExp .
Declaration -> int identifier = IFuncDef .
Declaration -> vec identifier = VExp .
Declaration -> vec identifier = VFuncDef .
Declaration -> {int} identifier = ISetExp .
Declaration -> {int} identifier = ISFuncDef .
Declaration -> {vec} identifier = VSetExp .
Declaration -> {vec} identifier = VSFuncDef .
Declaration -> [int] identifier = IListExp .
Declaration -> [int] identifier = ILFuncDef .
Declaration -> [vec] identifier = VListExp .
Declaration -> [vec] identifier = VLFuncDef .

DeclList -> Declaration .
DeclList -> DeclList Declaration .

GridDef -> DeclList .
GridDef -> DeclList grid VExp VSetExp .
GridDef -> DeclList grid VSetExp .


