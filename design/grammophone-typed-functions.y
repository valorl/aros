# Type a grammar here:
Prog -> GridDef .

EmptySet -> { } .

EmptyList -> [ ] .

IdList -> identifier .
IdList -> IdList , identifier .

#==== Type, Func defs ====

Type -> int .
Type -> vec .
Type -> [int] .
Type -> [vec] .
Type -> {int} .
Type -> {vec} .

TypeList -> Type .
TypeList -> TypeList , Type .

#we cannot pass a function that takes a function
#not because it does not work but it seems like a lot of hassle
FuncType -> ( TypeList => Type ) .

FOrTList -> Type .
FOrTList -> FuncType .
FOrTList -> FOrTList , Type .
FOrTList -> FOrTList , FuncType .

IFuncDef -> func ( FOrTList => int ) identifier = ( IdList ) => { IExp } .
VFuncDef -> func ( FOrTList => vec ) identifier = ( IdList ) => { VExp } .
ILFuncDef -> func ( FOrTList => [int] ) identifier = ( IdList ) => { IListExp } .
VLFuncDef -> func ( FOrTList => [vec] ) identifier = ( IdList ) => { VListExp } .
ISFuncDef -> func ( FOrTList => {int} ) identifier = ( IdList ) => { ISetExp } .
VSFuncDef -> func ( FOrTList => {vec} ) identifier = ( IdList ) => { VSetExp } .


PossibleFuncParameter -> integer .
PossibleFuncParameter -> < integer , integer > .
PossibleFuncParameter -> identifier .
PossibleFuncParameter -> < identifier , identifier > .

FuncParameterList -> PossibleFuncParameter .
FuncParameterList -> FuncParameterList , PossibleFuncParameter .

FuncAppl -> identifier ( FuncParameterList ) .


#=== End Type, Func defs ====

IExp -> IExpTerm .
IExp -> IExp + IExpTerm .
IExp -> IExp - IExpTerm .

IExpTerm -> IExpFactor .
IExpTerm -> IExpTerm * IExpFactor .
IExpTerm -> IExpTerm / IExpFactor .

IExpFactor -> integer .
IExpFactor -> identifier .
IExpFactor -> FuncAppl . 
IExpFactor -> ( IExp ) .

IExpList -> IExp .
IExpList -> IExpList , IExp .

BasicIListExp -> [ IExpList ] .
BasicIListExp -> identifier .
BasicIListExp -> FuncAppl .
BasicIListExp -> EmptyList .
IListExp -> BasicIListExp .
IListExp -> IListExp ++ BasicIListExp .

BasicISetExp -> { IExpList } .
BasicISetExp -> identifier .
BasicISetExp -> FuncAppl .
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
VExpFactor -> FuncAppl .
VExpFactor -> ( VExp ) .

VExp -> VExp ** IExpFactor .

VExpList -> VExp .
VExpList -> VExpList , VExp .

BasicVListExp -> [ VExpList ] .
BasicVListExp -> identifier .
BasicVListExp -> EmptyList .
BasicVListExp -> FuncAppl .
VListExp -> BasicVListExp .
VListExp -> VListExp ++ BasicVListExp .

BasicVSetExp -> { VExpList } .
BasicVSetExp -> identifier .
BasicVSetExp -> FuncAppl .
BasicVSetExp -> EmptySet .
VSetExp -> BasicVSetExp .
VSetExp -> VSetExp <> BasicVSetExp .
VSetExp -> VSetExp >< BasicVSetExp .
VSetExp -> VSetExp >> VExp .
VSetExp -> VSetExp scale VExp .
VSetExp -> VSetExp crop VExp .


# we can do int myid = myfunc(3,4) because IExpFactor can be a FuncAppl

Declaration -> IFuncDef .
Declaration -> VFuncDef .
Declaration -> ILFuncDef .
Declaration -> VLFuncDef .
Declaration -> ISFuncDef .
Declaration -> VSFuncDef .
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
GridDef -> DeclList grid VSetExp .


