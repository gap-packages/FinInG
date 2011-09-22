
# Read("~/pkg/fining/gap/enum_projectivespace.gd");

DeclareGlobalFunction("FinInGFieldNumbering");
DeclareGlobalFunction("FinInGFieldNumberingInverse");
DeclareGlobalFunction("enum_projective");

DeclareOperation("ElementToNumber", [IsSubspaceOfProjectiveSpace]);
DeclareOperation("NumberToElement", [IsPosInt]);