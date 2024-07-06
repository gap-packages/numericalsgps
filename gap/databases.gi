###########################################################################
##
#F  NumericalSemigroupsWithMaxPrimitiveNC(M)
##
## Reads from the database the set of numerical semigroups with
## maximum primitive M (M must be smaller than .. )
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithMaxPrimitiveNC,
	function(M)
	local dir,file,L,pair,s,list;
	dir:=DirectoriesPackageLibrary( "numericalsgps", "data/maxprim");
	list:=EvalString(StringFile(Filename(dir,Concatenation("maxprim",String(M),".gl"))));
	L:=[];
	for pair in list do
	    s := NumericalSemigroup(pair[1]);
	    SetSmallElements(s,pair[2]);
	    Add(L,s);
	od;
	return L;
end);

###########################################################################
##
#F  NumericalSemigroupsWithGenusNC(M)
##
## Reads from the database the set of numerical semigroups with
## genus M (M must be smaller than .. )
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithGenusNC,
	function(M)
	local dir,file,L,pair,s,list;
	dir:=DirectoriesPackageLibrary( "numericalsgps", "data/genus");
	list:=EvalString(StringFile(Filename(dir,Concatenation("genus",String(M),".gl"))));
	L:=[];
	for pair in list do
	    s := NumericalSemigroup(pair[1]);
	    SetSmallElements(s,pair[2]);
	    Add(L,s);
	od;
	return L;
end);

###########################################################################
##
#F  NumericalSemigroupsWithFrobeniusNumberNC(M)
##
## Reads from the database the set of numerical semigroups with
## Frobenius number M (M must be smaller than .. )
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithFrobeniusNumberNC,
	function(M)
	local dir,file,L,pair,s,list;
	dir:=DirectoriesPackageLibrary( "numericalsgps", "data/frobenius");
	list:=EvalString(StringFile(Filename(dir,Concatenation("frobenius",String(M),".gl"))));
	L:=[];
	for pair in list do
	    s := NumericalSemigroup(pair[1]);
	    SetSmallElements(s,pair[2]);
	    Add(L,s);
	od;
	return L;
end);
