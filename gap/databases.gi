###########################################################################
##
#F  NumericalSemigroupsWithMaxPrimitivePC(M)
##
## Reads from the Pre-Computed database the set of numerical semigroups with
## maximum primitive M
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithMaxPrimitivePC,
	function(M)
  local dir,max,file,L,pair,s,list;
  dir:=DirectoriesPackageLibrary( "numericalsgps", "data/maxprim");
  # max is the maximum M stored in the database
  max := Length(DirectoryContents(dir[1]))-2; # the -2 is to not count "." and ".."

  if M > max then
    Info(InfoNumSgps,1,"The set of numerical semigroups with maximum primitive ",M," is not in the database in use. If the maximum primitive you are interested in is still reasonably small, please send an email to mdelgado@fc.up.pt for an enlarged database, or use the (much slower function) NumericalSemigroupsWithMaxPrimitive");
  else
    list:=EvalString(StringFile(Filename(dir,Concatenation("maxprim",String(M),".gl"))));
    L:=[];
    for pair in list do
      s := NumericalSemigroup(pair[1]);
      SetSmallElements(s,pair[2]);
      Add(L,s);
    od;
    return L;
  fi;
end);

###########################################################################
##
#F  NumericalSemigroupsWithGenusPC(M)
##
## Reads from the database the set of numerical semigroups with
## genus M (M must be smaller than .. )
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithGenusPC,
	function(M)
  local dir,max,file,L,pair,s,list;
  dir:=DirectoriesPackageLibrary( "numericalsgps", "data/genus");
  # max is the maximum M stored in the database
  max := Length(DirectoryContents(dir[1]))-2; # the -2 is to not count "." and ".."

  if M > max then
    Info(InfoNumSgps,1,"The set of numerical semigroups with genus ",M," is not in the database in use. If the genus you are interested in is still reasonably small, please send an email to mdelgado@fc.up.pt for an enlarged database, or use the (much slower function) NumericalSemigroupsWithMaxPrimitive");
  else
    list:=EvalString(StringFile(Filename(dir,Concatenation("genus",String(M),".gl"))));
    L:=[];
    for pair in list do
      s := NumericalSemigroup(pair[1]);
      SetSmallElements(s,pair[2]);
      Add(L,s);
    od;
    return L;
  fi;
end);

###########################################################################
##
#F  NumericalSemigroupsWithFrobeniusNumberPC(M)
##
## Reads from the database the set of numerical semigroups with
## Frobenius number M (M must be smaller than .. )
#############################################################################
InstallGlobalFunction(NumericalSemigroupsWithFrobeniusNumberPC,
	function(M)
  local dir,max,file,L,pair,s,list;
  dir:=DirectoriesPackageLibrary( "numericalsgps", "data/frobenius");
  # max is the maximum M stored in the database
  max := Length(DirectoryContents(dir[1]))-2; # the -2 is to not count "." and ".."

  if M > max then
    Info(InfoNumSgps,1,"The set of numerical semigroups with Frobenius number ",M," is not in the database in use. If the Frobenius number you are interested in is still reasonably small, please send an email to mdelgado@fc.up.pt for an enlarged database, or use the (much slower function) NumericalSemigroupsWithMaxPrimitive");
  else
    list:=EvalString(StringFile(Filename(dir,Concatenation("frobenius",String(M),".gl"))));
    L:=[];
    for pair in list do
      s := NumericalSemigroup(pair[1]);
      SetSmallElements(s,pair[2]);
      Add(L,s);
    od;
    return L;
  fi;
end);
