#############################################################################
##
#W  other-families-ns.gi
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2017-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################



#############################################################################
##
#P  IsAcuteNumericalSemigroup(s)
##
##  Checks whether or not the nuemrical semigroup s is acute.
##
#############################################################################
InstallMethod(IsAcuteNumericalSemigroup,
  "Tests wheter the semigroup is acute",
  [IsNumericalSemigroup],1,
  function(s)
    local ds, nds;

    ds:=DesertsOfNumericalSemigroup(s);
    nds:=Length(ds);
    if nds<=1 then
      return true;
    fi;

    return Length(ds[nds])<=Length(ds[nds-1]);
  end);

InstallTrueMethod(IsAcuteNumericalSemigroup, IsIrreducibleNumericalSemigroup);

#############################################################################
##
#P  IsOrdinaryNumericalSemigroup(s)
##
##  Checks whether or not the nuemrical semigroup s is acute.
##
#############################################################################
InstallMethod(IsOrdinaryNumericalSemigroup,
  "Tests wheter the semigroup is ordinary",
  [IsNumericalSemigroup],1,
  function(s)
  local ds, nds;

  if HasMultiplicity(s) and HasConductor(s) then
    return Multiplicity(s)=Conductor(s);
  fi;
  ds:=DesertsOfNumericalSemigroup(s);
  nds:=Length(ds);

  return nds<=1;
end);

InstallTrueMethod(IsAcuteNumericalSemigroup, IsOrdinaryNumericalSemigroup);
