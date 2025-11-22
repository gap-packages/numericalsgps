#############################################################################
##
#F  NumericalSetBySmallElements(L)
##
##  Returns the numerical set specified by L,
##  the list of small elements of the numerical set.
##
#############################################################################
InstallGlobalFunction(NumericalSetBySmallElements,
function(L)
  local ns,ls,ln;
  if not IsListOfIntegersNS(L) or Minimum(L)<0 then
    return Error("The argument must be a nonempty list of non-negative integers");
  fi;

  if not 0 in L then
    return Error("The list of small elements must contain 0");
  fi;
  ls:=List(Set(L));
  ln:=Length(ls);
  # Remove all the elements greater than or equal to the potential conductor
  while (ln>1) and (ls[ln]=ls[ln-1]+1) do
      Remove(ls,ln);
      ln:=ln-1;
  od;

  ns:= Objectify( NumericalSetsType, rec() );
  SetSmallElements(ns, ls);
  return ns;
end);

#############################################################################
##
#F  NumericalSetByGaps(L)
##
##  Returns the numerical set specified by L,
##  the list of gaps of the numerical set.
##
#############################################################################
InstallGlobalFunction(NumericalSetByGaps,
function(L)
  local ns,gs,c;
  if not IsListOfIntegersNS(L) or Minimum(L)<0 then
    return Error("The argument must be a nonempty list of positive integers");
  fi;
  gs:=List(Set(L));
  c:=Maximum(gs)+1;
  ns:= Objectify( NumericalSetsType, rec() );
  SetGaps(ns, gs);
  SetSmallElements(ns, Difference([0..c], gs));
  return ns;
end);

#############################################################################
##
#O Gaps(ns)
## Gaps of a numerical set
##
###############################################################################
InstallMethod( Gaps, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  local c;
  c:= Maximum(SmallElements(ns));
  return Difference( [0..c], SmallElements(ns) );
end);

#############################################################################
##
##O Conductor(ns)
## Conductor of a numerical set
##
#############################################################################
InstallMethod( Conductor, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  return Maximum(SmallElements(ns));
end);

#############################################################################
##
##O FrobeniusNumber(ns)
## Frobenius number of a numerical set
##
#############################################################################
InstallMethod( FrobeniusNumber, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  return Conductor(ns)-1;
end);

#############################################################################
##
##O Genus(ns)
## Genus of a numerical set
##
###############################################################################
InstallMethod( Genus, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  return Length( Gaps(ns) );
end);

#############################################################################
##O Multiplicity(ns)
## Multiplicity of a numerical set
###############################################################################
InstallMethod( Multiplicity, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  if Length(SmallElements(ns))=1 then # ns = {0,1,2,...}
    return 1;
  fi;
  return First(SmallElements(ns), x -> x > 0);
end);

#############################################################################
## printing
InstallMethod( PrintObj, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  if Length(SmallElements(ns))<20 then
    Print( "Numerical set with small elements ", SmallElements(ns) );
    return;
  fi;
  Print( "Numerical set with " , Length(SmallElements(ns)), " small elements" );
end);

# InstallMethod( String, "for numerical sets",
# [ IsNumericalSet ],
# function( ns )
#   if Length(SmallElements(ns))<20 then
#     return(Concatenation("Numerical set with small elements ", String(SmallElements(ns))));
#   fi;
#   return(Concatenation("Numerical set with " , String(Length(SmallElements(ns))), " small elements" ));
# end);


#############################################################################
## viewing
InstallMethod( ViewObj, "for numerical sets",
[ IsNumericalSet ],
function( ns )
  Print("<Numerical set>");
end);

InstallMethod( ViewString, "for numerical sets",
[ IsNumericalSet ],
function( ns ) 
  return "Numerical set";
end);

#############################################################################
## display
InstallMethod( String, "for numerical sets",
[ IsNumericalSet ],
function( ns )
    local   L, S, condensed, T, toString;

      condensed := function(L)
        local   C,  bool,  j,  c,  search;

        C := [];
        bool := true;
        j := 0;
        c := L[1];
        search := function(n) # searches the greatest subinterval starting in n
            local i, k;
            k := 0;
            for i in [Position(L,n).. Length(L)-1] do
                if not (L[i]+1 = L[i+1]) then
                    c := L[i+1];
                    return [n..n+k];
                fi;
                k := k+1;
            od;
            bool := false;
            return [n..L[Length(L)]];
        end;
        while bool do
            Add(C,search(c));
        od;
        return C;
    end;

    toString := function(x)
        if Length(x)>2 then
            return Concatenation(String(x[1]),",...,",String(x[Length(x)]));
        elif Length(x)=2 then
            return Concatenation(String(x[1]),",",String(x[2]));
        else
            return String(x[1]);
        fi;
    end;

    L:=SmallElements(ns);
    T:=Concatenation(List(condensed(L), toString));
    S:=Concatenation("{", T, ",->}");
    return S;
end);