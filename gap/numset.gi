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
  if Length(SmallElements(ns))<80 then
    Print( String(ns) );
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
## string
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
        if Length(x)>3 then
            return Concatenation(String(x[1]),",...,",String(x[Length(x)]));
        else
            return JoinStringsWithSeparator(x, ",");
        fi;
    end;

    L:=SmallElements(ns);
    T:=JoinStringsWithSeparator(List(condensed(L), toString), ",");
    S:=Concatenation("{", T, ",->}");
    return S;
end);

#############################################################################
## display
InstallMethod(Display, "for numerical sets",
[ IsNumericalSet ],
function( ns )
    local   condensed,  L,  M,  u;

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
    ##  End of condensed()  --

    L := SmallElements(ns);
    M := condensed(L);
    u := [M[Length(M)][1],"->"];
    M[Length(M)] := u;
    return M;
end);

#############################################################################
## equality
InstallMethod(\=, "for numerical sets",
[ IsNumericalSet, IsNumericalSet ],
function( ns1, ns2 )
  return SmallElements(ns1) = SmallElements(ns2);
end);

#############################################################################
## ordering
InstallMethod( \<, "for numerical sets",
[ IsNumericalSet, IsNumericalSet ],
function( ns1, ns2 )
  return SmallElements(ns1) < SmallElements(ns2);
end);

#############################################################################
## membership
InstallMethod(\in, "for numerical sets",
[ IsInt, IsNumericalSet ],
function( n, ns ) 
  if n<0 then
    return false;
  fi;
  if n>Conductor(ns) then
    return true;
  fi;
  return n in SmallElements(ns);
end);

#############################################################################
## inclusion
InstallMethod( IsSubset, "for numerical sets",
[ IsNumericalSet, IsNumericalSet ],
function( ns1, ns2 )
    if Conductor(ns1) > Conductor(ns2) then
        return false;
    fi;
    return ForAll(SmallElements(ns2), j-> (j>Conductor(ns1)) or (j in ns1));
end);

#############################################################################
## difference
InstallMethod( Difference, "for numerical sets",
[ IsNumericalSet, IsNumericalSet ],
function( ns1, ns2 )
  local s1,s2,m1,m2,m,sm1,sm2;
  s1:=SmallElements(ns1);
  s2:=SmallElements(ns2);
  m1:=Conductor(ns1);
  m2:=Conductor(ns2);
  m:=Maximum([m1,m2]);
  sm1:=Union(s1, [m1..m]);
  sm2:=Union(s2, [m2..m]);
  return Difference(sm1, sm2);
end);