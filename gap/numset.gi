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
  if not( IsList(L) ) then
    return Error("The argument must be a list of positive integers");
  fi;
  gs:=List(Set(L));
  if gs=[] then
    return NumericalSetBySmallElements([0]);
  fi;
  if not IsListOfIntegersNS(L) or Minimum(L)<0 then
    return Error("The argument must be a list of positive integers");
  fi;
  c:=Maximum(gs)+1;
  ns:= Objectify( NumericalSetsType, rec() );
  SetGaps(ns, gs);
  SetSmallElements(ns, Difference([0..c], gs));
  return ns;
end);

#############################################################################
##
#O AsNumericalSet( S )
##
## Returns S as a numerical set.
##
#############################################################################
InstallMethod( AsNumericalSet ,
[ IsNumericalSemigroup ],
function( S )
  return NumericalSetBySmallElements( SmallElements(S) );
end);

InstallMethod( AsNumericalSet ,
[ IsIdealOfNumericalSemigroup ],
function( I )
  if Minimum(I)<>0 then
    return Error("The minimum of ideal must be 0 to be converted into a numerical set");
  fi;
  return NumericalSetBySmallElements( SmallElements(I) );
end);

#############################################################################
##
#O AsNumericalSemigroup( S )
##
## Returns S as a numerical semigroup if s+s=s; raises an error otherwise.
##
#############################################################################
InstallMethod(AsNumericalSemigroup,"for numerical sets",[IsNumericalSet],
  function(s)
  if s+s=s then 
    return NumericalSemigroupBySmallElements(SmallElements(s));
  fi;
  Error("The given numerical set is not a numerical semigroup");
end);

#############################################################################
##
#O AsIdealOfNumericalSemigroup( R, S )
##
## Returns R as an ideal of the numerical semigroup S if R+S=R; 
## raises an error otherwise.
##
#############################################################################
# InstallMethod(AsIdealOfNumericalSemigroup, "for numerical sets", [IsNumericalSet,IsNumericalSemigroup]
# function (r,s)
#   if r+s=r then
#     return IdealOfNumericalSemigroupBySmallElements(SmallElements(r),s);
# end);

#############################################################################
##
#O IsAssociatedNumericalSetOfNumericalSemigroup( R, S )
##
## Checks if R is an associated set of S, that is,  R is an ideal and R-R=S
##
#############################################################################
InstallMethod(IsAssociatedNumericalSetOfNumericalSemigroup, [IsNumericalSet,IsNumericalSemigroup],
function(r,s)
  local i;
  if r+s=r then # r is an ideal of s
    i:=IdealOfNumericalSemigroupBySmallElements(SmallElements(r),s);
    return i-i=s;
  fi;
  return false;
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

InstallMethod(\=, "for numerical semigroups and numerical sets",
[ IsNumericalSemigroup, IsNumericalSet ],
function( ns1, ns2 )
  return SmallElements(ns1) = SmallElements(ns2);
end);

InstallMethod(\=, "for numerical sets and numerical semigroups",
[ IsNumericalSet, IsNumericalSemigroup ],
function( ns1, ns2 )
  return SmallElements(ns1) = SmallElements(ns2);
end);

InstallMethod(\=, "for numerical sets and ideals of numerical semigroups",
[ IsNumericalSet, IsIdealOfNumericalSemigroup ],
function( ns, i )
  if Minimum(i)<>0 then
    return false;
  fi;
  return SmallElements(ns) = SmallElements(i);
end);

InstallMethod(\=, "for ideals of numerical semigroups and numerical sets",
[ IsIdealOfNumericalSemigroup, IsNumericalSet ],
function( i, ns )
  return ns=i;
end);


#############################################################################
## ordering
InstallMethod( \<, "for numerical sets",
[ IsNumericalSet, IsNumericalSet ],
function( ns1, ns2 )
  return SmallElements(ns1) < SmallElements(ns2);
end);

InstallMethod( \<, "for numerical sets and numerical semigroups",
[ IsNumericalSet, IsNumericalSemigroup ] ,
function( ns1, ns2 )
  return SmallElements(ns1) < SmallElements(ns2);
end);

InstallMethod( \<, "for numerical semigroups and numerical sets",
[ IsNumericalSemigroup, IsNumericalSet ] ,
function( ns1, ns2 )
  return SmallElements(ns1) < SmallElements(ns2);
end);

InstallMethod( \<, "for numerical sets and ideals of numerical semigroups",
[ IsNumericalSet, IsIdealOfNumericalSemigroup ] ,
function( ns, I )
  return SmallElements(ns) < SmallElements(I);
end);

InstallMethod( \<, "for ideals of numerical semigroups and numerical sets",
[ IsIdealOfNumericalSemigroup, IsNumericalSet ] ,
function( I, ns )
  return SmallElements(I) < SmallElements(ns);
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

InstallMethod( IsSubset, "for numerical sets and numerical semigroups",
[ IsNumericalSet, IsNumericalSemigroup ],
function( ns1, ns2 )
    if Conductor(ns1) > Conductor(ns2) then
        return false;
    fi;
    return ForAll(SmallElements(ns2), j-> (j>Conductor(ns1)) or (j in ns1));
end);

InstallMethod( IsSubset, "for numerical semigroups and numerical sets",
[ IsNumericalSemigroup, IsNumericalSet ],
function( ns1, ns2 )
    if Conductor(ns1) > Conductor(ns2) then
        return false;
    fi;
    return ForAll(SmallElements(ns2), j-> (j>Conductor(ns1)) or (j in ns1));
end);

InstallMethod( IsSubset, "for numerical sets and ideals of numerical semigroups",
[ IsNumericalSet, IsIdealOfNumericalSemigroup ],
function( ns, I )
    if Conductor(ns) > Conductor(I) then
        return false;
    fi;
    return ForAll(SmallElements(I), j-> (j>Conductor(ns)) or (j in ns));
end);

InstallMethod( IsSubset, "for ideals of numerical semigroups and numerical sets", 
[ IsIdealOfNumericalSemigroup, IsNumericalSet ],
function( I, ns )
    if Conductor(I) > Conductor(ns) then
        return false;
    fi;
    return ForAll(SmallElements(ns), j-> (j>Conductor(I)) or (j in I));
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

InstallMethod( Difference, "for numerical sets and lists",
[ IsNumericalSet, IsList ],
function( ns, l )
  local gs;
  if (0 in l) then
    Error("The list of integers must not contain 0");
  fi;
  gs:=Gaps(ns);
  return NumericalSetByGaps(Union(gs, Filtered(l, IsPosInt)));
 end);

InstallMethod( Difference, "for lists and numerical sets",
[ IsList, IsNumericalSet ],
function( l, ns )
  return Filtered(l, x-> not (x in ns) );
end);

InstallMethod( Difference, "for numerical semigroups and lists",
[ IsNumericalSemigroup, IsList ],
function( ns, l )
  return Difference( AsNumericalSet(ns), l );
end);


##################################################################################
##
#O ns[n]
## The nth element of ns
##################################################################################

InstallOtherMethod(\[\], [IsNumericalSet,IsInt],
function(ns,n)
  local sm,nsm;
  if n<=0 then 
    Error("The index must be a positive integer");
  fi;
  sm:=SmallElements(ns);
  nsm:=Length(sm);
  if n<=nsm then
    return sm[n];
  else
    return Conductor(ns)-nsm+n;
  fi;
end);

##################################################################################
##
#O ns{ls}
## [I[n] :  n in ls]
##################################################################################


InstallOtherMethod(\{\}, [IsNumericalSet,IsList],
function(i,l)
    return List(l,n->i[n]);
end);

##################################################################################
##
#O Positions(s,n)
#O Position(s,n)
## 
## Determines the position of n in s as a list
##
#################################################################################
InstallOtherMethod(PositionsOp, "for numerical sets", [IsNumericalSet,IsObject],
function(s,n)
  local c,sm;
  if not(IsInt(n)) then
    return [];
  fi;
  c:=Conductor(s);
  sm:=SmallElements(s);
  if n>=c then 
    return [n-c+Length(sm)];
  fi;
  return Positions(sm,n);
end);

InstallOtherMethod(Position, [IsNumericalSet,IsObject,IsInt],
function(s,n,f)
  local c,sm,p;
  if not(IsInt(n)) then
    return fail;
  fi;

  c:=Conductor(s);
  sm:=SmallElements(s);
  if n>=c then 
    p:=n-c+Length(sm);
    if p > f then 
      return p;
    else
      return fail;
    fi;
  fi;
  return Position(sm,n,f);
end);



##################################################################################
##
#O Union(ns1,ns2)
## Union of the numerical sets ns1 and ns2
##
##################################################################################
InstallMethod(Union2, [IsNumericalSet,IsNumericalSet], 
function(ns1,ns2)
 local sm1,sm2,c1,c2,c;

 c1:=Conductor(ns1);
 c2:=Conductor(ns2);
 c:=Maximum(c1,c2);
 sm1:=Union(SmallElements(ns1),[c1..c]);
 sm2:=Union(SmallElements(ns2),[c2..c]);
 return NumericalSetBySmallElements(Union(sm1,sm2));
end);

### Unions of numerical sets and numerical semigroups

InstallMethod(Union2, [IsNumericalSet,IsNumericalSemigroup], 
function(ns1,ns2)
  return Union(ns1,AsNumericalSet(ns2));
end);

InstallMethod(Union2, [IsNumericalSemigroup,IsNumericalSet], 
function(ns1,ns2)
  return Union(AsNumericalSet(ns1),ns2);
end);

InstallMethod(Union2, [IsNumericalSemigroup,IsNumericalSemigroup], 
function(ns1,ns2)
  return Union(AsNumericalSet(ns1),AsNumericalSet(ns2));
end);

### Unions of numerical sets (and numerical semigroups) and lists of integers

InstallMethod(Union2, [IsNumericalSet,IsList], 
function(ns,l)
  local sm, c;
  if not( IsListOfIntegersNS(l) ) then
    return Error("The second argument must be a list of non-negative integers");
  fi;
  if not( ForAll(l, x-> x>=0) ) then
    return Error("The list must contain only non-negative integers");
  fi;
  c:=Conductor(ns);
  sm:=Union(SmallElements(ns), Filtered(l, x-> x<c) );
  return NumericalSetBySmallElements(sm);
end);

InstallMethod(Union2, [IsList,IsNumericalSet], 
function(l,ns)
  local sm, c;
  if not( IsListOfIntegersNS(l) ) then
    return Error("The first argument must be a list of non-negative integers");
  fi;
  if not( ForAll(l, x-> x>=0) ) then
    return Error("The list must contain only non-negative integers");
  fi;
  c:=Conductor(ns);
  sm:=Union(SmallElements(ns), Filtered(l, x-> x<c) );
  return NumericalSetBySmallElements(sm);
end);

InstallMethod(Union2, [IsNumericalSemigroup,IsList], 
function(ns,l)
  return Union(AsNumericalSet(ns),l);
end);

InstallMethod(Union2, [IsList,IsNumericalSemigroup], 
function(l,ns)
  return Union(l,AsNumericalSet(ns));
end);

##################################################################################
##
#O Intersection(ns1,ns2)
## Intersection of the numerical sets ns1 and ns2
##
##################################################################################
InstallMethod(Intersection2, [IsNumericalSet,IsNumericalSet], 
  function(ns1,ns2)
  return NumericalSetByGaps(Union(Gaps(ns1),Gaps(ns2)));
end);


InstallMethod(Intersection2, [IsNumericalSemigroup,IsNumericalSet], 
  function(ns1,ns2)
  return Intersection(AsNumericalSet(ns1),ns2);
end);

InstallMethod(Intersection2, [IsNumericalSet,IsNumericalSemigroup], 
  function(ns1,ns2)
  return Intersection(ns2,ns1);
end);

##################################################################################
##
#O \+(ns1,ns2)
## Sum of the numerical sets ns1 and ns2
##
##################################################################################
InstallMethod(\+, [IsNumericalSet,IsNumericalSet], 
function(ns1,ns2)
  local sm1,sm2,c1,c2,c;

  c1:=Conductor(ns1);
  c2:=Conductor(ns2);
  c:=c1+c2;
  sm1:=Union(SmallElements(ns1),[c1..c]);
  sm2:=Union(SmallElements(ns2),[c2..c]);
  return NumericalSetBySmallElements(Set(Cartesian(sm1,sm2),Sum));
end);

InstallMethod(\+, [IsNumericalSemigroup,IsNumericalSet], 
function(ns1,ns2)
  return AsNumericalSet(ns1)+ns2;
end);

InstallMethod(\+, [IsNumericalSet,IsNumericalSemigroup], 
function(ns1,ns2)
  return ns2+ns1;
end);


#############################################################################
##
#O IntegerPartition(S)
##
## Returns the integer partition associated to the numerical set S
## S can also be a numerical semigroup
## Written in collaboration with M. Yeşil
###############################################################################
InstallMethod(IntegerPartition, [IsNumericalSet],
function(S)
  local ns, yd, i, ri, k, P;

  ns := SmallElements(S);

  i := Length(ns);
  P := [];
  while i > 1 do
    yd := [];
    ri := ns[i] - ns[i-1] - 1;
    for k in [1..ri] do
      yd[k] := i-1;
    od;
    i := i - 1;
    Append(P, yd);
  od;
  return P;
end);

InstallMethod(IntegerPartition, [IsNumericalSemigroup],
function(S)
  return IntegerPartition(AsNumericalSet(S));
end);

#############################################################################
##
#F NumericalSetByIntegerPartition(part)
##
## Returns the numerical set associated to the integer partition part
## Written in collaboration with M. Yeşil
#################################################################################
InstallGlobalFunction(NumericalSetByIntegerPartition,
function(P)
  local i, j, n, ns, aux, p;
 
  if P=[] then
    return NumericalSetBySmallElements([0]);
  fi;

  if not IsListOfIntegersNS(P) then 
    Error("The argument must be a list of non-negative integers");
  fi;

  if P=[] then
    return NumericalSetBySmallElements([0]);
  fi;
  if not( ForAll(P, IsPosInt) and ForAll([1..Length(P)-1], i-> P[i]>=P[i+1]) ) then
    Error("The integers in the partition must be positive and in non-increasing order");
  fi;

  p := ShallowCopy(P);
  n := Length(p);
  i := p[n];
  ns := [0];
  if i>1 then
  Append(ns, [1..i-1]);
  fi;
  j := 1;
  while j < n do
    aux := p[n-j] - p[n-j+1];
    if aux = 0 then
      i := i + 1;
    else
      Append(ns, [i+1..i+aux]);
      i := i + aux+1;
    fi;
    j := j + 1;
  od;
  Add(ns, i+1);
  return NumericalSetBySmallElements(ns);
end);

#############################################################################
##
#O HookLengths(S)
## Returns a list of lists with the hook lengths of the integer partition
## associated to the numerical set S
## S can also be a numerical semigroup
## Written in collaboration with M. Yeşil
###############################################################################
InstallMethod(HookLengths, [IsNumericalSet],
function(S)
  local ns, i, hooks, n, f, mset, k, a, aux;
  ns := SmallElements(S);
  mset := IntegerPartition(S);
  k := Length(mset);
  n := Length(ns);
  f := ns[n]-1;
  hooks := [];
  hooks[1] := Filtered(f - ns, m -> m > 0);
  for i in [2..k] do
    a := mset[i-1] - mset[i] + 1;
    aux := hooks[i-1]-a;
    hooks[i] := Filtered(aux, m -> m > 0);
  od;
  return hooks;
end);

InstallMethod(HookLengths, [IsNumericalSemigroup],
function(S)
  return HookLengths(AsNumericalSet(S));
end);


#############################################################################
##
#O DualNumericalSet(S)
## Returns the dual numerical set of S
## S can also be a numerical semigroup
## Written in collaboration with M. Yeşil
###############################################################################
InstallMethod(DualNumericalSet, [IsNumericalSet],
function(S)
  local numset, dual, f, c, l, g, gaps;
  numset := SmallElements(S);
  l:= Length(numset);
  c:= Conductor(S);
  f:= FrobeniusNumber(S);
  gaps := Gaps(S);
  g:= Genus(S);
  dual := List([1..g], i -> f - gaps[g - i + 1]);
  Add(dual,c);
  return NumericalSetBySmallElements(dual);
end);

InstallMethod(DualNumericalSet, [IsNumericalSemigroup],
function(S)
  return DualNumericalSet(AsNumericalSet(S));
end);

#############################################################################
##
#O BondedSum(S,T)
## Returns the bonded sum of the numerical sets (or semigroups) S and T
## as defined in [Mehmet2025]
## Written in collaboration with M. Yeşil
###############################################################################
InstallMethod(BondedSum, [IsNumericalSet, IsNumericalSet],
function(S,T)
  local ls, lt, k1, k2;
  ls := SmallElements(S);
  lt := SmallElements(T);
  k1 := ls[Length(ls)];
  k2 := lt[Length(lt)];
  if ls=[0] or lt=[0] then
    Error("The arguments cannot be the set of non-negative integers");
  fi;
  #Remove(ls,Length(ls));
  ls:=ls{[1..Length(ls)-1]};
  lt := List(lt, s -> s+k1-1);
  return NumericalSetBySmallElements(Concatenation(ls,lt));
end);

InstallMethod(BondedSum, [IsNumericalSet,IsNumericalSemigroup],
function(S,T)
  return BondedSum(S,AsNumericalSet(T));
end);

InstallMethod(BondedSum, [IsNumericalSemigroup,IsNumericalSet],
function(S,T)
  return BondedSum(AsNumericalSet(S),T);
end);

InstallMethod(BondedSum, [IsNumericalSemigroup,IsNumericalSemigroup],
function(S,T)
  return BondedSum(AsNumericalSet(S),AsNumericalSet(T));
end);