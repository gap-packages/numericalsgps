#############################################################################
##
#W  basics.gi               Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#Y  Copyright 2005 by Manuel Delgado,
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#A  MultiplicityOfNumericalSemigroup(S)
##
##  Returns the multiplicity of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a numerical semigroup",
        [IsNumericalSemigroup and HasGenerators],10,
        function(S)
    return Minimum(GeneratorsOfNumericalSemigroup(S));
end);

InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a numerical semigroup",
        [IsNumericalSemigroup and HasAperyList],1,
        function(S)
    return Length(S!.aperylist);
end);

InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a modular numerical semigroup",
        [IsNumericalSemigroup and HasModularConditionNS],
        function(S)
    local   a,  b;

    a := ModularConditionNS(S)[1];
    b := ModularConditionNS(S)[2];
    return First([1..b], i-> a*i mod b <= i);
end);

InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a proportionally modular numerical semigroup",
        [IsNumericalSemigroup and HasProportionallyModularConditionNS],
        function(S)
    local   a,  b,  c;

    a := ProportionallyModularConditionNS(S)[1];
    b := ProportionallyModularConditionNS(S)[2];
    c := ProportionallyModularConditionNS(S)[3];
    return First([1..b], i-> a*i mod b <= c*i);
end);

# Agorithm in RosalesVasco2008MIA
InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a numerical semigroup given by a closed interval",
        [IsNumericalSemigroup and HasClosedIntervalNS],
        function(S)
    local   r,  s,  ListReducedIntervalsNC,  P,  list,  j,  i,  n;

    r := ClosedIntervalNS(S)[1];
    s := ClosedIntervalNS(S)[2];
    #############
    ## local function
    ListReducedIntervalsNC := function(r,s)
        local  list,  x,  y,  b1,  a1,  b2,  a2;

        list := [[r,s]];
        x := r;
        y := s;
        while not x <= Int(y) do
            b1 := NumeratorRat(x);
            a1 := DenominatorRat(x);
            b2 := NumeratorRat(y);
            a2 := DenominatorRat(y);
            # note that neither (b2 mod a2) nor (b1 mod a1) is zero
            x := a2/(b2 mod a2);
            y := a1/(b1 mod a1);
            Append(list, [[x,y]]);
        od;
        return list;
    end; # of local function
    P := [];
    list := ListReducedIntervalsNC(r,s);
    j := Length(list);
    P[j] := CeilingOfRational(list[j][1]);
    if j >1 then
        for i in [2..j] do
            n := j-i+2;
            P[n-1] := 1/P[n] + Int(list[n-1][1]);
        od;
    fi;
    return NumeratorRat(P[1]);
end);

InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a numerical semigroup",
        [IsNumericalSemigroup],
        function(S)
    if Length(SmallElementsOfNumericalSemigroup(S)) > 1 then
        return SmallElementsOfNumericalSemigroup(S)[2];
    else
        return 1;
    fi;
end);

#############################################################################
##
#A  FrobeniusNumber(S)
#A  FrobeniusNumberOfNumericalSemigroup(S)
##
##  Returns the Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasGaps],100,
        function(S)
  if Gaps(S) = [] then
    return -1;
  fi;
  return(Maximum(Gaps(S)));
end);

InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasSmallElements],99,
        function(S)
    return(Maximum(SmallElements(S)) - 1);
end);
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasAperyList],50,
        function(S)
    return(Maximum(AperyList(S))-Length(AperyList(S)));
end);

##########
## the generic method
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup],
        function(S)
  local  set, len, min_mult_n3_in_n1n2, gens, n, C, gg, c, n1, n2, n3, c1, 
         c2, c3, delta, d, gn, og, newgens, ap;

  if not (HasMinimalGenerators(S) or HasGenerators(S)) then
    return(Maximum(SmallElementsOfNumericalSemigroup(S))-1);
  fi;
    ## Local Functions
    ##############################################################
    ## Rosales&Vasco
    min_mult_n3_in_n1n2 := function(n1,n2,n3)
        local   u,  a,  b,  c, ns;

        if n1=n3 then
            return 1;
        fi;
        u := n2^-1 mod n1; #requires gcd(n1,n2)=1
        a := (u*n2*n3) mod (n1*n2);
        b := n1*n2;
        c := n3;
        ns := NumericalSemigroupByInterval(b/a,b/(a-c));
        return MultiplicityOfNumericalSemigroup(ns);
    end;

    ##############################################################
    gens := MinimalGeneratingSystemOfNumericalSemigroup(S);
    n := Length(gens);
    C := IteratorOfCombinations(gens,n-1);
    gg := fail;
    
    for c in C do
      if Gcd(c)<>1 then
        gg := c;
      break;
      fi;
    od;    
    
    ## for the case of three coprime generators we use an algorithms due to Rosales & Vasco
    if gg = fail then ## Rosales&Vasco
        if n = 3 then
            n1 := gens[1];
            n2 := gens[2];
            n3 := gens[3];
            c1 := min_mult_n3_in_n1n2(n2,n3,n1);
            c2 := min_mult_n3_in_n1n2(n1,n3,n2);
            c3 := min_mult_n3_in_n1n2(n1,n2,n3);

            delta := RootInt((c1*n1+c2*n2+c3*n3)^2-4*(c1*n1*c2*n2+c1*n1*c3*n3+c2*n2*c3*n3-n1*n2*n3));

            return ((c1-2)*n1+(c2-2)*n2+(c3-2)*n3+delta)/2;
        else
            ap := AperyList(S);
            return Maximum(ap)-Length(ap);;

        fi;
    fi;
    ## next we make use of Johnson's reduction
    d := Gcd(gg);
    gn := Difference(gens,gg);
    og := List(gg, i -> i/d);
    newgens := MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroup(Union(og,gn)));
    if  Length(newgens) = 2 then # Sylvester
        return d*(newgens[1]*newgens[2]-newgens[1]-newgens[2]) + (d-1)*gn[1];
    elif Length(newgens) = 1 then
        return -d + (d-1)*gn[1];
    else
        return d*FrobeniusNumberOfNumericalSemigroup(NumericalSemigroup(newgens)) + (d-1)*gn[1];
    fi;

end);

#The algorithm used here was obtained by Rosales&Vasco
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and IsModularNumericalSemigroup],1,
        function(S)
    local   a,  b,  r,  s,  ns,  m;

    a := ModularConditionNS(S)[1];
    b := ModularConditionNS(S)[2];
    if (a=1) or (b=1) then
        return -1;
    fi;
    r := (2*b^2+1)/(2*a*b);
    s := (2*b^2-1)/(2*b*(a-1));
    ns := NumericalSemigroupByInterval(r,s);
    m := MultiplicityOfNumericalSemigroup(ns);
    return b - m;
end);


#The algorithm used here was obtained by Delgado&Rosales
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasProportionallyModularConditionNS],1,
        function(S)
    local   a,  b,  c,  j;

    a := ProportionallyModularConditionNS(S)[1];
    b := ProportionallyModularConditionNS(S)[2];
    c := ProportionallyModularConditionNS(S)[3];

    if a <= c then
        return -1;
    fi;
    a := a mod b;
    if a = 0 then
        return -1;
    fi;
    if a > (b+c)/2 then
        a := b+c-a;
    fi;
    j := CeilingOfRational(a-a/c-a/b+(2*a)/(c*b));

    while ((j*b) mod a + Int((j*b)/a)*c <= (c-1)*b+a-c) do
        j := j+1;
    od;
    return b-Int((j*b)/a) -1;
end);


#############################################################################
##
#F  ConductorOfNumericalSemigroup(S)
##
##  Returns the conductor of the numerical semigroup S.
##
#############################################################################
InstallMethod(Conductor,
        "Returns the conductor of a numerical semigroup",
        [IsNumericalSemigroup],
        function( sgp )
    return FrobeniusNumber(sgp)+1;
end);

#############################################################################
##
#A  TypeOfNumericalSemigroup(S)
##
##  Returns the type of the numerical semigroup S.
##
#############################################################################
InstallMethod( TypeOfNumericalSemigroup,
        "Returns the type of a numerical sgp",
        [IsNumericalSemigroup],
        function( sgp )
    return Length(PseudoFrobeniusOfNumericalSemigroup(sgp));
end);

InstallMethod(Type,
        "Returns the type of a numerical sgp",
        [IsNumericalSemigroup],
        function( sgp )
    return TypeOfNumericalSemigroup(sgp);
end);

#############################################################################
##
#A  Generators(S)
#A  GeneratorsOfNumericalSemigroup(S)
##
##  Returns a set of generators of the numerical
##  semigroup S. If a minimal generating system has already been computed, this
##  is the set returned.
##
#############################################################################
InstallMethod( GeneratorsOfNumericalSemigroup,
        "Returns generators of a numerical sgp",
        [IsNumericalSemigroup],
        function(S)
    if HasMinimalGenerators(S) then
        return(MinimalGenerators(S));
    elif HasGenerators(S) then
        return(Generators(S));
    fi;
    return(MinimalGeneratingSystemOfNumericalSemigroup(S));
end);

#############################################################################
##
#A  MinimalGeneratingSystem(S)
#A  MinimalGeneratingSystemOfNumericalSemigroup(S)
##
##  Returns the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod( MinimalGeneratingSystemOfNumericalSemigroup,
        "method for a numerical semigroup",
        true,
        [IsNumericalSemigroup],0,
        function(S)
  local  sumNS, Elm, c, m, max, T, mingen, generators, aux, i, g, gen, ss;

  #####################################################
  # Let A and B be *sets* (not just lists) of positive elements of the numerical semigroup S
  # returns the elements of A+B not greater than max

  sumNS := function(A,B,max)
    local   R, a,  b;

    R := [];
    for a in A do
      for b in B do
        if a+b > max then
          break;
        else
          AddSet(R,a+b);
        fi;
      od;
    od;
    return R;
  end;
  ##

  if HasMinimalGenerators(S) then
    return MinimalGenerators(S);
  fi;
  ##
  if HasGeneratorsOfNumericalSemigroup(S) then
    generators := Generators(S);
    m := Minimum(generators); # the multiplicity
    if m = 1 then #the semigroup is the whole N
      SetMinimalGenerators(S, [1]);
      return MinimalGenerators(S);
    elif m = 2 then
      SetMinimalGenerators(S, [2,First(generators, g -> g mod 2 = 1)]);
      return MinimalGenerators(S);
    fi;
    # A naive reduction that takes into account that the minimal generators are incongruent modulo the multiplicity. This reduction is slow and proves only to be useful for small multiplicities.
    if m < LogInt(Length(generators),2)^4 then
      aux := [m];
      for i in [1..m-1] do
        g := First(generators, g -> g mod m = i);
        if g <> fail then
          Append(aux,[g]);
        fi;
      od;
      gen := Set(aux);
    else
      gen := ShallowCopy(generators);
    fi;
    # now remove the generators that are not irreducible (i.e. may be written as the sum of others)
    ss := sumNS(gen,gen,Maximum(gen)); # non irreducible elements that are the sum of two generators
    while ss <> [] do
      gen :=  Difference(gen,ss);
      ss := sumNS(ss,gen,Maximum(gen));# non irreducible elements that are the sum of three, four, etc generators
    od;
    mingen := gen; # now gen is the set of irreducible elements
    SetMinimalGenerators(S,mingen);
    return mingen;
  fi;
  ## When nor a set of generators nor the small elements are known, the small elements are computed and, in case S is not N, the function is called again (now that the system has enlarged its knowledge on S)
  Elm := SmallElementsOfNumericalSemigroup(S);
  if Elm = [0] then #if S = N, the single minimal generator is 1.
    return [1];
  fi;
  return MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroup(Union(Elm,[Elm[Length(Elm)]..Elm[Length(Elm)]+Elm[2]-1])));
end);

#############################################################################
##
#F  MinimalGeneratingSystem(S)
##  If S is a numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfNumericalSemigroup
## If S is an ideal of numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfIdealOfNumericalSemigroup
##
# InstallGlobalFunction(MinimalGeneratingSystem,
#         function(S)
#   if IsNumericalSemigroup(S) then
#     return MinimalGeneratingSystemOfNumericalSemigroup(S);
#   elif IsIdealOfNumericalSemigroup(S) then
#     return MinimalGeneratingSystemOfIdealOfNumericalSemigroup(S);
#   else
#     Error("The argument must be a numerical semigroup or an ideal of a numerical semigroup.");
#   fi;
# end);

#############################################################################
##
#A  EmbeddingDimensionOfNumericalSemigroup(S)
##
##  Returns the cardinality of the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod(EmbeddingDimensionOfNumericalSemigroup,
        "Returns the embedding dimension of a numerical semigroup",
        [IsNumericalSemigroup],10,
        function(sgp)
    return Length(MinimalGeneratingSystemOfNumericalSemigroup(sgp));
end);

#############################################################################
##
#A  FundamentalGapsOfNumericalSemigroup(S)
##
##  Returns the fundamental gaps of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod( FundamentalGapsOfNumericalSemigroup,
        "returns the list of fundamental gaps",
        true,
        [IsNumericalSemigroup],
        function(S)
  local  g, h, fh;

  h := ShallowCopy(GapsOfNumericalSemigroup(S));
  if HasFundamentalGaps(S) then
    return FundamentalGaps(S);
  fi;
  h := GapsOfNumericalSemigroup(S);
  fh := [];
  while h <> [] do
    g := h[Length(h)];
    Add(fh,g);
    h := Difference(h,DivisorsInt(g));
  od;
  fh := Set(fh);
  return fh;
#  SetFundamentalGaps(S, fh);
#  return FundamentalGaps(S);
end);
#############################################################################
##
#A  PseudoFrobeniusOfNumericalSemigroup(S)
##
##  Returns the pseudo Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod( PseudoFrobeniusOfNumericalSemigroup,
        "returns a list of the pseudo Frobenius numbers of a  numerical semigroup",
        true,
        [IsNumericalSemigroup],
        function(S)
  local  gaps, f, lefts;
  gaps := GapsOfNumericalSemigroup(S);
  if gaps = [] then                 # S is N
    return [-1];
  fi;
  f := Maximum(gaps);
  lefts := Difference([1..f],gaps);
  return Filtered(gaps, g-> ForAll(lefts, n -> (n+g in lefts) or (n+g > f)));
end);

#############################################################################
##
#A  SpecialGapsOfNumericalSemigroup(S)
##
##  Returns the special gaps of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod( SpecialGapsOfNumericalSemigroup,
        "returns the list of special gaps",
        true,
        [IsNumericalSemigroup],
        function(S)
  local  gaps, PF, f, non_specials, y, specials;
  gaps := GapsOfNumericalSemigroup(S);
  PF := PseudoFrobeniusOfNumericalSemigroup(S);
  f := Maximum(PF);
  non_specials := [];
  for y in Intersection(PF,[1..Int(f/2)+1]) do
    if (2*y in gaps) then
      Add(non_specials,y);
    fi; 
  od;
  specials := Difference(PF,non_specials);
  return specials;
end);


#############################################################################
##
#O  BelongsToNumericalSemigroup(n,S)
##
##  Tests if the integer n belongs to the numerical
##  semigroup S.
##
#############################################################################
InstallMethod( \in,
        "for numerical semigroups",
        [ IsInt, IsNumericalSemigroup ],
        function( x, s )
    return BelongsToNumericalSemigroup(x,s);
end);


InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasSmallElements],20,
        function(n,S)
    local s;
    if n=0 then
        return true;
    fi;
    s := SmallElements(S);
    return (n in s) or (n >= Maximum(s));
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasAperyList],10,
        function(n,S)
    local   ap,  m;

    if n=0 then
        return true;
    fi;
    ap := AperyList(S);
    m := Length(ap);
    if First([1..m], i-> (n mod m = i-1) and n >= ap[i]) <> fail then
        return true;
    else
        return false;
    fi;
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasFundamentalGaps],10,
        function(n,S)
    local   f;

    if n=0 then
        return true;
    fi;
    f := FundamentalGaps(S);
    return First(f, i -> i mod n =0) = fail;
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasModularConditionNS],15,
        function(n,S)
    local a,b;
    if n=0 then
        return true;
    fi;
    a := ModularConditionNS(S)[1];
    b := ModularConditionNS(S)[2];
    return a*n mod b <= n;
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasProportionallyModularConditionNS],20,
        function(n,S)
    local a,b,c;
    if n=0 then
        return true;
    fi;
    a := ProportionallyModularConditionNS(S)[1];
    b := ProportionallyModularConditionNS(S)[2];
    c := ProportionallyModularConditionNS(S)[3];
    return a*n mod b <= c*n;
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasGenerators],
        function(n,S)
    local gen, ss, sumNS, ed, belongs, maxgen, mingen;
    #####################################################
    # Computes the sum of subsets of numerical semigroups
    sumNS := function(S,T)
        local mm, s, t, R;
        R := [S[1]+T[1]];
        mm := Minimum(Maximum(S),Maximum(T));
        for s in S do
            for t in T do
                if s+t > mm then
                    break;
                else
                    AddSet(R,s+t);
                fi;

            od;
        od;
        return R;
    end;
    if n=0 then
        return true;
    fi;
    if n in Generators(S) then
        return true;
    fi;
    if HasMinimalGenerators(S) then
        gen := MinimalGenerators(S);
        ed:=Length(gen);
        if ed=1 then 
            return n>=0;
        fi;
        # some konwn bounds for Frobenius number can be used
        # Selmer's, Erdos-Graham, Schur
        if n>Minimum([2*gen[ed]*Int(gen[1]/ed)-gen[1], 2*gen[ed-1]*Int(gen[ed]/ed)-gen[ed], (gen[1]-1)*(gen[ed]-1)-1] ) then
          return true;
        fi;
    else
        gen := Generators(S);
        maxgen:=Maximum(gen);
        mingen:=Minimum(gen);
        # Schur's bound
        if n> (mingen-1)*(maxgen-1)  then
          return true;
        fi;
    fi;
    ss := sumNS(gen,gen);
	if n in ss then
		return true;
	fi;


  if n < Minimum(ss) then
      return false;
  fi;

  ##########################
  # the desperate method, inspired in the code of NrRestrictedPartitions
  # the old version caused some recursion depth problems
  belongs:=function(n,set)
    local p,m,l;
    p := [];
    for m  in [1..n+1]  do
	if (m-1) mod set[1] = 0  then
	    p[m] := 1;
	else
	    p[m] := 0;
	fi;
    od;
    for l  in set{ [2..Length(set)] }  do
	for m  in [l+1..n+1]  do
	    p[m] := p[m] + p[m-l];
	od;
	if p[n+1]>0 then
	    return true;
	fi;
    od;
    return p[n+1]<>0;
  end;

  return belongs(n,gen);

  #TryNextMethod();

end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup],
        function(n,S)
    local   m,  ap;

    if n=0 then
        return true;
    fi;
    m := MultiplicityOfNumericalSemigroup(S);
    ap := AperyListOfNumericalSemigroupWRTElement(S,m);
    if ForAny([1..m], i-> (n mod m = i-1) and n >= ap[i]) then
        return true;
    else
        return false;
    fi;
end);



#############################################################################
##
#O  AperyListOfNumericalSemigroupWRTElement(s,m)
##
##  Returns the Apery list of the numerical
##  semigroup s with respect to m.
##  This version is by Chris O'Neill 
#############################################################################
InstallMethod( AperyListOfNumericalSemigroupWRTElement,
        "returns the Apery list of a  numerical semigroup with respect to a nonzero element of the semigroup",
        true,
        [IsNumericalSemigroup,IsInt],
        function(s,m)
	local  msg, nonmults, ret, g, curround, nextround, a;

    if not BelongsToNumericalSemigroup(m,s) then
        Error("The second argument  must be an element of the first argument");
    else
        msg := Generators(s);
        nonmults := Difference(msg,[m]);
        ret := ListWithIdenticalEntries(m,infinity);

        for g in Reversed(nonmults) do
            ret[(g mod m)+1] := g;
        od;
        ret[1] := 0;

        curround := List(nonmults);

        while curround <> []  do
            nextround := [];

            for a in curround do
                if ret[(a mod m)+1] <> a then
                continue;
                fi;

                for g in nonmults do
                if g + a < ret[((g + a) mod m)+1] then
                    ret[((g + a) mod m)+1] := g + a;
                    Append(nextround, [g + a]);
                fi;
                od;
            od;

            curround := nextround;
        od;
        
        if m =MultiplicityOfNumericalSemigroup(s) then
            SetAperyList(s, ret); #
        fi;
    fi;
    return ShallowCopy(ret);
end);

#############################################################################
##
#A  AperyList(S)
#A  AperyListOfNumericalSemigroup(S)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to the multiplicity.
##
#############################################################################
InstallMethod( AperyList,
        "returns the Apery list of a  numerical semigroup with respect to the multiplicity",
        true,
        [IsNumericalSemigroup],
        function(S)
  return(AperyListOfNumericalSemigroupWRTElement(S,
                MultiplicityOfNumericalSemigroup(S)));
end);

# Now another method for the case an integer is specified

InstallOtherMethod( AperyList,
        "returns the Apery list of a  numerical semigroup with respect to the multiplicity",
        true,
        [IsNumericalSemigroup, IsInt],
        function(S,n)
    if n in S then
        return(AperyListOfNumericalSemigroupWRTElement(S,n));
    fi;
    return(AperyListOfNumericalSemigroupWRTInteger(S,n));
end);


#############################################################################
##
#F  AperyListOfNumericalSemigroupWRTInteger(S,n)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to the positive integer n.
##
#############################################################################
InstallGlobalFunction( AperyListOfNumericalSemigroupWRTInteger,
        function(S,n)
    local   Ap,  f,  max,  i;

    if not(IsInt(n)) then
        Error("The second argument must be a positive integer");
	fi;
	#if n<=0 then
  #      Error("The second argument must be a positive integer");
	#fi;

	if not(IsNumericalSemigroup(S)) then
		Error("The first argument must be a numerical semigroup");
	fi;
    f := FrobeniusNumberOfNumericalSemigroup(S);
    max := f + n+1; #from this point on x-n is in S
    Ap:=Filtered(Difference([0..max],GapsOfNumericalSemigroup(S)), x -> not((x-n) in S));
	return Ap;
end);


#############################################################################
##
#F  AperyListOfNumericalSemigroupAsGraph(ap)
##
##  <ap> is the Apery set of a numerical semigroup.
##  This function returns the adjacency list of the graph
##  whose vertices are
##  the elements of <ap> and the arrow u -> v exists
##  iff v - u is in <ap>.
##  The 0 is ignored.
##
#############################################################################
InstallGlobalFunction(AperyListOfNumericalSemigroupAsGraph, function(ap)
    local   ap2,  E,  i,  j,  G,  e;

    if not IsAperyListOfNumericalSemigroup(ap) then
        Error("The argument must be the Apery set of a numerical semigroup");
    fi;

    # Compute the set of edges of the digraph
    ap2 := Set(ap);
    E := [];
    for i in ap do
        for j in ap do
            if j-i in ap2 then
                Add(E, [i,j]);
            fi;
        od;
    od;

    # Build the adjacency list
    G := [];
    for e in E do
        if not (e[1] = 0 or e[2] = 0) then
            if not IsBound(G[e[1]]) then
                G[e[1]] :=[];
            fi;
            AddSet(G[e[1]], e[2]);
        fi;
    od;

    return G;
end);


#############################################################################
##
#F  FirstElementsOfNumericalSemigroup(n,s)
##
##  Prints the list of the first <n> elements of <s>.
##
#############################################################################
InstallGlobalFunction(FirstElementsOfNumericalSemigroup, function(n,s)
    local   se,  l,  max;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.");
    fi;

    if n=0 then
        return [];
    fi;


    if not IsPosInt(n) then
        Error("The first argument must be a nonnegative integer.");
    fi;

    se:=SmallElementsOfNumericalSemigroup(s);
    l:=Length(se);

    if l>=n then
        return se{[1..n]};
    fi;

    max:=se[l];

    return Concatenation( se, [max+1..(max+n-l)]);
end);

#############################################################################
##
#F KunzCoordinatesOfNumericalSemigroup(arg)
##
## If two argumets are given, the first is a semigroup s and the second an
## element m in s. If one argument is given, then it is the semigroup, and
## m is set to the multiplicity.
## Then the Apéry set of m in s has the form [0,k_1m+1,...,k_{m-1}m+m-1], and
## the output is the (m-1)-uple [k_1,k_2,...,k_{m-1}]
#############################################################################
InstallGlobalFunction(KunzCoordinatesOfNumericalSemigroup,
        function(arg)
    local narg,s,m,ap;

    narg:=Length(arg);
    if narg>2 then
        Error("The number of arguments is at most two");
    fi;
    s:=arg[1];
    if not(IsNumericalSemigroup(s)) then
        Error("The first argument must be a numerical semigroup");
    fi;
    if narg=2 then
        m:=arg[2];
        if not(m in s) then
            Error("The second argument must be an element of the first");
        fi;
        if m=0 then
            Error("The second argument cannot be zero");
        fi;
    else
        m:=MultiplicityOfNumericalSemigroup(s);
    fi;

    ap:=AperyListOfNumericalSemigroupWRTElement(s,m);
    return List([2..m],i->(ap[i]-i+1)/m);
end);

InstallMethod(KunzCoordinates,
    "for a numerical semigroup",
    [IsNumericalSemigroup],
    KunzCoordinatesOfNumericalSemigroup);

InstallMethod(KunzCoordinates,
    "for a numerical semigroup and an integer",
    [IsNumericalSemigroup,IsInt],
    KunzCoordinatesOfNumericalSemigroup);

#############################################################################
##
#F KunzPolytope(m)
## For a fixed multiplicity, the Kunz coordinates of the semigroups
## with that multiplicity are solutions of a system of inequalities Ax\ge b
## (see [R-GS-GG-B]). The output is the matrix (A|-b)
##
#############################################################################
InstallGlobalFunction(KunzPolytope,
        function(m)
    local mat,c, eq, row, it,zero;

    if not(IsPosInt(m)) then
        Error("The argument must be a positive integer");
    fi;

    c:=Cartesian([1..m-1],[1..m-1]);
    c:=Filtered(c,p->p[1]<=p[2]);
    eq:=IdentityMat(m-1);
    eq:=TransposedMat(Concatenation(eq,[List([1..m-1],_->-1)]));
    zero:=List([1..m],_->0);

    for it in c do
        row:=ShallowCopy(zero);
        row[it[1]]:=row[it[1]]+1;
        row[it[2]]:=row[it[2]]+1;
        if (it[1]+it[2])<m then
            row[it[1]+it[2]]:=-1;
            eq:=Concatenation(eq,[row]);
        fi;
        if (it[1]+it[2])>m then
            row[it[1]+it[2]-m]:=-1;
            row[m]:=1;
            eq:=Concatenation(eq,[row]);
        fi;
    od;
    return eq;
end);

#############################################################################
##
#A HolesOfNumericalSemigroup(s)
## For a numerical semigroup, finds the set of gaps x such that F(S)-x is
## is also a gap
##
#############################################################################
InstallMethod(HolesOfNumericalSemigroup,
        "Returns the embedding dimension of a numerical semigroup",
        [IsNumericalSemigroup],10,
function(s)
  local gs, f;
  gs:=GapsOfNumericalSemigroup(s);
  f:=FrobeniusNumber(s);
  return Filtered(gs, x-> not((f-x) in s));
end);


#############################################################################
##
#F  CocycleOfNumericalSemigroupWRTElement(S,n)
##
##  Returns the cocycle of the numerical semigroup S with respect to
##  the positive integer n (an element in S)
##
#############################################################################
InstallGlobalFunction(CocycleOfNumericalSemigroupWRTElement,function(S,s)
  local i,j,b, ap;

  if not(IsNumericalSemigroup(S)) then
    Error("The first argument must be a numerical semigroup");
  fi;

  if not(s in S)then
    Error("The second argument must be in the first");
  fi;

  b:=IdentityMat(s);
  ap:=AperyListOfNumericalSemigroupWRTElement(S,s);
  for i in [0..s-1] do
    for j in [0..s-1] do
      b[i+1][j+1]:=(ap[i+1]+ap[j+1]-ap[(i+j) mod s + 1])/s;
    od;
  od;
  return b;
end);

#############################################################################
##
#O RthElementOfNumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer r, returns the r-th element of S
#############################################################################
InstallMethod(RthElementOfNumericalSemigroup,
        [IsNumericalSemigroup,IsPosInt],
        function(S,r)
  local   selts,  n;

  selts := SmallElementsOfNumericalSemigroup( S );
  n := Length(selts);
  if r <= Length(selts) then
    return selts[r];
  else
    return selts[n] + r - n;
  fi;
end);
#########
InstallMethod(RthElementOfNumericalSemigroup,
        [IsInt,IsNumericalSemigroup],
        function(r,S)
  return(RthElementOfNumericalSemigroup(S,r));
end);

#############################################################################
##
#O NextElementOfNumericalSemigroup(S,n)
## Given a numerical semigroup S and an integer r, returns the least integer
## greater than r belonging to S
#############################################################################
InstallMethod(NextElementOfNumericalSemigroup,
        [IsNumericalSemigroup,IsInt],
        function(S,r)
            return First([r+1..r+Multiplicity(S)+1], x->x in S);
end);
#########
InstallMethod(NextElementOfNumericalSemigroup,
        [IsInt,IsNumericalSemigroup],
        function(r,S)
  return(NextElementOfNumericalSemigroup(S,r));
end);
#############################################################################
##
#O DivisorsOfElementInNumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer n, returns a list L of integers such that
# x in L if and only if n - x belongs to S, that is, it returns S\cap(n-S)
# These elements are called divisors of n
##
#############################################################################
InstallMethod(DivisorsOfElementInNumericalSemigroup,
        [IsNumericalSemigroup,IsInt],
        function(S,n)
  local   elts;

  #the first n elements of S not greater than n
  elts := Intersection([0..n], S );
  if n=0 then 
    return [0];
  fi;
  return(Intersection(n - elts,elts));
end);
########
InstallMethod(DivisorsOfElementInNumericalSemigroup,
        [IsInt,IsNumericalSemigroup],
        function(n,S)
  return(DivisorsOfElementInNumericalSemigroup(S,n));
end);


#########################################################################
#F NumericalSemigroupByNuSequence(NuSeq)
## Given a nu-sequence, compute the semigroup asociated to it.
## Based on the code by Jorge Angulo, inspired in 
## - Bras-Amorós, Maria Numerical semigroups and codes. 
## Algebraic geometry modeling in information theory, 167–218, 
## Ser. Coding Theory Cryptol., 8, World Sci. Publ., Hackensack, NJ, 2013
#########################################################################
InstallGlobalFunction(NumericalSemigroupByNuSequence,
function(NuSeq)
    local isNu, i, l, S, g, c, k, G, DTilde, cand, ncand, NuSequence;
    if not(IsListOfIntegersNS(NuSeq)) then
        Error("The argument must be a list of integers.");
    fi;
    #Some checks on sequence. This are only necesary Conditions for a Nu sequence
    isNu:=true;
    l:=Size(NuSeq);
    if l>0 then
        if not(NuSeq[1]=1) then isNu:=false; fi;
        if l>1 then
            if not(NuSeq[2]=2) then isNu:=false; fi;
        else #If nu sequece is [1], then the semigroup is N
            return NumericalSemigroup(1);
        fi;
        for i in [1..l] do
            if not(NuSeq[i]<=i) then isNu:=false; fi;
        od;
    fi;
    if not isNu then
        Error("The argument is not a nu-sequence.");
    fi;
    #We compute numerical semigroup from NuSeq.
    S:=[];
    #We first need to compute k, greatest i such that nu_i=nu_{i+1}
    k:=1;
    for i in [1..(l-1)] do
        if NuSeq[i]=NuSeq[i+1] then
        k:=i;
        fi;
    od;
    #From k on, every entry should increas by one
    if First([k+1..(l-1)],i->not(NuSeq[i]+1=NuSeq[i+1]))<>fail then
        Error("The argument is not a nu-sequence.");
    fi;
    #With k, we can calculate both Genus and Conductor
    g:=k+1-NuSeq[k];
    c:=(k+g+1)/2;

    #We keep track of gaps.
    #Note that 1 is gap, since the trivial semigroup was already considered.
    G:=[1,c-1];

    #This auxiliar function computes the number of gaps, l, i<l<c-1 such that:
    #c-1+i-l is also a Gap
    DTilde:=function(i,c,G)
        local l, D;
        D:=0;
        for l in [(i+1)..(c-2)] do
        if l in G then
            if (c+i-l-1) in G then D:=D+1; fi;
        fi;
        od;
        return D;
    end;

    for i in Reversed([2..(c-1)]) do
        if NuSeq[c+i-g]=(c+i-2*g+DTilde(i,c,G)) then
        Add(S,i,1);
        else
        Add(G,i);
        fi;
    od;
    #Now, we determine small elements of the semigroup.
    Add(S,0,1); # O is always n the semigroup.
    Add(S,c); # Conductor is always n the semigroup.

    NuSequence:=S->List([1..2*Conductor(S)-Genus(S)],i->Length(DivisorsOfElementInNumericalSemigroup(S[i],S)));
    cand:= NumericalSemigroupBySmallElements(Set(S));
    ncand:=NuSequence(cand);
    if ncand<>NuSeq{[1..Length(ncand)]} then
        Error("The sequence determines a semigroup, but it is not a nu-sequence");
    fi;
    return cand;
end);

#########################################################################
#F NumericalSemigroupByTauSequence(TauSeq)
## Given a tau-sequence, compute the semigroup asociated to it.
## Based on the code by Jorge Angulo, inspired in 
## - Bras-Amorós, Maria Numerical semigroups and codes. 
## Algebraic geometry modeling in information theory, 167–218, 
## Ser. Coding Theory Cryptol., 8, World Sci. Publ., Hackensack, NJ, 2013
#########################################################################
InstallGlobalFunction(NumericalSemigroupByTauSequence,
function(TauSeq)
  local i, j, k, l, FoundNewMin, g, c, S, aux, min,small;
    if not(IsListOfIntegersNS(TauSeq)) then
        Error("The argument must be a list of integers.");
    fi;
  #The first step is to compute the minumun integer, k, such that for all i,
  #Tau_{k+2i}=Tau_{k+2i+1} and Tau_{k+2i+2}=Tau_{k+2i+1}+1.
  l:=Length(TauSeq);
  k:=l-1;
  for j in Reversed([1..l]) do
    FoundNewMin:=true;
    for i in [0..Int((l-j)/2-2)] do

      FoundNewMin:=(TauSeq[j+2*i]=TauSeq[j+2*i+1]) and
        (TauSeq[j+2*i+1]+1=TauSeq[j+2*i+2]) ;

      if not FoundNewMin then break; fi; #Break out of the inner loop
    od;
    if FoundNewMin then k:=j; fi;
  od;
  #With this, we have the conductor and genus
  c:=k-TauSeq[k];
  g:=k-2*TauSeq[k]-1;

  #Now, we compute the semigrup.
  #We need initialize it to a list of lenght l
  S:=[];
  for i in [1..(c-g)] do Add(S,0); od; #The first values don't matter now, but will matter later
  for i in [(c-g)..l] do Add(S,i+g); od; #The values after the conductor are tivial

  for i in Reversed([2..c-g]) do
    #Follow the procedure, as outlined in the proof by Maria Brass
    aux:=Positions(TauSeq,i-1);
    min:=1;
    for j in [1..Length(aux)] do
        if S[aux[min]]>(S[aux[j]]) then
          min:=j;
        fi;
    od;
    S[i]:=S[aux[min]]/2;
  od;
  small:=S{[1..(c-g+1)]};
  if not(RepresentsSmallElementsOfNumericalSemigroup(small)) then
    Error("The argument is not the tau-sequence of a numerical semigroup.");
  fi;
  return NumericalSemigroupBySmallElementsNC(small);
end);

#############################################################################
##
#F ElementNumber_NumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer n, returns the nth element of S
#############################################################################
InstallGlobalFunction(ElementNumber_NumericalSemigroup, 
    function(s,n)
        return RthElementOfNumericalSemigroup(s,n);
    end
);


#############################################################################
##
#F NumberElement_NumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer n, returns the position of 
# n in S
#############################################################################
InstallGlobalFunction(NumberElement_NumericalSemigroup,
    function(s,n)
        local f, nse;
        if not(n in s) then 
            return(fail);
        fi;
        f:=FrobeniusNumber(s);
        if n<f then 
            return Position(SmallElements(s),n);
        fi;
        nse:=Length(SmallElements(s));
        return nse+n-f-1;
    end
);


##################################################################################
##
#O Iterator(s)
## Iterator for numerical semigroups
##################################################################################
InstallMethod(Iterator, "Iterator for numerical semigroups", [IsNumericalSemigroup], 
    function(sem)
    local iter;

    iter:=IteratorByFunctions(rec( 
        pos := -1,  
        s := sem,
        IsDoneIterator := ReturnFalse, 
        NextIterator := function(iter) local n; n:=First([iter!.pos+1..iter!.pos+Multiplicity(iter!.s)+1], x->x in iter!.s); iter!.pos:=n; return n; end, 
        ShallowCopy := iter -> rec( s := iter!.s,  pos := iter!.pos )
        ));
    return iter;
    end
);

##################################################################################
##
#O S[n]
## The nth element of S
##################################################################################

InstallOtherMethod(\[\], [IsNumericalSemigroup,IsInt],
    function(s,n)
        return ElementNumber_NumericalSemigroup(s,n);
    end
);

##################################################################################
##
#O S{ls}
## [S[n] :  n in ls]
##################################################################################


InstallOtherMethod(\{\}, [IsNumericalSemigroup,IsList],
    function(s,l)
        return List(l,n->s[n]);
    end
);
