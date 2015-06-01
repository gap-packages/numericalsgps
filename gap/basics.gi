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
        [IsNumericalSemigroup and HasGeneratorsNS],10,
        function(S)
    return GeneratorsOfNumericalSemigroup(S)[1];
end);

InstallMethod(MultiplicityOfNumericalSemigroup,
        "Returns the multiplicity of a numerical semigroup",
        [IsNumericalSemigroup and HasAperyListNS],1,
        function(S)
    return Minimum(Difference(S!.aperylist,[0]),Length(S!.aperylist));
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
#A  FrobeniusNumberOfNumericalSemigroup(S)
##
##  Returns the Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasGapsNS],100,
        function(S)
  if GapsNS(S) = [] then
    return -1;
  fi;
  return(GapsNS(S)[Length(GapsNS(S))]);
end);

InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasSmallElementsNS],99,
        function(S)
    return(SmallElementsNS(S)[Length(SmallElementsNS(S))] - 1);
end);
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup and HasAperyListNS],50,
        function(S)
    return(Maximum(AperyListNS(S))-Length(AperyListNS(S)));
end);

##########
## the generic method
InstallMethod(FrobeniusNumberOfNumericalSemigroup,
        "Returns the Frobenius Number of the numerical sgp",
        [IsNumericalSemigroup],
        function(S)
    local   set,  len,  min_mult_n3_in_n1n2,  gens,  n,  C,  gg,  n1,  n2,  
            n3,  c1,  c2,  c3,  delta,  d,  gn,  og,  newgens;

    if not (HasMinimalGeneratorsNS(S) or HasGeneratorsNS(S)) then
        set := SmallElementsOfNumericalSemigroup(S);
        len := Length(set);
        return(set[len] - 1);
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
    C := Combinations(gens,n-1);
    gg := First(C,c -> Gcd(c)<>1);
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
            set := SmallElementsOfNumericalSemigroup(S);
            len := Length(set);
            return(set[len] - 1);

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
        [IsNumericalSemigroup and IsModularNumericalSemigroup],
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
        [IsNumericalSemigroup and IsProportionallyModularNumericalSemigroup],
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
InstallGlobalFunction(ConductorOfNumericalSemigroup,
        function( sgp )
    if not IsNumericalSemigroup(sgp) then
        Error("The argument must be a numerical semigroup");
    fi;
    return FrobeniusNumber(sgp)+1;
end);

#############################################################################
##
#F  TypeOfNumericalSemigroup(S)
##
##  Returns the type of the numerical semigroup S. 
##
#############################################################################
InstallGlobalFunction(TypeOfNumericalSemigroup,
        function( sgp )
    if not IsNumericalSemigroup(sgp) then
        Error("The argument must be a numerical semigroup");
    fi;
    return Length(PseudoFrobeniusOfNumericalSemigroup(sgp));
end);

#############################################################################
##
#F  GeneratorsOfNumericalSemigroup(S)
##
##  Returns a set of generators of the numerical
##  semigroup S. If a minimal generating system has already been computed, this
##  is the set returned.
##
#############################################################################
InstallGlobalFunction( GeneratorsOfNumericalSemigroup, function(S)
    if not IsNumericalSemigroup(S) then
        Error("The argument must be a numerical semigroup");
    fi;

    if HasMinimalGeneratorsNS(S) then
        return(MinimalGeneratorsNS(S));
    elif HasGeneratorsNS(S) then
        return(GeneratorsNS(S));
    fi;
    return(MinimalGeneratingSystemOfNumericalSemigroup(S));
end);



#############################################################################
##
#F  GeneratorsOfNumericalSemigroupNC(S)
##
##  Returns a set of generators of the numerical
##  semigroup S.
##
#############################################################################
# InstallGlobalFunction( GeneratorsOfNumericalSemigroupNC, function(S)
#     if not IsNumericalSemigroup(S) then
#         Error("The argument must be a numerical semigroup");
#     fi;
#     if HasGeneratorsNS(S) then
#         return(GeneratorsNS(S));
#     fi;
#     return(MinimalGeneratingSystemOfNumericalSemigroup(S));
# end);



#############################################################################
##
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
    local   sumNS,  Elm,  g,  T,  generators,  m,  aux,  i,  gen,  ss;
    
    #####################################################
    # Computes the sum of subsets of numerical semigroups
    # WARNING: the arguments have to be non empty sets, not just lists
    sumNS := function(S,T)
        local   R,  mm,  s,  t;

        R := [];
        mm := Maximum(Maximum(S),Maximum(T));
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
    ##

    if HasMinimalGeneratorsNS(S) then
      return MinimalGeneratorsNS(S);
    elif HasFrobeniusNumberOfNumericalSemigroup(S) then
        Elm := SmallElementsOfNumericalSemigroup(S);
        if Elm = [0] then
            SetMinimalGeneratorsNS(S, [1]);
            return MinimalGeneratorsNS(S);
        fi;

        g := FrobeniusNumberOfNumericalSemigroup(S);
        T:=Union(Elm{[2..Length(Elm)]},[Elm[Length(Elm)]..Elm[Length(Elm)]+Elm[2]]);
        SetMinimalGeneratorsNS(S, Difference(T,sumNS(T,T)));
        return MinimalGeneratorsNS(S);

    elif HasGeneratorsNS(S) then
        # Note that the minimal generators are precisely those generators that are irreducible. 
        # The fact that minimal generators are incongruent modulo the multiplicity is used (for small multiplicities, since for big ones the reduction is slow and may not bring any advantages)

        generators := GeneratorsNS(S);
        m := Minimum(generators); # the multiplicity

        if m = 1 then
            SetMinimalGeneratorsNS(S, [1]);
            return MinimalGeneratorsNS(S);
        elif m = 2 then
            SetMinimalGeneratorsNS(S, [2,First(generators, g -> g mod 2 = 1)]);
            return MinimalGeneratorsNS(S); 
        elif m < LogInt(Length(generators),2)^4 then
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

        ss := sumNS(gen,gen);
        while ss <> [] do
            gen :=  Difference(gen,ss);
            ss := sumNS(ss,gen);
        od;
        SetMinimalGeneratorsNS(S, gen);
        return MinimalGeneratorsNS(S);

    else
        Elm := SmallElementsOfNumericalSemigroup(S);
        if Elm = [0] then
            SetMinimalGeneratorsNS(S, [1]);
            return MinimalGeneratorsNS(S);
        fi;
        g := FrobeniusNumberOfNumericalSemigroup(S);
        T:=Union(Elm{[2..Length(Elm)]},[Elm[Length(Elm)]..Elm[Length(Elm)]+Elm[2]]);
        return MinimalGeneratingSystemOfNumericalSemigroup(NumericalSemigroup(T));
    fi;
  end);
  
#############################################################################
##
#F  MinimalGeneratingSystem(S)
##  If S is a numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfNumericalSemigroup
## If S is an ideal of numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfIdealOfNumericalSemigroup
##
InstallGlobalFunction(MinimalGeneratingSystem,
        function(S)
  if IsNumericalSemigroup(S) then
    return MinimalGeneratingSystemOfNumericalSemigroup(S);
  elif IsIdealOfNumericalSemigroup(S) then
    return MinimalGeneratingSystemOfIdealOfNumericalSemigroup(S);
  else
    Error("The argument must be a numerical semigroup or an ideal of a numerical semigroup.");
  fi;
end);
          
#############################################################################
##
#F  ReducedSetOfGeneratorsOfNumericalSemigroup(arg)
##
##  Returns a set with possibly fewer generators than those recorded in <C>S!.generators</C>. It changes <C>S!.generators</C> to the set returned.
##The function has 1 to 3 arguments. One of them a numerical semigroup. Then an argument is a boolean (<E>true</E> means that all the elements not belonging to the Apery set with respect to the multiplicity are removed; the default is "false") and another argument is a positive integer <M>n</M> (meaning that generators that can be written as the sum of <n> or less generators are removed; the default is "2"). The boolean or the integer may not be present. If a minimal generating set for <M>S</M> is known or no generating set is known, then the minimal generating system is returned.
##  
# InstallGlobalFunction( ReducedSetOfGeneratorsOfNumericalSemigroup, function(arg)
#     local   sumNS,  S,  apery,  n,  generators,  m,  aux,  i,  g,  gen,  ss;

#     #####################################################
#     # Computes the sum of subsets of numerical semigroups
#     # WARNING: the arguments have to be non empty sets, not just lists
#     sumNS := function(S,T)
#         local mm, s, t, R;
#         R := [];
#         mm := Maximum(Maximum(S),Maximum(T));
#         for s in S do
#             for t in T do
#                 if s+t > mm then
#                     break;
#                 else
#                     AddSet(R,s+t);
#                 fi;
#             od;
#         od;
#         return R;
#     end;
#     ##
#     S := First(arg, s -> IsNumericalSemigroup(s));
#     if S = fail then
#         Error("Please check the arguments of ReducedSetOfGeneratorsOfNumericalSemigroup");
#     fi;
#     apery := First(arg, s -> IsBool(s));
#     if apery = fail then
#         apery := false;
#     fi;       
#     n := First(arg, s -> IsInt(s));
#     if n = fail then
#         n := 2;
#     fi;

#     if not IsBound(S!.generators) then
#         S!.generators := MinimalGeneratingSystemOfNumericalSemigroup(S);
#         return S!.generators;
#     else
#         if IsBound(S!.minimalgenerators) then
#             #S!.generators := MinimalGeneratingSystemOfNumericalSemigroup(S);
#             return S!.minimalgenerators;
#         fi;
#         generators := S!.generators;
#         m := Minimum(generators); # the multiplicity
#         if m = 1 then
#             S!.generators := [1];
#             return S!.generators;
#         elif m = 2 then
#             S!.generators := [2,First(generators, g -> g mod 2 = 1)];
#             return S!.generators;
#         fi;
#         if apery then
#             aux := [m];
#             for i in [1..m-1] do
#                 g := First(generators, g -> g mod m = i);
#                 if g <> fail then
#                     Append(aux,[g]);
#                 fi;
#             od;
#             gen := Set(aux);
#         else
#             gen := ShallowCopy(generators);
#         fi;
#         ss := sumNS(gen,gen);
#         i := 1;
#         while i < n and ss <> [] do
#             gen :=  Difference(gen,ss);
#             ss := sumNS(gen,ss);
#             i := i+1;
#         od;
#     fi;

#     S!.generators := gen;
#     return S!.generators;
# end);

##
#############################################################################


#############################################################################
##
#F  EmbeddingDimensionOfNumericalSemigroup(S)
##
##  Returns the cardinality of the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
InstallGlobalFunction(EmbeddingDimensionOfNumericalSemigroup,
        function( sgp )
    if not IsNumericalSemigroup(sgp) then
        Error("The argument must be a numerical semigroup.");
    fi;

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
  if HasFundamentalGapsNS(S) then
    return FundamentalGapsNS(S);
  fi;
  h := GapsOfNumericalSemigroup(S);
  fh := [];
  while h <> [] do
    g := h[Length(h)];
    Add(fh,g);
    h := Difference(h,DivisorsInt(g));
  od;
  fh := Set(fh);
  SetFundamentalGapsNS(S, fh);
  return FundamentalGapsNS(S);
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
    local   hs,  ehs,  SE,  S0,  x;

    hs := GapsOfNumericalSemigroup(S);
    if hs = [] then                 # S is N
        return [-1];
    fi;
    ehs := [];
    SE := SmallElementsOfNumericalSemigroup(S);
    S0 := SE{[2..Length(SE)]};
    for x in hs do
        if ForAll(S0, s-> BelongsToNumericalSemigroup(x+s,S)) then
            Add(ehs, x);
        fi;
    od;
    return ehs;
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
    local  PF, Y, y;

    PF := PseudoFrobeniusOfNumericalSemigroup(S);
    Y := [];
    for y in PF do
      if BelongsToNumericalSemigroup(2*y,S) then
        Add(Y,y);
      fi;
    od;
    return Y;
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
        [IsInt,IsNumericalSemigroup and HasSmallElementsNS],100,
        function(n,S)
    local s;
    if n=0 then
        return true;
    fi;
    s := SmallElementsNS(S);
    return (n in s) or (n >= Maximum(s));
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasAperyListNS],
        function(n,S)
    local   ap,  m;

    if n=0 then
        return true;
    fi;
    ap := AperyListNS(S);
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
        [IsInt,IsNumericalSemigroup and HasFundamentalGapsNS],
        function(n,S)
    local   f;

    if n=0 then
        return true;
    fi;
    f := FundamentalGapsNS(S);
    return First(f, i -> i mod n =0) = fail;
end);

InstallMethod( BelongsToNumericalSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [IsInt,IsNumericalSemigroup and HasModularConditionNS],
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
        [IsInt,IsNumericalSemigroup and HasProportionallyModularConditionNS],
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
        [IsInt,IsNumericalSemigroup and HasGeneratorsNS],
        function(n,S)
    local gen, ss, sumNS;
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
    if n in GeneratorsNS(S) then
        return true;
    fi;
    if HasMinimalGeneratorsNS(S) then
        gen := MinimalGeneratorsNS(S);
    else
        gen := GeneratorsNS(S);
    fi;
    ss := sumNS(gen,gen);
	if n in ss then
		return true;
	fi;
    if n < Minimum(ss) then
        return false;
    else
        TryNextMethod();
    fi;
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
    if First([1..m], i-> (n mod m = i-1) and n >= ap[i]) <> fail then
        return true;
    else
        return false;
    fi;
end);



#############################################################################
##
#O  AperyListOfNumericalSemigroupWRTElement(S,n)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to n.
##
#############################################################################
InstallMethod( AperyListOfNumericalSemigroupWRTElement,
        "returns the Apery list of a  numerical semigroup with respect to a nonzero element of the semigroup",
        true,
        [IsNumericalSemigroup,IsInt],
        function(S,n)
    local   Ap,  f,  max,  i;

    #    if IsBound(S!.aperylist) and Length(S!.aperylist) = n then
    #        return S!.aperylist;
    #    elif not BelongsToNumericalSemigroup(n,S) then
    if not BelongsToNumericalSemigroup(n,S) then
        Error("The second argument  must be an element of the first argument in AperyListOfNumericalSemigroupWRTElement");
    else
        Ap := [0];
        f := FrobeniusNumberOfNumericalSemigroup(S);
        max := f + n; #see proposition 10.4 (book)
        for i in [1..n-1] do
            Add(Ap, First(Difference([1..max],GapsOfNumericalSemigroup(S)), j -> j mod n = i));
        od;
        if n =MultiplicityOfNumericalSemigroup(S) then
            SetAperyListNS(S, Ap); #
        fi;
    fi;
    return ShallowCopy(Ap);
end);

#############################################################################
##
#F  AperyListOfNumericalSemigroup(S)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to the multiplicity.
##
#############################################################################
InstallGlobalFunction( AperyListOfNumericalSemigroup,
        function(S)
  return(AperyListOfNumericalSemigroupWRTElement(S,
                MultiplicityOfNumericalSemigroup(S)));
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
	if n<=0 then
        Error("The second argument must be a positive integer");
	fi;

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

    max:=Maximum(se);

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
