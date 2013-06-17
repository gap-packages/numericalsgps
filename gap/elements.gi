#############################################################################
##
#W  elements.gi             Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: elements.gi,v 0.98 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
##
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Returns the list of elements in the numerical semigroup S,
##  not greater than the Frobenius number + 1.
##
#############################################################################
InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupBySmallElements ],
        function( sgp )
    return sgp!.elements;
end);

InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupByGaps ],
        function( sgp )
    return sgp!.elements;
end);


InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupByFundamentalGaps ],
        function( sgp )
    return sgp!.elements;
end);



InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupByAperyList ],
        function( sgp )
    local ap, m, x;
    ap := sgp!.aperylist;
    m := Length(ap);
    return Filtered([0..FrobeniusNumber(sgp)+1], x -> x mod m = 0 or
                   ap[x mod m + 1] <= x);
end);



InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupByGenerators],
         function( sgp )
     local g, S, n, bool, gen, R, sumNS, ss;
     
    #####################################################
    # Computes the sum of subsets of numerical semigroups
    sumNS := function(S,T)
        local mm, s, t, R;
        R := [];
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
    
    if IsBound(sgp!.minimalgenerators) then
        gen := sgp!.minimalgenerators;
    else
        gen := sgp!.generators;
        # a naive reduction of the number of generators
        ss := sumNS(gen,gen);
        gen := Difference(gen,ss);
        if ss <> [] then
            gen := Difference(gen,sumNS(ss,gen));
        fi;
    fi;
    
    S := [0];
    n := 1;
    bool := true;
    while bool do
        for g in gen do
            if n -g in S then
                AddSet(S,n);
                break;
            fi;
        od;
        if not IsSubset(S,[S[Length(S)]-gen[1]+1..S[Length(S)]]) then
            n:=n+1;
        else
            bool := false;
        fi;
    od;
    if Length(S) > 1 then
        SetGapsOfNumericalSemigroup(sgp,AsList(Difference([1..S[Length(S)]],S)));
    fi;
    R := GapsOfNumericalSemigroup(sgp);
    if R = [] then
        g := -1;
    else
        g := R[Length(R)];
    fi;
    SetFrobeniusNumberOfNumericalSemigroup(sgp,g);
    
    sgp!.elements := Intersection([0..g+1],Union(S, [g+1]));
    return Intersection([0..g+1],Union(S, [g+1]));;
end);



#############################################################################
##
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Returns the list of the elements of S(a,b) 
##  in [0..b].
##
#############################################################################
InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsModularNumericalSemigroup],
         function( sgp )
    local a, b, g, R, S, x;
    a := sgp!.modularcondition[1];
    b := sgp!.modularcondition[2];
    S := [0];
    for x in [1..b] do
        if a*x <= x or RemInt(a*x,b) <= x then
            Add(S,x);
        fi;
    od;
    if Length(S) > 1 then
        SetGapsOfNumericalSemigroup(sgp,AsList(Difference([1..S[Length(S)]],S)));
    fi;
    R := GapsOfNumericalSemigroup(sgp);
    if R = [] then
        g := -1;
    else
        g := R[Length(R)];
    fi;
    SetFrobeniusNumberOfNumericalSemigroup(sgp,g);
    
    sgp!.elements := Intersection([0..g+1],Union(S, [g+1]));
    return Intersection([0..g+1],Union(S, [g+1]));;
end);



#############################################################################
##
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Returns the list of the elements of S(a,b,c) 
##  in [0..b]. 
##
#############################################################################
InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsProportionallyModularNumericalSemigroup],
         function( sgp )
   local a, b, c, g, R, S, x;
    a := sgp!.proportionallymodularcondition[1];
    b := sgp!.proportionallymodularcondition[2];
    c := sgp!.proportionallymodularcondition[3];
    S := [0];
    for x in [1..b] do
        if a*x <= c*x or RemInt(a*x,b) <= c*x then
            Add(S,x);
        fi;
    od;

    if Length(S) > 1 then
        SetGapsOfNumericalSemigroup(sgp,AsList(Difference([1..S[Length(S)]],S)));
    fi;
    R := GapsOfNumericalSemigroup(sgp);
    if R = [] then
        g := -1;
    else
        g := R[Length(R)];
    fi;
    SetFrobeniusNumberOfNumericalSemigroup(sgp,g);
    
    sgp!.elements := Intersection([0..g+1],Union(S, [g+1]));
    return Intersection([0..g+1],Union(S, [g+1]));;
end);


#############################################################################
##
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Computes the numerical semigroup consiting of the non negative integers 
##  of the submonoid of RR generated by the interval ]r,s[ where r and s
##  are rational numbers. 
##
#############################################################################
InstallMethod(SmallElementsOfNumericalSemigroup,
    "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupByOpenInterval],
         function( sgp )
    local   r,  s,  k,  max,  NS,  i,  R,  g;
    
    r := sgp!.openinterval[1];
    s := sgp!.openinterval[2];

    k := 1;
    while k*s <= (k+1)*r do
        k := k+1;
    od;
    max := Int(k * s);
    NS := [0];
    for i in [1..k] do
        NS := Union(NS, Filtered([1..max+1], j -> (i*r < j) and (j < i *s)));
    od;
    
    if Length(NS) > 1 then
        SetGapsOfNumericalSemigroup(sgp,AsList(Difference([1..NS[Length(NS)]],NS)));
    fi;
    R := GapsOfNumericalSemigroup(sgp);
    if R = [] then
        g := -1;
    else
        g := R[Length(R)];
    fi;
    SetFrobeniusNumberOfNumericalSemigroup(sgp,g);
    
    sgp!.elements := Intersection([0..g+1],Union(NS, [g+1]));
    return Intersection([0..g+1],Union(NS, [g+1]));
end);
    


#############################################################################
##
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Given a subadditive function which is periodic with period Length(L)
##  produces the corresponding numerical semigroup
##
##  The periodic subadditive function is given through a list and
##  the last element of the list must be 0. 
##
#############################################################################
InstallMethod(SmallElementsOfNumericalSemigroup,
        "Returns the list of elements in the numerical semigroup not greater that the Frobenius number + 1",
        [IsNumericalSemigroup and IsNumericalSemigroupBySubAdditiveFunction],
         function( sgp )
    local   L,  m,  F,  S,  x,  fx,  R,  g;
    
    L := sgp!.subadditive;
    
    m := Length(L);
    F := Maximum(L) + m +1;
    S := [0];
    for x in [1..F] do
        if x mod m <> 0 then
            fx := L[x mod m];
        else
            fx := 0;
        fi;
        if fx <= x then
            Add(S,x);
        fi;
    od;

    if Length(S) > 1 then
        SetGapsOfNumericalSemigroup(sgp,AsList(Difference([1..S[Length(S)]],S)));
    fi;
    R := GapsOfNumericalSemigroup(sgp);
    if R = [] then
        g := -1;
    else
        g := R[Length(R)];
    fi;
    SetFrobeniusNumberOfNumericalSemigroup(sgp,g);
    
    sgp!.elements := Intersection([0..g+1],Union(S, [g+1]));
    return Intersection([0..g+1],Union(S, [g+1]));;
end);




#############################################################################
##
#A  GapsOfNumericalSemigroup(S)
##
##  Returns the list of the gaps of the numerical semigroup S.
##
#############################################################################
InstallMethod(GapsOfNumericalSemigroup,
        "Returns the list of the gaps of a numerical semigroup",
        [IsNumericalSemigroup],
        function( sgp )
    local S;
    S := SmallElementsOfNumericalSemigroup(sgp);
    return Difference([1..S[Length(S)]],S);
end);


#############################################################################
##
#F  GenusOfNumericalSemigroup(S)
##
##  Returns the number of gaps of the numerical semigroup S.
##
#############################################################################
InstallGlobalFunction(GenusOfNumericalSemigroup,
        function( sgp )
	    if not IsNumericalSemigroup(sgp) then
	        Error("The argument must be a numerical semigroup");
	    fi;
	return Length(GapsOfNumericalSemigroup(sgp));
end);
