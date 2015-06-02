#############################################################################
##
#W  preliminaries.gi        Manuel Delgado <mdelgado@fc.up.pt>
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
#F  BezoutSequence(arg)
##
##  Computes a Bezout sequence with ends two given rational numbers which may
##  be given as a list of two rationals.
##  Warning: rational numbers are silently transformed into
##  irreducible fractions.
##
#############################################################################
InstallGlobalFunction(BezoutSequence, function(arg)
    local   r,  s,  a1,  b1,  a2,  b2,  bz,  AD,  BD,  i,  AC,  BC,  z,  A,  
            B;

    if IsList(arg[1]) then
        r := arg[1][1];
        s := arg[1][2];
    else
        r := arg[1];
        s := arg[2];
    fi;
    if not (IsRat(r) and IsRat(s) and r<s) then
        Error("The arguments r<s of BezoutSequence must be rationals");
    fi;
    a1 := NumeratorRat(r);
    b1 := DenominatorRat(r);
    a2 := NumeratorRat(s);
    b2 := DenominatorRat(s);

    if a1<b1 or a2<b2 then
        bz := BezoutSequence(r+1,s+1);
        return List(bz, x->x-1);
    fi;

    ################# The Decreasing sequence############
    AD := [a1];
    BD := [b1];
    i := 1;
    while BD[i] <> 1 do
        AD[i+1] := 1/BD[i] mod AD[i];
        BD[i+1] := -1/AD[i] mod BD[i];
        i:= i+1;
    od;
    ################# The increasing sequence############
    AC := [a2];
    BC := [b2];
    i := 1;
    while AC[i] <> 1 do
        if BC[i] <> 1 then
            AC[i+1] := -1/BC[i] mod AC[i];
            BC[i+1] := 1/AC[i] mod BC[i];
            i:= i+1;
        elif BC[i] = 1 then
            AC[i+1] := AC[i]-1;
            BC[i+1] := 1;
            i:= i+1;
        fi;
    od;

    ################# The construction...###################
    z := [];
    A := Concatenation(AD,Reversed(AC));
    B := Concatenation(BD,Reversed(BC));
    for i in [1..Length(AD)] do
        Add(z,A[i]/B[i]);
    od;
    for i in [Length(AD)+1..Length(A)] do
        if not A[i]/B[i] in z then
            Add(z,A[i]/B[i]);
        else
            z := z{[1..Position(z,A[i]/B[i])]};
        fi;

    od;

    return z;
end);



#############################################################################
##
#F  IsBezoutSequence(L)
##
##  Tests if a sequence is a Bezout sequence.
##
#############################################################################
InstallGlobalFunction(IsBezoutSequence, function(L)
    local i;

    if not (IsList(L) and ForAll(L, x -> IsRat(x))) then
        return false; # Error("The argument must be a list of rational numbers");
    fi;

	if L=[] or Minimum(L)<0 then
		return false;
	fi;
    for i in [1..Length(L)-1] do
        if NumeratorRat(L[i+1])*DenominatorRat(L[i])-
           NumeratorRat(L[i])*DenominatorRat(L[i+1]) <> 1 then
            Print("Take the ",i," and the ",i+1," elements of the sequence","\n");
            return false;
        fi;
    od;
    return true;
end);


#############################################################################
##
#F  RepresentsPeriodicSubAdditiveFunction(L)
##
##  Tests whether a list L of length m represents a subadditive function f
##  periodic of period m. To avoid defining f(0) (which we assume to be 0) we
##  define f(m)=0 and so the last element of the list must be 0.
##  This technical need is due to the fact that <position>
##  in a list must be positive (not a 0).
##
#############################################################################
InstallGlobalFunction(RepresentsPeriodicSubAdditiveFunction, function(L)
    local i, j, k, m;

    if not IsListOfIntegersNS(L) then
        return false; #Error("The argument must be a non empty list of integers");
    fi;

    m := Length(L);
    if not L[m] = 0 then
        return false;
    fi;

	if Minimum(L)<0 then
		return false;
	fi;

    for i in [1..m] do
        for j in [1..m] do
            k := (i+j) mod m;
            if k = 0 then
                k := m;
            fi;

            if L[k] > L[i] + L[j] then
                return false;
            fi;
        od;
    od;
    return true;
end);




#############################################################################
##
#F  RepresentsSmallElementsOfNumericalSemigroup(L)
##
##  Tests if a list (which has to be a set) may represent the "small"
##  elements of a numerical semigroup.
##
#############################################################################
InstallGlobalFunction(RepresentsSmallElementsOfNumericalSemigroup, function(L)
    local L0, n, m, sum ,p;

    if not IsListOfIntegersNS(L) then
        return false; #Error("The argument must be a nonempty list of integers");
    fi;

	if Minimum(L)<0 then
		return false;
	fi;

    if not Set(L) = L or not 0 in L then
        return false;
    fi;
    L0 := Difference(L,[0]);
    sum := [];
    for n in L0 do
        for m in L0 do
            AddSet(sum,m+n);
        od;
    od;
    return ForAll(sum, p-> (p in L) or (p > L0[Length(L0)]));
end);




#############################################################################
##
#F  CeilingOfRational(R)
##
##  Computes the smallest integer greater than a rational r/s.
##
#############################################################################
InstallGlobalFunction(CeilingOfRational, function(R)
    local r,s;
    if not IsRat(R) then
        Error("R must be  rational");
    fi;
    r := NumeratorRat(R);
    s := DenominatorRat(R);

    if IsInt(R) then
        return R;
    elif R < 0 then
        return Int(R);
    else
        return Int(R) + 1;
    fi;
end);

#############################################################################
##
#F  IsListOfIntegersNS(list)
##
##  Tests whether L is a nonemty list integers.
##
#############################################################################
InstallGlobalFunction(IsListOfIntegersNS, function(list)
	if not(IsList(list)) then 
		return false;
	fi;
	
	if list <> [] then
		if IsInt(list[1]) then
			if IsHomogeneousList(list) then
				return true;
			fi;
		fi;
	fi;
	return false;
end);

#############################################################################
##
#F  IsAperyListOfNumericalSemigroup(L)
##
##  Tests whether a list may represent the Apery set of a numerical semigroup.
##
#############################################################################
InstallGlobalFunction(IsAperyListOfNumericalSemigroup, function(L)
    local   m,  firstToLastPosition,  K,  i,  j,  k;

    #if not (IsList(L) and ForAll(L, i -> IsInt(i))) then
	if not(IsListOfIntegersNS(L)) then
        return false; #Error("The argument of IsAperyListOfNumericalSemigroup must be a nonempty list of integers\n");
    fi;

	if Minimum(L)<0 then
		return false;
	fi;

    m := Length(L);

    #####################################################################
    # Moves the first element of a list to the end of the list
    firstToLastPosition := function(L)
        local mL;
        if Length(L) = 1 or Length(L) = 0 then
            mL := L;
        else
            mL :=  L{[2..Length(L)]};
            Add(mL,L[1]);
        fi;
        return mL;
    end;
    ## end of local function ##
    K := List(L, i-> i mod m);
    if K <> [0.. m-1] then
        return false;
    fi;
    #	K := firstToLastPosition(Set(L));
    K := firstToLastPosition(L);
    for i in [1..m] do
        for j in [1..m] do
            k := (i+j) mod m;
            if k = 0 then
                k := m;
            fi;

            if K[k] > K[i] + K[j] then
                return false;
            fi;
        od;
    od;
    return true;
end);

