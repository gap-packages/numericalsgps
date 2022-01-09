#############################################################################
##
#W  operations.gi           Manuel Delgado <mdelgado@fc.up.pt>
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
#F  QuotientOfNumericalSemigroup(S,p)
##
##  Computes S/p, where S is a numerical semigroup
##  and p a positive integer.
##
#############################################################################
InstallGlobalFunction(QuotientOfNumericalSemigroup, function(S,p)
    local   f,  T,  s,  R,  g,  l;

    if not (IsNumericalSemigroup(S) and IsPosInt(p)) then
        Error("QuotientOfNumericalSemigroup takes a numerical semigroup and a positive integer as arguments.");
    fi;

    f := FrobeniusNumberOfNumericalSemigroup(S);
    T := [0];
    if not p in GapsOfNumericalSemigroup(S) then 
        return NumericalSemigroup(1);
    else
        for s in [1..f+1] do
            if BelongsToNumericalSemigroup(p * s,S) then
                Add(T,s);
            fi;
        od;

        R := Difference([1..T[Length(T)]],T);
        g := R[Length(R)];
        l := Intersection([0..g+1],Union(T, [g+1]));
        return(NumericalSemigroupByGaps(Difference([0..l[Length(l)]], l)));
    fi;
end);


#############################################################################
##
#M  S / p
##
##  A short for QuotientOfNumericalSemigroup(S, p)
##
#############################################################################
InstallOtherMethod(\/, "for a numerical semigroup and a positive integer", true,
        [IsNumericalSemigroup,
         IsPosInt and IsMultiplicativeElement], 999999990,
        function( S,p )
    return(QuotientOfNumericalSemigroup(S, p));
end);


############################################################
##
#F MultipleOfNumericalSemigroup(s,a,b)
## s is a numerical semigroup; a and b are positive integers
## Computes a*s \cup [b,\infty)
##
############################################################
InstallGlobalFunction(MultipleOfNumericalSemigroup,function(s,a,b)
    local l, c;
    
    if not(IsNumericalSemigroup(s)) then
        Error("The first argument must be a numerical semigroup");
    fi;
    if not(IsPosInt(a)) then
        Error("The second argument must be a positive ingteger");
    fi;
    if not(IsPosInt(b)) then
        Error("The third argument must be a positive integer");
    fi;
    
    c:=ConductorOfNumericalSemigroup(s);
    if b<a*c then
        Info(InfoNumSgps,1,"The third argument is smaller than the second times the conductor of the first");
    fi;
    
    l:=[0..Int(b/a)];
    l:=Union(a*Intersection(s,l), [b]);
    return NumericalSemigroupBySmallElementsNC(l);
end);

#################################################################
##
#F InductiveNumericalSemigroup(a,b)
## a and b are lists of positive integers with b[i+1]\ge a[i]b[i]
## Computes inductively the semigroup
## S_0=N
## S_i=a_iS_{i-1}\cup \{a_ib_i,a_ib_i+1,->\}
## and outputs S_n, with n the length of a and b
##
##################################################################
InstallGlobalFunction(InductiveNumericalSemigroup,function(a,b)
	local stmp, n, i;

	stmp:=NumericalSemigroup(1);
	if not(IsListOfIntegersNS(a)) or not(IsListOfIntegersNS(b)) then
		Error("The arguments must be lists of positive integers");
	fi;

	if not(ForAll(Union(a,b), x-> x>0)) then 
		Error("The arguments must be lists of positive integers");
	fi;

	n:=Length(a);

	if not(Length(b)=n) then
		Error("Both arguments must have the same length");
	fi;

	if not(ForAll([1..n-1], i->b[i+1]>= a[i]*b[i])) then
		Error("The condition b[i+1]>= a[i]*b[i] does not hold");
	fi;
	for i in [1..n] do
		stmp:=MultipleOfNumericalSemigroup(stmp, a[i],a[i]*b[i]);
	od;	

	return stmp;
	
end);    

#############################################################################
##
#F  DilatationOfNumericalSemigroup(S,a)
##
##  Computes {0}\cup{a+s |s in S\{0}}; a must be in M-2M, 
##  with M the maximal ideal of S 
##
#############################################################################
InstallGlobalFunction( DilatationOfNumericalSemigroup,
function(S,a)
    local M;
    if not(IsNumericalSemigroup(S)) then
        Error("The first argument must be a numerical semigroup");
    fi;
    M:=MaximalIdeal(S);
    if not(a in M-2*M) then
        Error("The second argument must be an integer in M-2M, with M the maximal ideal of the first argument");
    fi;
    return NumericalSemigroupBySmallElements(Concatenation([0], a+Difference(SmallElements(S),[0])));
end);
