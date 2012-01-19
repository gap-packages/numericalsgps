#############################################################################
##
#W  operations.gi           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: operations.gi,v 0.971 $
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
InstallMethod(\/, "for a numerical semigroup and a positive integer", true,
        [IsNumericalSemigroup,
         IsPosInt and IsMultiplicativeElement], 999999990,
        function( S,p )
    return(QuotientOfNumericalSemigroup(S, p));
end);

