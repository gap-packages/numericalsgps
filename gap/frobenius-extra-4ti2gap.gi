#############################################################################
##
#W  frobenius-extra-4ti2gap.gi        Ignacio Ojeda <mdelgado@fc.up.pt>
#W                                    Carlos Jesús Moreno Ávila <camorenoa@alumnos.unex.es>
#W                                    Manuel Delgado <mdelgado@fc.up.pt>
#W                                    Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Universidad de Extremadura and Universidad de Granada, Spain
#############################################################################

#############################################################################
##
#F  FrobeniusNumber(s)
##
##  Computes the Frobenius Number of the numerical semigroup <s>.
##
##  The definition of Frobenius Number can be found in
##  the book
##   - Rosales, J. C.; García-Sánchez, P. A. Numerical semigroups.
##     Developments in Mathematics, 20. Springer, New York, 2009.
##  The main algorithm used appears in
##   - Roune, B.H. The slice algorithm for irreducible decomposition of
##     monomial ideals.J. Symbolic Comput. 44 (2009), no. 4, 358–381.
##
##   REQUERIMENTS: 4ti2Interface
#############################################################################
InstallMethod(FrobeniusNumber,
    "method using 4ti2 for the calculaction of the Frobenius number",
    [IsNumericalSemigroup],60,
    function( S )
    local v, n, M, msm, MonomialIdeal, MaximalStandardMonomials,  BelongsToMonomialIdeal, MinimalGeneratingSystemOfMonomialIdeal, QuotientOfMonomialIdealByMonomial;

    Info(InfoNumSgps,2,"Using 4ti2gap for the calculation of the Frobenius number");
    MonomialIdeal := function(v)
        local n, I, M, E, upperbound, C, m, L, ap, i;
        n := Length(v);
        I := GroebnerBasis4ti2( [v], [v] );
        I := I{[1 .. Length(I)]}{[2 .. n]};
        M := [];
        for i in I do
    	     Add(M,List(i,x->Maximum(x,0)));
        od;
        return M;
    end;

    MinimalGeneratingSystemOfMonomialIdeal := function(M)
        return Filtered(M, m->not(BelongsToMonomialIdeal(Difference(M,[m]),m)));
    end;

    BelongsToMonomialIdeal := function(M,m)
        return ForAny(M, x->ForAll(m-x, y -> y>=0));
    end;

    QuotientOfMonomialIdealByMonomial := function(M,m)
        local n,quotient,f,g;
        n := Length(m);
        quotient := [];
        for f in M do
            g := List(f-m, x->Maximum(x,0));
            if IsZero(g) then
                return [g];
            fi;
            Add(quotient,g);
        od;
        return quotient;
    end;

    MaximalStandardMonomials := function(M,S,p,i,msm)
        local n, C, q, M1, S1,tr;

        tr:=function(v)
          return List(v-1, x->Maximum(x,0));
        end;
        n := Length(M[1]);
        M := MinimalGeneratingSystemOfMonomialIdeal(M);
        if ForAll(Sum(M), x->x=1) then
            Add(msm,p);
            return msm;
        fi;
        q:=First(M{[i..Length(M)]}, qq->not(IsZero(tr(qq)) or BelongsToMonomialIdeal(S,tr(qq))));
        if q<>fail then
          q:=tr(q);
          M1 := QuotientOfMonomialIdealByMonomial(M,q);
          S1 := QuotientOfMonomialIdealByMonomial(S,q);
          msm := MaximalStandardMonomials(M1,MinimalGeneratingSystemOfMonomialIdeal(S1),p+q,1,msm);
          Add(S,q);
          i:=i+1;
          msm := MaximalStandardMonomials(M,MinimalGeneratingSystemOfMonomialIdeal(S),p,i,msm);
          return msm;
        fi;
        return msm;
    end;

    v := MinimalGeneratingSystemOfNumericalSemigroup(S);
    n := Length(v);
    M := MonomialIdeal(v);
    msm := MaximalStandardMonomials(M,[],Zero([1 .. n-1]),1,[]);
    return Maximum(msm*v{[2 .. Length(v)]})-v[1]; #return maximum S-degree - v_1
end);

#############################################################################
##
#F  AperyList(s)
##
##  Computes the Apery set of the numerical semigroup <s> with respect to
##  the multiplicit of <s>
##
##  The definition of Apery set can be found in
##  the book
##   - Rosales, J. C.; García-Sánchez, P. A. Numerical semigroups.
##     Developments in Mathematics, 20. Springer, New York, 2009.
##  The main algorithm used appears in
##   - Roune, B.H. The slice algorithm for irreducible decomposition of
##     monomial ideals.J. Symbolic Comput. 44 (2009), no. 4, 358–381.
##
##   REQUERIMENTS: 4ti2Interface
#############################################################################

InstallMethod(AperyList,
    "method using 4ti2 for the calculaction of the Apery set",
    [IsNumericalSemigroup],60,
    function( S )
    local v, n, M, msm, c, L, MonomialIdeal, MaximalStandardMonomials, BelongsToMonomialIdeal, MinimalGeneratingSystemOfMonomialIdeal, QuotientOfMonomialIdealByMonomial, LatticePointsInBoxGivenByDiagonal;

    Info(InfoNumSgps,2,"Using 4ti2gap for the calculation of the Apery set");

    MonomialIdeal := function(v)
        local n, I, M, E, upperbound, C, m, L, ap, i;
        n := Length(v);
        I := GroebnerBasis4ti2([v], [v] );
        I := I{[1 .. Length(I)]}{[2 .. n]};
        M := [];
        for i in I do
    	     Add(M,List(i,x->Maximum(x,0)));
        od;
        return M;
    end;

    MinimalGeneratingSystemOfMonomialIdeal := function(M)
        return Filtered(M, m->not(BelongsToMonomialIdeal(Difference(M,[m]),m)));
    end;

    BelongsToMonomialIdeal := function(M,m)
        return ForAny(M, x->ForAll(m-x, y -> y>=0));
    end;

    QuotientOfMonomialIdealByMonomial := function(M,m)
        local n,quotient,f,g;
        n := Length(m);
        quotient := [];
        for f in M do
            g := List(f-m, x->Maximum(x,0));
            if IsZero(g) then
                return [g];
            fi;
            Add(quotient,g);
        od;
        return quotient;
    end;

    MaximalStandardMonomials := function(M,S,p,i,msm)
        local n, C, q, M1, S1,tr;

        tr:=function(v)
          return List(v-1, x->Maximum(x,0));
        end;
        n := Length(M[1]);
        M := MinimalGeneratingSystemOfMonomialIdeal(M);
        if ForAll(Sum(M), x->x=1) then
            Add(msm,p);
            return msm;
        fi;
        q:=First(M{[i..Length(M)]}, qq->not(IsZero(tr(qq)) or BelongsToMonomialIdeal(S,tr(qq))));
        if q<>fail then
          q:=tr(q);
          M1 := QuotientOfMonomialIdealByMonomial(M,q);
          S1 := QuotientOfMonomialIdealByMonomial(S,q);
          msm := MaximalStandardMonomials(M1,MinimalGeneratingSystemOfMonomialIdeal(S1),p+q,1,msm);
          Add(S,q);
          i:=i+1;
          msm := MaximalStandardMonomials(M,MinimalGeneratingSystemOfMonomialIdeal(S),p,i,msm);
          return msm;
        fi;
        return msm;
    end;

    LatticePointsInBoxGivenByDiagonal := function( lowerconer, uppercorner )
        local V,i;
        V := [];
        for i in [1 .. Length(lowerconer)] do
            Add(V,[lowerconer[i] .. uppercorner[i]]);
        od;
        return Cartesian(V);
    end;

    v := MinimalGeneratingSystemOfNumericalSemigroup(S);
    n := Length(v);
    M := MonomialIdeal(v);
    msm := MaximalStandardMonomials(M,[],Zero([1 .. n-1]),1,[]);
    L := [];
    for c in msm do
        L := Union(L,LatticePointsInBoxGivenByDiagonal(Zero([1 .. n-1]),c));
    od;
    L:= L*v{[2 .. Length(v)]};
    return List([0..v[1]-1], i->First(L, y->(y-i) mod v[1]=0));
end);
