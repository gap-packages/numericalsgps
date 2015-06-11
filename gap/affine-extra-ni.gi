#############################################################################
##
#W  affine-extra-ni.gi
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################
#if not TestPackageAvailability("NormalizInterface") = fail then
#    LoadPackage("NormalizInterface");
#fi;


##########################################################################
# Computes the Hilbert basis of the system A X=0 mod md, where the rows
# of A are the elements of ls.
# md can be empty of have some modulus, if the length of md is smaller than
# the lengths of the elements of ls, then the rest of equations are considered
# to be homogeneous linear Diophantine equations
# REQUERIMENTS: NormalizInterface
##########################################################################
InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousEquations,
        "Computes the Hilbert basis of a system of linear Diophantine equations, some of them can be in congruences",[IsMatrix,IsHomogeneousList],5,
        function(ls,md)
    local matcong, cone, ncong, ncoord, nequ, matfree;

    Info(InfoNumSgps,2,"Using normaliz to find the Hilbert basis.");

    if not(IsHomogeneousList(ls)) or not(IsHomogeneousList(md)) then
        Error("The arguments must be homogeneous lists.");
    fi;

    if not(ForAll(ls,IsListOfIntegersNS)) then
        Error("The first argument must be a list of lists of integers.");
    fi;

    ncong:=Length(md);

    if ncong>0 and not(IsListOfIntegersNS(md)) then
        Error("The second argument must be a lists of integers.");
    fi;

    if not(ForAll(md,x->x>0)) then
        Error("The second argument must be a list of positive integers");
    fi;

    nequ:=Length(ls);
    ncoord:=Length(ls[1]);
    matcong:=[];
    matfree:=[];

    if ncoord=0 then
        return [];
    fi;

    if ncong>0 and not(IsListOfIntegersNS(md)) then
        Error("The second argument must be either an empty list or a list of integers");
    fi;

    if ncong>nequ then
        Error("More mudulus than equations");
    fi;

    if nequ>ncong and ncong>0 then
        matcong:=ls{[1..ncong]};
        matcong:=TransposedMat(
                         Concatenation(TransposedMat(matcong),[md]));
        matfree:=ls{[ncong+1..nequ]};
        cone:=NmzCone(["congruences",matcong,"equations",matfree]);
    fi;

    if nequ=ncong then
        matcong:=TransposedMat(Concatenation(
                         TransposedMat(ls),[md]));
        cone:=NmzCone(["congruences",matcong]);
    fi;
    if ncong=0 then
        matfree:=ls;
        cone:=NmzCone(["equations",matfree]);
    fi;

    NmzCompute(cone,"DualMode");

    return NmzHilbertBasis(cone);
end);

##########################################################################
# Computes the Hilbert basis of the system ls*X>=0 over the nonnegative
# integers
# REQUERIMENTS: NormalizInterface
##########################################################################
InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousInequalities,
        "Computes the Hilbert basis of a system of inequalities",
        [IsMatrix],5,
        function(ls)
    local cone,  ncoord;

    Info(InfoNumSgps,2,"Using normaliz to find the Hilbert basis.");

    if not(IsHomogeneousList(ls)) then
        Error("The argument must be a homogeneous lists.");
    fi;

    if not(ForAll(ls,IsListOfIntegersNS)) then
        Error("The argument must be a list of lists of integers.");
    fi;

    if not(Length(Set(ls, Length))=1) then
        Error("The first argument must be a list of lists all with the same length.");
    fi;

    ncoord:=Length(ls[1]);

    if ncoord=0 then
        return [];
    fi;

    cone:=NmzCone(["inequalities",ls,"signs",[List([1..ncoord],_->1)]]);
    NmzCompute(cone,"DualMode");

    return NmzHilbertBasis(cone);
end);


########################################################################
# Computes the set of factorizations of v in terms of the elements of ls
# That is, a Hilbert basis for ls*X=v
# If ls contains vectors that generate a nonreduced monoid, then it
# deprecates the infinite part of the solutions, or in other words, it
# returns only the minimal solutions of the above system of equations
# REQUERIMENTS: NormalizInterface
########################################################################
InstallOtherMethod(FactorizationsVectorWRTList,
        "Computes the set of factorizations of the first argument in terms of the elements of the second",
        [IsHomogeneousList, IsMatrix],5,
        function(v,ls)
    local mat, cone, n, facs;
   	Info(InfoNumSgps,2,"Using NormalizInterface to compute minimal factorization.");

    n:=Length(ls);
    mat:=TransposedMat(Concatenation(ls,[-v]));

    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;

    if not(ForAll(ls,IsListOfIntegersNS)) then
        Error("The second argument must be a list of lists of integers.");
    fi;

    if not(IsRectangularTable(mat)) then
        Error("All lists must in the second argument have the same length as the first argument.");
    fi;

    cone:=NmzCone(["inhom_equations",mat]);
    NmzCompute(cone,"DualMode");
    facs:=List(NmzConeProperty(cone,"ModuleGenerators"), f->f{[1..n]});
    return facs;
end);

#####################################################################
# Computes the omega-primality of v in the affine semigroup a
# REQUERIMENTS: NormalizInterface
#####################################################################
InstallOtherMethod(OmegaPrimalityOfElementInAffineSemigroup,
        "Computes the omega-primality of v in the affine semigroup a",
        [IsHomogeneousList,IsAffineSemigroup],5,
        function(v,a)
    local mat, cone, n, hom, par, tot, le, ls;

    le:=function(a,b)  #ordinary partial order
    	return ForAll(b-a,x-> x>=0);
    end;

    if not(IsAffineSemigroup(a)) then
        Error("The second argument must be an affine semigroup");
    fi;

    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;

    if not(ForAll(v, x-> x>=0)) then
        Error("The first argument must be a list of on nonnegative integers.");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);
    n:=Length(ls);
    mat:=TransposedMat(Concatenation(ls,-ls,[-v]));

    if not(IsHomogeneousList(mat)) then
        Error("The first argument has not the dimension of the second.");
    fi;

    cone:=NmzCone(["inhom_equations",mat]);
    NmzCompute(cone,"DualMode");
    par:=Set(NmzModuleGenerators(cone), f->f{[1..n]});
    tot:=Filtered(par, f-> Filtered(par, g-> le(g,f))=[f]);
    Info(InfoNumSgps,2,"Minimals of v+ls =",tot);
    if tot=[] then
        return 0;
    fi;

    return Maximum(Set(tot, Sum));
end);
############################################
# Omega primality for full affine semigroups
###########################################
InstallMethod(OmegaPrimalityOfElementInAffineSemigroup,
        "Computes the omega-primality of v in the affine semigroup a",
        [IsHomogeneousList,IsAffineSemigroup and HasEquationsAS],3,
        function(v,a)

    local mat, cone, n, hom, par, tot, le, ls, one;

    le:=function(a,b)  #ordinary partial order
    	return ForAll(b-a,x-> x>=0);
    end;

    Info(InfoNumSgps,2,"Using that the semigroup is full.");

    ls:=GeneratorsAS(a);
    n:=Length(ls);
    one:=[List([1..n],_->1)];
    mat:=TransposedMat(Concatenation(ls,[-v]));
    cone:=NmzCone(["inhom_inequalities",mat,"signs",one]);
    NmzCompute(cone,"DualMode");
    par:=Set(NmzModuleGenerators(cone), f->f{[1..n]});

    Info(InfoNumSgps,2,"Minimals =",par);
    return Maximum(Set(par, Sum));
end);

######################################################################
# Computes the set of primitive elements of an affine semigroup, that
# is, the set of elements whose factorizations are involved in the
# minimal generators of the congruence associated to the monod
# (generators as a monoid; not to be confused with minimal presentations
# to this end, use BettiElementsOfAffineSemigroup)
#####################################################################
# An implementation of PrimitiveElementsOfAffineSemigroup using
# Normaliz
# REQUERIMENTS: NormalizInterface
#####################################################################
InstallOtherMethod(PrimitiveElementsOfAffineSemigroup,
        "Computes the primitive elements of an affine semigroup",
        [IsAffineSemigroup],5,
        function(a)
    local mat, n, cone, facs, ls;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);

    n:=Length(ls);
    mat:=TransposedMat(Concatenation(ls,-ls));
    cone:=NmzCone(["equations",mat]);
    NmzCompute(cone,"DualMode");
    facs:=Set(NmzHilbertBasis(cone), f->f{[1..n]});

    return Union(Set(facs, f->f*ls),ls);
end);

########
# Tame degree for full affine semigroups
########
InstallMethod(TameDegreeOfAffineSemigroup,
        "Computes the tame degree of the full affine semigroup a",
        [IsAffineSemigroup and HasEquationsAS],3,
        function(a)
    local ls, min, tame, gen,m, facts, t, minimalElementsPrincipalIdealOfAffineSemigroup;

    # uses the procedure of arXiv:1504.02998

    minimalElementsPrincipalIdealOfAffineSemigroup:=function(v,a)
        local mat, cone, n, hom, par, tot, le, ls, one;

        le:=function(a,b)  #ordinary partial order
            return ForAll(b-a,x-> x>=0);
        end;

        ls:=GeneratorsAS(a);
        n:=Length(ls);
        one:=[List([1..n],_->1)];
        mat:=TransposedMat(Concatenation(ls,[-v]));
        cone:=NmzCone(["inhom_inequalities",mat,"signs",one]);
        NmzCompute(cone,"DualMode");
        par:=Set(NmzModuleGenerators(cone), f->f{[1..n]});
        #tot:=Filtered(par, f-> Filtered(par, g-> le(g,f))=[f]);
        Info(InfoNumSgps,2,"Minimals Z(v+a)=",par);
        return List(par,x->x*ls);
    end;

    Info(InfoNumSgps,2,"Using NormalizInterface with full affine semigroup");

    ls:=GeneratorsOfAffineSemigroup(a);
    tame:=0;
    for gen in ls do
        min:=minimalElementsPrincipalIdealOfAffineSemigroup(gen,a);
        Info(InfoNumSgps,2,"Minimal elements of ",gen,"+a=",min);
        for m in min do
            facts:=FactorizationsVectorWRTList(m,ls);
            t:=TameDegreeOfSetOfFactorizations(facts);
            if t> tame then
                tame:=t;
                Info(InfoNumSgps,2,"Tame degree updated to ",tame);
            fi;
        od;
    od;
    return tame;

end);


#####################################################################
# Computes the tame degree of the affine semigroup a
# REQUERIMENTS: NormalizInterface
#####################################################################
#moved to affine.gi
# InstallGlobalFunction(TameDegreeOfAffineSemigroup,
#         function(a)
#     local prim, tams, p, max, ls;

#     if not(IsAffineSemigroup(a)) then
#         Error("The argument must be an affine semigroup");
#     fi;

#     ls:=GeneratorsOfAffineSemigroup(a);

#     Info(InfoNumSgps,2,"Computing primitive elements of ", ls);
#     prim:=PrimitiveElementsOfAffineSemigroup(a);
#     Info(InfoNumSgps,2,"Primitive elements of ", ls, ": ",prim);
#     max:=0;
#     for p in prim do
#         Info(InfoNumSgps,2,"Computing the tame degree of ",p);
#         tams:=TameDegreeOfSetOfFactorizations(
#                       FactorizationsVectorWRTList(p,ls));
#         Info(InfoNumSgps,2,"The tame degree of ",p, " is ",tams);
#         if tams>max then
#             max:=tams;
#         fi;
#     od;

#     return max;
# end);

#####################################################################
# Computes the elasticity of the affine semigroup a
# REQUERIMENTS: NormalizInterface
#####################################################################
# InstallGlobalFunction(ElasticityOfAffineSemigroup,
#         function(a)
#     local mat, n, cone, facs, ls;


#     if not(IsAffineSemigroup(a)) then
#         Error("The argument must be an affine semigroup");
#     fi;

#     ls:=GeneratorsOfAffineSemigroup(a);

#     n:=Length(ls);
#     mat:=TransposedMat(Concatenation(ls,-ls));
#     cone:=NmzCone(["equations",mat]);
#     NmzCompute(cone,"DualMode");
#     facs:=Set(NmzHilbertBasis(cone), f->[f{[1..n]},f{[n+1..2*n]}]);

#     return Maximum(Set(facs, y->Sum(y[1])/Sum(y[2])));
# end);
#Elasticity now done with circuits: much faster

#############################################################
#############################################################################################################################
###
#M IsFullAffineSemigroup
# Detects if the affine semigroup is full: the nonnegative
# of the the group spanned by it coincides with the semigroup
# itself; or in other words, if a,b\in S and a-b\in \mathbb N^n,
# then a-b\in S
################################################################
## moved to affine-def
# InstallGlobalFunction(IsFullAffineSemigroup,function(a)
#     local eq, h, gens;

#     if not(IsAffineSemigroup(a)) then
#         Error("The argument must be an affine semigroup.");
#     fi;

#     gens:=GeneratorsAS(a);
#     eq:=EquationsOfGroupGeneratedBy(gens);
#     h:=HilbertBasisOfSystemOfHomogeneousEquations(eq[1],eq[2]);
#     return ForAll(h, x->BelongsToAffineSemigroup(x,a));
# end);
##
# InstallMethod(IsFullAffineSemigroup,
#         "Tests if the affine semigroup S has the property of being full",
#         [IsAffineSemigroup],2,
#         function( S )
#   local  gens, eq, h;

#   gens := GeneratorsOfAffineSemigroup(S);
#   eq:=EquationsOfGroupGeneratedBy(gens);
#   h:=HilbertBasisOfSystemOfHomogeneousEquations(eq[1],eq[2]);
#   if ForAll(h, x->BelongsToAffineSemigroup(x,S)) then
#     SetEquationsAS(eq);
#     Setter(IsAffineSemigroupByEquations)(S,true);
#     Setter(IsFullAffineSemigroup)(S,true);
#     return true;
#   fi;
#   return false;
# end);
#############################################################################
##
#O  GeneratorsOfAffineSemigroup(S)
##
##  Computes a set of generators of the affine semigroup S.
##  If a set of generators has already been computed, this
##  is the set returned.
############################################################################
#InstallOtherMethod(GeneratorsOfAffineSemigroup,
#        "Computes a set of generators of the affine semigroup",
#        [IsAffineSemigroup],2,
#        function(S)
#  local  basis, eq;

#  if HasGeneratorsAS(S) then
#     return GeneratorsAS(S);
#   fi;
#   # REQUERIMENTS: NormalizInterface
#   if IsAffineSemigroupByEquations(S) then
#       eq:=EquationsAS(S);
#       basis := HilbertBasisOfSystemOfHomogeneousEquations(eq[1],eq[2]);
#       SetGeneratorsAS(S,basis);
#       return basis;
#   elif IsAffineSemigroupByInequalities(S) then
#       basis := HilbertBasisOfSystemOfHomogeneousInequalities(InequalitiesAS(S));
#       SetGeneratorsAS(S,basis);
#       return basis;
#   fi;
# end);
