#############################################################################
##
#W  ideals-extra-ni.gi           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
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
#F IntersectionPrincipalIdealsOfAffineSemigroup(I,J)
##
## returns the intersection of the principal ideals I and J (in the same ambient affine semigroup)
## REQUERIMENTS: NormalizInterface
#############################################################################

InstallMethod(IntersectionPrincipalIdealsOfAffineSemigroup,[IsIdealOfAffineSemigroup,IsIdealOfAffineSemigroup],5,function(I,J)
    local a, b, S, l, n, A, A2, P1, P2, x, res, i;
    
    if not (Length(GeneratorsOfIdealOfAffineSemigroup(I))=1 and Length(GeneratorsOfIdealOfAffineSemigroup(J))=1)
       or not AmbientAffineSemigroupOfIdeal(I)
       = AmbientAffineSemigroupOfIdeal(J) then
        Error("The arguments must be principal ideals of the same affine semigroup.");
    fi;

    a := Generators(I)[1];
    b := Generators(J)[1];
    S := UnderlyingASIdeal(I);
    l := GeneratorsOfAffineSemigroup(S);
    n := Length(l);
    A := TransposedMat(l);
    A2 := TransposedMat(Concatenation(l,-l,[-b+a]));
    P1 := NmzCone(["inhom_equations", A2]);
    P2 := NmzModuleGenerators(P1);
    if Length(P2) = 0 then
        return Set([]);
    fi;
    
    x := P2{[1..Length(P2)]}{[1..n]};
    res := [];
    for i in x do
        Append(res,[a + A*i]);
    od;
    return IdealOfAffineSemigroup(res, S);

end);

##########################################################################
##
#O NormalizedIdeals(s)
##  Given a numerical semigroup S, returns the list of normalized ideals of S,
##  that is, the ideals I of S such that min(I)=0
##########################################################################
InstallMethod(NormalizedIdeals,
    "Computes the list of normalized ideals of a numerical semigroup",
    [IsNumericalSemigroup],5,
function(s)
    local c, points, m, isaperylistideal,I0ineq, idealByKunzCoordinates;
    # detects if a given list is the apery list of an ideal of s

    m:=Multiplicity(s);

    # computes the inequalities defining the cone of Kunz coordinates of 
    # normalized ideals of s
    I0ineq:=function(s)
        local k, ineq, m, i,j, id;
        m:=Multiplicity(s);
        id:=IdentityMat(m);
        ineq:=id{[1..m-1]}; # x_i>=0 para todo i in {1,...,m-1}
        k:=KunzCoordinates(s);
        for i in [1..m-1] do
            Add(ineq, -id[i]+k[i]*id[m]); # first inequalities from Th. 4.4, x_i<=k_i
        od;
        for i in [1..m-1] do
            for j in [1..m-1] do
                if (i+j)< m then
                    Add(ineq, k[j]*id[m]+id[i]-id[i+j]); #second family of inequalities Th. 4.4
                elif (i+j)>m then
                    Add(ineq, (k[j]+1)*id[m]+id[i]-id[i+j-m]); #third family
                fi;
            od;
        od;
        return ineq;
    end;

    # builds the ideal from its Kunz coordinates
    idealByKunzCoordinates:=function(k,s)
        local m;
        m:=Length(k)+1;
        return Concatenation([0],List([1..m-1],i->k[i]*m+i))+s;
    end;

    c:=NmzCone(["inhom_inequalities",I0ineq(s)]);
    points:=NmzLatticePoints(c);
    return List(points,p->idealByKunzCoordinates(p{[1..m-1]},s));
end);