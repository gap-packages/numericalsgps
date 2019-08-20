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