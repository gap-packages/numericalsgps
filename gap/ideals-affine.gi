#############################################################################
##
#W  ideals-affine.gi           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
#W                          Helena Martin Cruz <Helena.mc18@gmail.com>
##
##
#Y  Copyright 2019 by Manuel Delgado,
#Y  Pedro Garcia-Sanchez and Helena Martin Cruz
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
#####################        Defining Affine Ideals           ###############
#############################################################################
##
#F IdealOfAffineSemigroup(l,S)
##
## l is a list of integers tuples and S an affine semigroup
##
## returns the ideal of S generated by l.
##
#############################################################################

InstallGlobalFunction(IdealOfAffineSemigroup, function(l,S)
    local  I, gens;

    if not IsAffineSemigroup(S) then
        Error("The second argument must be an affine semigroup.");
    fi;

    if l=[] then
        I := Objectify(IdealsOfAffineSemigroupsType, rec());
        SetUnderlyingASIdeal(I,S);
        SetGenerators(I,Set([]));
        return I;
    fi;

    if IsListOfIntegersNS(l) then
        gens:=[l];
        if Dimension(S)<>Length(l) then 
        Error("The length of the generator must be equal to the dimension of the affine semigroup.");
        fi;    
    elif not(IsRectangularTable(l) and ForAll(l,IsListOfIntegersNS)) then
        Error("The first argument must be a list of lists with the same length, or a list of integers");
    fi;
    if IsRectangularTable(l) then
        gens:=l;
        if Dimension(S)<>Length(l[1]) then
            Error("The length of the lists (generators) must be equal to the dimension of the affine semigroup.");
        fi;
    fi;
    
    I := Objectify(IdealsOfAffineSemigroupsType, rec());
    SetUnderlyingASIdeal(I,S);
    SetGenerators(I,Set(gens));

    return I;
end );

InstallMethod(IsEmpty, "for ideals of affine semigroups", [IsIdealOfAffineSemigroup], 
    function(I)
        return Generators(I)=[];
    end);


##############################################################################
## L is a list of integers tuples and S an affine semigroup
## L + S is an abbreviation for IdealOfAffineSemigroup(L, S)
##############################################################################

InstallOtherMethod(\+,
    "for a list and an affine semigroup", true,
    [IsRectangularTable, IsAffineSemigroup], function(l,S)
    return IdealOfAffineSemigroup(l,S);
end);

##############################################################################
## x is an integer tuple and S an affine semigroup
## x + S is an abbreviation for IdealOfAffineSemigroup([x], S)
##############################################################################

InstallOtherMethod(\+,
    "for an integer tuple and an affine semigroup", true,
    [IsList, IsAffineSemigroup], function(x,S)
    return IdealOfAffineSemigroup(x,S);
end);

#############################################################################
##
#M  PrintObj(S)
##
##  This method for ideals of affine semigroups.
##
#############################################################################

InstallMethod(PrintObj,
    "prints an ideal of an affine semigroup",
    [IsIdealOfAffineSemigroup], function(I)
    Print(Generators(I)," + AffineSemigroup( ", GeneratorsOfAffineSemigroup(UnderlyingASIdeal(I)), " )\n");
end);

#############################################################################
##
#M  ViewString(S)
##
##  This method for ideals of affine semigroups.
##
#############################################################################

InstallMethod(ViewString,
    "prints an ideal of an affine semigroup",
    [IsIdealOfAffineSemigroup], function(I)
    return ("Ideal of affine semigroup");
end);

#############################################################################
##
#M  ViewObj(S)
##
##  This method for ideals of affine semigroups.
##
#############################################################################

InstallMethod(ViewObj,
    "prints an ideal of an affine semigroup",
    [IsIdealOfAffineSemigroup], function(I)
    Print("<Ideal of affine semigroup>");
end);



#############################################################################
##
#F AmbientAffineSemigroupOfIdeal(I)
##
##  Returns the ambient affine semigroup of the ideal I.
############################################################################

InstallGlobalFunction(AmbientAffineSemigroupOfIdeal, function(I)
    if not IsIdealOfAffineSemigroup(I) then
        Error("The argument must be an ideal of an affine semigroup.");
    fi;
    return UnderlyingASIdeal(I);
end);

#############################################################################
##
#P  IsIntegralIdealOfAffineSemigroup(I)
##
##  Detects if the ideal I is contained in its ambient affine semigroup
##
#############################################################################

InstallMethod(IsIntegralIdealOfAffineSemigroup,
  "Test it the ideal is integral", [IsIdealOfAffineSemigroup], function(I)
    local s;

    s := AmbientAffineSemigroupOfIdeal(I);

    return IsSubset(s,MinimalGeneratingSystemOfIdealOfAffineSemigroup(I));
end);



#############################################################################
##
#F SumIdealsOfAffineSemigroup(I,J)
##
## returns the sum of the ideals I and J (in the same ambient affine semigroup)
#############################################################################

InstallGlobalFunction(SumIdealsOfAffineSemigroup, function(I,J)
    local   l1,  l2,  l;

    if not (IsIdealOfAffineSemigroup(I) and IsIdealOfAffineSemigroup(J))
       or not AmbientAffineSemigroupOfIdeal(I)
       = AmbientAffineSemigroupOfIdeal(J) then
        Error("The arguments must be ideals of the same affine semigroup.");
    fi;

    l1 := GeneratorsOfIdealOfAffineSemigroup(I);
    l2 := GeneratorsOfIdealOfAffineSemigroup(J);
    l := Set(Cartesian(l1,l2),n -> Sum(n));

    return IdealOfAffineSemigroup(l, AmbientAffineSemigroupOfIdeal(I));
end);

#############################################################################
## I + J means SumIdealsOfAffineSemigroup(I,J)
#############################################################################

InstallOtherMethod(\+,
    "for ideals of the same Affine semigroup", true,
    [IsIdealOfAffineSemigroup, IsIdealOfAffineSemigroup], function(I,J)
    return SumIdealsOfAffineSemigroup(I,J);
end);



#############################################################################
##
#F UnionIdealsOfAffineSemigroup(I,J)
##
## returns the union of the ideals I and J (in the same ambient affine semigroup)
#############################################################################

InstallGlobalFunction(UnionIdealsOfAffineSemigroup, function(I,J)
    local l1, l2, l;
    
    if not (IsIdealOfAffineSemigroup(I) and IsIdealOfAffineSemigroup(J))
       or not AmbientAffineSemigroupOfIdeal(I)
       = AmbientAffineSemigroupOfIdeal(J) then
        Error("The arguments must be ideals of the same affine semigroup.");
    fi;

    l1 := GeneratorsOfIdealOfAffineSemigroup(I);
    l2 := GeneratorsOfIdealOfAffineSemigroup(J);
    l := Set(Union(l1,l2));

    return IdealOfAffineSemigroup(l, AmbientAffineSemigroupOfIdeal(I));
end);


InstallMethod(Union2, [IsIdealOfAffineSemigroup, IsIdealOfAffineSemigroup], function(I,J)
  return UnionIdealsOfAffineSemigroup(I,J);
end);


#############################################################################
##
#F IntersectionPrincipalIdealsOfAffineSemigroup(I,J)
##
## returns the intersection of the principal ideals I and J (in the same ambient affine semigroup)
#############################################################################

InstallMethod(IntersectionPrincipalIdealsOfAffineSemigroup,[IsIdealOfAffineSemigroup,IsIdealOfAffineSemigroup],1,function(I,J)
    local a, b, S, l, n, A, A2, P, x, res, i;
    
    if not (Length(MinimalGenerators(I))=1 and Length(MinimalGenerators(J))=1)
    or not AmbientAffineSemigroupOfIdeal(I) = AmbientAffineSemigroupOfIdeal(J) then
        Error("The arguments must be principal ideals of the same affine semigroup.");
    fi;

    a := MinimalGenerators(I)[1];
    b := MinimalGenerators(J)[1];
    S := AmbientAffineSemigroupOfIdeal(I);
    l := MinimalGenerators(S);
    n := Length(l);
    A := TransposedMat(l);
    A2 := TransposedMat(Concatenation(l,-l,[-b+a]));
    P := Filtered(HilbertBasisOfSystemOfHomogeneousEquations(A2,[]),l->l[2*n+1]=1);
    if Length(P) = 0 then
        return Set([]);
    fi;
    
    x := P{[1..Length(P)]}{[1..n]};
    res := [];
    for i in x do
        Append(res,[a + A*i]);
    od;
    return IdealOfAffineSemigroup(res, S);

end);



#############################################################################
##
#F IntersectionIdealsOfAffineSemigroup(I,J)
##
## returns the intersection of the ideals I and J (in the same ambient affine semigroup)
#############################################################################

InstallGlobalFunction(IntersectionIdealsOfAffineSemigroup, function(I,J)
    local S, l1, l2, res, i, j, it;
    
    if not (IsIdealOfAffineSemigroup(I) and IsIdealOfAffineSemigroup(J))
       or not AmbientAffineSemigroupOfIdeal(I)
       = AmbientAffineSemigroupOfIdeal(J) then
        Error("The arguments must be ideals of the same affine semigroup.");
    fi;

    S := AmbientAffineSemigroupOfIdeal(I);
    l1 := MinimalGenerators(I);
    l2 := MinimalGenerators(J);
    res := []+S;
    for i in l1 do
        for j in l2 do
            it := IntersectionPrincipalIdealsOfAffineSemigroup(i+S, j+S);
            res:=Union(res,it);
        od;
    od;

    return res;
end);

InstallMethod(Intersection2, [IsIdealOfAffineSemigroup, IsIdealOfAffineSemigroup], function(I,J)
  return IntersectionIdealsOfAffineSemigroup(I,J);
end);


#############################################################################
##
#F  BelongsToIdealOfAffineSemigroup(n,I)
##
##  Tests if the integer tuple n belongs to the ideal I.
##
#############################################################################

InstallGlobalFunction(BelongsToIdealOfAffineSemigroup, function(x, I)
    local gI, S, small;

    if not (IsIdealOfAffineSemigroup(I) and IsListOfIntegersNS(x))  then
        Error("The arguments must be an integer tuple and an ideal of an affine semigroup.");
    elif Dimension(AmbientAffineSemigroupOfIdeal(I)) <> Length(x) then
        Error("The element to test must be a list of n positive integers, where n is the dimension of the affine semigroup.");
    fi;

    S := AmbientAffineSemigroupOfIdeal(I);
    gI := GeneratorsOfIdealOfAffineSemigroup(I);

    return(First(gI, n -> (BelongsToAffineSemigroup(x-n,S))) <> fail);
end);



#############################################################################
## n in I means BelongsToIdealOfAffineSemigroup(n,I)
#############################################################################

InstallMethod( \in,
    "for ideals of affine semigroups", [IsList, IsIdealOfAffineSemigroup],
    function(x,I)
        return BelongsToIdealOfAffineSemigroup(x,I);
end);



#############################################################################
##
#F MultipleOfIdealOfAffineSemigroup(n,I)
##
## n is a non negative integer and I is an ideal
## returns the multiple nI (I+...+I n times) of I
#############################################################################

InstallGlobalFunction(MultipleOfIdealOfAffineSemigroup, function(n,I)
    local i, II;

    if not (IsInt(n) and n >=0 and IsIdealOfAffineSemigroup(I)) then
        Error("The arguments must be a non negative integer and an ideal.");
    fi;

    if n=0 then
        return 0*[1..Dimension(AmbientAffineSemigroupOfIdeal(I))]+AmbientAffineSemigroupOfIdeal(I);
    elif n=1 then
        return I;
    fi;

    II := I;

    for i in [1..n-1] do
        II := II+I;
    od;

    return II;
end);



############################################################################
## n is an integer and S an affine semigroup
## n * I is an abbreviation for MultipleOfIdealOfAffineSemigroup(n,I)
############################################################################

InstallOtherMethod( \*,
    "for a non negative integer and an ideal of an affine semigroup", true,
    [IsInt and IsMultiplicativeElement, IsIdealOfAffineSemigroup],
    function(n,I)
        return MultipleOfIdealOfAffineSemigroup(n,I);
end);



#############################################################################
##
#A MinimalGenerators(I)
#A MinimalGeneratingSystem(I)
#A MinimalGeneratingSystemOfIdealOfAffineSemigroup(I)
##
## The argument I is an ideal of an affine semigroup
## returns the minimal generating system of I.
##
#############################################################################

InstallMethod(MinimalGeneratingSystemOfIdealOfAffineSemigroup,
    "Returns the minimal generating system of an ideal",
    [IsIdealOfAffineSemigroup], function(I)
        local  S, gens;

        S := AmbientAffineSemigroupOfIdeal(I);
        gens := Generators(I);

        return Filtered(gens, x -> ForAll(Difference(gens,Set([x])), y ->
        BelongsToAffineSemigroup(x-y,S) = false));
end);



#############################################################################
##
#F  TranslationOfIdealOfAffineSemigroup(k,I)
##
##  Given an ideal I of an affine semigroup S and an integer k
##  returns an ideal of the affine semigroup S generated by
##  {i1+k,...,in+k} where {i1,...,in} is the system of generators of I.
##
#############################################################################

InstallGlobalFunction(TranslationOfIdealOfAffineSemigroup, function(k,I)
    local l;

    if not IsListOfIntegersNS(k) then
        Error("The first argument must be a list of integers");
    fi;
    if not IsIdealOfAffineSemigroup(I) then
        Error("The second argument must be an ideal of an affine semigroup");
    fi;

    if Dimension(AmbientAffineSemigroupOfIdeal(I))<>Length(k) then
        Error("Dimension of the ambient semigroup of the ideal and length of the translation vector do not coincide");
    fi;

    l := List(GeneratorsOfIdealOfAffineSemigroup(I), g -> g+k);

    return IdealOfAffineSemigroup(l, AmbientAffineSemigroupOfIdeal(I));
end);



##############################################################################
##
##  k is an integer and I an ideal of an affine semigroup.
##  k + I is an abbreviation for TranslationOfIdealOfAffineSemigroup(k, I)
##
##############################################################################

InstallOtherMethod( \+,
    "for an integer and an ideal of an affine semigroup", true,
    [IsList and IsAdditiveElement, IsIdealOfAffineSemigroup], function(k,I)
    return TranslationOfIdealOfAffineSemigroup(k, I);
end);


#############################################################################
##
#O  MaximalIdealOfNumericalSemigroup(S)
##
##  Returns the maximal ideal of S.
##
#############################################################################
InstallMethod(MaximalIdeal,
    "of a numerical semigroup",
    [IsAffineSemigroup],
    function(S)
        return MinimalGenerators(S)+S;
    end);
