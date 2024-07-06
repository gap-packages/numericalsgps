
###########################################################################
##

# to be executed inside the folder "/home/mdelgado/git/pkg/numericalsgps/data/genus"

dataGenus := function(n)
        local list, L, W, filename;

    L := NumericalSemigroupsWithGenus(n);
    W := List(L, s -> [MinimalGenerators(s),SmallElements(s)]);

    filename := Concatenation("genus",String(n),".gl");
    PrintTo(filename,W);
    Exec(Concatenation("gzip"," ", filename));
end;


for n in [1..22] do
    dataGenus(n);
#test
    list:=EvalString(StringFile(Concatenation("genus",String(n),".gl.gz")));
    Print(Length(list),"\n");
od;
###########################################################################
##
# to be executed inside the folder "/home/mdelgado/git/pkg/numericalsgps/data/frobenius"

dataFrobenius := function(n)
        local list, L, W, filename;

    L := NumericalSemigroupsWithFrobeniusNumber(n);
    W := List(L, s -> [MinimalGenerators(s),SmallElements(s)]);

    filename := Concatenation("frobenius",String(n),".gl");
    PrintTo(filename,W);
    Exec(Concatenation("gzip"," ", filename));
end;


for n in [1..32] do
    dataFrobenius(n);
#test
    list:=EvalString(StringFile(Concatenation("frobenius",String(n),".gl.gz")));
    Print(Length(list),"\n");
od;

###########################################################################
##
# to be executed inside the folder "/home/mdelgado/git/pkg/numericalsgps/data/maxprim"

dataMaxPrim := function(n)
        local list, L, W, filename;

    L := NumericalSemigroupsWithMaxPrimitive(n);
#    L := List(list, gens -> NumericalSemigroup(gens));
    W := List(L, s -> [MinimalGenerators(s),SmallElements(s)]);

    filename := Concatenation("maxprim",String(n),".gl");
    PrintTo(filename,W);
    Exec(Concatenation("gzip"," ", filename));
end;


for n in [1..32] do
    dataMaxPrim(n);
#test
    list:=EvalString(StringFile(Concatenation("maxprim",String(n),".gl.gz")));
    Print(Length(list),"\n");
od;
