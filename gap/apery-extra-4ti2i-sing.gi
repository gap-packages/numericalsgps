#############################################################################
##
#W  apery-extra-4ti2i-sing.gi         Ignacio Ojeda <mdelgado@fc.up.pt>
#W                                    Carlos Jesús Moreno Ávila
#W                                    Manuel Delgado <mdelgado@fc.up.pt>
#W                                    Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Universidad de Extremadura and Universidad de Granada, Spain
#############################################################################

InstallMethod(AperyList,
    "method using 4ti2 for the calculaction of the Apery set",
    [IsNumericalSemigroup],80,
    function( S )
    local v, tv, n, I, R, x, m, M, i, j, inM, ap, Ap;

    Info(InfoNumSgps,2,"Using 4ti2Interface and Singular for the calculation of the Apery set");

    v := MinimalGeneratingSystemOfNumericalSemigroup(S);
    n := Length( v );
    I := 4ti2Interface_groebner_matrix( TransposedMat([v]), v );
    R := PolynomialRing(GF(2),n-1);
    x := IndeterminatesOfPolynomialRing(R);
    m := [1];
    M := [0];
  	for i in [1 .. Length(I)] do
			for j in [1 .. n-1] do
	   		m[j+1] := m[j]*x[j]^((I[i][j+1]+AbsoluteValue(I[i][j+1]))/2);
			od;
			M[i] := m[n];
    od;
    M := Ideal(R,M);;
    inM := SingularInterface("kbase",[M],"ideal");
    inM := GeneratorsOfIdeal(inM);
    ap := [0];
    Ap := [0];
    for i in [1 .. Length(inM)] do
			for j in [1 .. n-1] do
	    	ap[j+1] := ap[j] + DegreeIndeterminate(inM[i],j)*v[j+1];
			od;
			Ap[i] := ap[n];
    od;
    return List([0..v[1]-1], i->First(Ap, y->(y-i) mod v[1]=0));
end);
