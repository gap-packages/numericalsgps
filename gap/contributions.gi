#############################################################################
##
#W  contributions.gi          
##
##
#H  @(#)$Id: contributions.gi,v 0.971 $
##
#Y  The functions in this file have been implemented by researchers that do 
#Y  not appear as authors of the package. References to its usage should be 
#Y made as suggested in the manual
#Y  
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
################################################################################################################
##
#F  IsGradedAssociatedRingNumericalSemigroupBuchsbaum(S)
##
##  Test for the Buchsbaum property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Mezzasalma, M. and Micale, V. "On the Buchsbaumness of the Associated Graded Ring 
##  of a One-Dimensional Local Ring", Communications in Algebra, 37: 5, 1594 — 1603
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallGlobalFunction(IsGradedAssociatedRingNumericalSemigroupBuchsbaum, function(S)
	local M,h,r,T,m,D,Max,A1,A2,A3,A4,B1,B2,B3,B4;
	r:=ReductionNumberIdealNumericalSemigroup(MaximalIdealOfNumericalSemigroup(S));
	if r<=3 then
		return true;
	fi;
	M:=MaximalIdealOfNumericalSemigroup(S);
	T:=BlowUpOfNumericalSemigroup(S);
	m:=MultiplicityOfNumericalSemigroup(S);
	for h in [1..r-2] do
		D:= DifferenceOfIdealsOfNumericalSemigroup(h*M,(h+1)*M);
		A1:= (h+1)*m + T; 
		B1:= (h+2)*M - M;
		A2:= SmallElementsOfIdealOfNumericalSemigroup(A1);
		B2:= SmallElementsOfIdealOfNumericalSemigroup(B1);		
		Max:= Maximum(Maximum(A2), Maximum(B2), Maximum(D));
		A3:= Union(A2,[Maximum(A2)..Max]);
		B3:= Union(B2,[Maximum(B2)..Max]);
		A4:= Intersection(A3,D);
		B4:= Intersection(B3,D);
		if not(A4=B4) then
			return false;
		fi;
	od;
	return true;
end);

##############################################################################################################
##
#F  IsMpureNumericalSemigroup(S)
##
##  Test for the M-Purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated 
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallGlobalFunction(IsMpureNumericalSemigroup,function(S)
	local m, v, w, b, i, j, Maximal;
	m:=MultiplicityOfNumericalSemigroup(S);
	if m=1 then
		return true;
	fi;
	v:=AperyListOfNumericalSemigroupWRTElement(S,m);
	b:=List(AperyListOfNumericalSemigroupWRTElement(S,m),
	   w->MaximumDegreeOfElementWRTNumericalSemigroup(w,S));
	Maximal:=[];
	Maximal[1]:=false;	
	for i in [2..m] do
		Maximal[i]:=true;
	od;
	for i in [2..m] do
		j:=2;
		while (Maximal[i] and j <= m) do
			if v[i] + v[j] = v[((i+j-2) mod m) + 1] and b[i] + b[j] = b[((i+j-2) mod m) + 1] then
				Maximal[i]:=false;
				Maximal[j]:=false;
			fi;
		j:=j+1;
		od;
	od;
	for i in [2..m-1] do
		for j in [i+1..m] do
			if Maximal[i] and Maximal[j] and b[i]<>b[j] then
				return false;
			fi;
		od;
	od;
	return true;
end);

##############################################################################################################
##
#F  IsPureNumericalSemigroup(S)
##
##  Test for the purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated 
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallGlobalFunction(IsPureNumericalSemigroup,function(S)
	local m, T, b;
	m:=MultiplicityOfNumericalSemigroup(S);
	T:=PseudoFrobeniusOfNumericalSemigroup(S)+m;
	b:=List(T, w->MaximumDegreeOfElementWRTNumericalSemigroup(w,S));
	if Length(Set(b))=1 then
		return true;
	else
		return false;
	fi;
end);

##############################################################################################################
##
#F  IsGradedAssociatedRingNumericalSemigroupGorenstein(S)
##
##  Test for the Gorenstein property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Micale, V. and Sammartano, A. "On the Associated Ring of a Semigroup Ring", 
##  preprint
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallGlobalFunction(IsGradedAssociatedRingNumericalSemigroupGorenstein,function(S)
	if IsSymmetricNumericalSemigroup(S) and IsMpureNumericalSemigroup(S) and IsGradedAssociatedRingNumericalSemigroupBuchsbaum(S) then
		return true;
	fi;
	return false;
end);
