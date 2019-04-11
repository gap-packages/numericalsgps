#############################################################################
##
#W  contributions.gi
##
##
#Y  The functions in this file have been implemented by researchers that do
#Y  not appear as authors of the package. References to its usage should be
#Y made as suggested in the manual
#Y
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
################################################################################################################
##
#P  IsGradedAssociatedRingNumericalSemigroupBuchsbaum(S)
##
##  Test for the Buchsbaum property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Mezzasalma, M. and Micale, V. "On the Buchsbaumness of the Associated Graded Ring
##  of a One-Dimensional Local Ring", Communications in Algebra, 37: 5, 1594 — 1603
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsGradedAssociatedRingNumericalSemigroupBuchsbaum,
	"Tests if the graded associated ring of the semigroup is Buchsbaum",[IsNumericalSemigroup],1,
	function(S)
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
#P  IsMpureNumericalSemigroup(S)
##
##  Test for the M-Purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsMpureNumericalSemigroup,
	"Determines if the semigroup is M-pure", [IsNumericalSemigroup],
	function(S)
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
#P  IsPureNumericalSemigroup(S)
##
##  Test for the purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsPureNumericalSemigroup,
	"Tests for purity", [IsNumericalSemigroup],
	function(S)
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
#P  IsGradedAssociatedRingNumericalSemigroupGorenstein(S)
##
##  Test for the Gorenstein property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Micale, V. and Sammartano, A. "On the Associated Ring of a Semigroup Ring", J. Commut. Algebra Volume 3, Number 2 (2011), 147-168.
##
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsGradedAssociatedRingNumericalSemigroupGorenstein,
	"Test for the Gorenstein property of the associated graded ring of a numerical semigroup ring", [IsNumericalSemigroup],
	function(S)
	if IsSymmetricNumericalSemigroup(S) and IsMpureNumericalSemigroup(S) and IsGradedAssociatedRingNumericalSemigroupBuchsbaum(S) then
		return true;
	fi;
	return false;
end);

##############################################################################################################
##
#P  IsGradedAssociatedRingNumericalSemigroupCI
##
##  Test for the Complete Intersection property of the associated graded ring of a numerical semigroup ring k[[S]]
##  Based on "When the associated graded ring of a semigroup ring is Complete Intersection"
##
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsGradedAssociatedRingNumericalSemigroupCI,
	"Test for the Complete Intersection property of the associated graded ring of a numerical semigroup ring",
	[IsNumericalSemigroup],
	function(S)
	if IsGradedAssociatedRingNumericalSemigroupCM(S) and IsAperySetGammaRectangular(S) then
		return true;
	fi;
	return false;
end);

##############################################################################################################
##
#P  IsAperySetGammaRectangular
##
##  Test for the Gamma-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsAperySetGammaRectangular,
	"Test for the Gamma-Rectangularity of the Apéry Set of a numerical semigroup", [IsNumericalSemigroup],
	function(S)
	local g, ni,  i, j, c,b, LL, G, G1;
	g:=MinimalGeneratingSystemOfNumericalSemigroup(S);
	ni:=Length(g);
	if ni <= 2 then
		return true;
	fi;
	c:=[];
	for i in [1..ni-1] do
		c[i]:=1;
		repeat
			c[i]:=c[i]+1;
			if BelongsToNumericalSemigroup(c[i]*g[i+1]-g[1],S) then
				break;
			fi;
			G:=GraphAssociatedToElementInNumericalSemigroup(c[i]*g[i+1],S);
			G1:=Difference(G[1],[g[i+1]]);
			LL:=List(G1, n-> MaximumDegreeOfElementWRTNumericalSemigroup(c[i]*g[i+1] - n,S));
			if Length(LL) >0 and Maximum(LL)>= c[i]-1 then
				break;
			fi;
		until false;
	od;
	if g[1]=Product(c) then
		return true;
	fi;
	return false;
end);
##############################################################################################################
##
#P  IsAperySetBetaRectangular
##
##  Test for the Beta-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsAperySetBetaRectangular,
	"Test for the Beta-Rectangularity of the Apéry Set of the numerical semigroup", [IsNumericalSemigroup],
	function(S)
	local g, ni, m, i, b;
	g:=MinimalGeneratingSystemOfNumericalSemigroup(S);
	ni:=Length(g);
	if ni <= 2 then
		return true;
	fi;
	m:=g[1];
	b:=[];
	for i in [1..ni-1] do
		b[i]:=1;
		repeat
			b[i]:=b[i]+1;
			if BelongsToNumericalSemigroup(b[i]*g[i+1]-g[1],S) or MaximumDegreeOfElementWRTNumericalSemigroup(b[i]*g[i+1],S)>b[i] then
				break;
			fi;
		until false;
	od;
	if Product(b)=g[1] then
		return true;
	fi;
	return false;
end);

##############################################################################################################
##
#P  IsAperySetAlphaRectangular
##
##  Test for the Alpha-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallMethod(IsAperySetAlphaRectangular,
	"Test for the Alpha-Rectangularity of the Apéry Set of the numerical semigroup", [IsNumericalSemigroup],
	function(S)
	local g, ni, m, i, a;
	g:=MinimalGeneratingSystemOfNumericalSemigroup(S);
	ni:=Length(g);
	if ni <= 2 then
		return true;
	fi;
	m:=g[1];
	a:=[];
	for i in [1..ni-1] do
		a[i]:=1;
		repeat
			a[i]:=a[i]+1;
			if BelongsToNumericalSemigroup(a[i]*g[i+1]-g[1],S) then
				break;
			fi;
		until false;
	od;
	if Product(a)=g[1] then
		return true;
	fi;
	return false;
end);

#####
# Filter implications
#
#IsGradedAssociatedRingNumericalSemigroupCI
#	=> IsGradedAssociatedRingNumericalSemigroupGorenstein
#	=> IsGradedAssociatedRingNumericalSemigroupCM
#	=> IsGradedAssociatedRingNumericalSemigroupBuchsbaum
InstallTrueMethod(IsGradedAssociatedRingNumericalSemigroupGorenstein,IsGradedAssociatedRingNumericalSemigroupCI);
InstallTrueMethod(IsGradedAssociatedRingNumericalSemigroupCM,IsGradedAssociatedRingNumericalSemigroupGorenstein);
InstallTrueMethod(IsGradedAssociatedRingNumericalSemigroupBuchsbaum,IsGradedAssociatedRingNumericalSemigroupCM);

#IsGradedAssociatedRingNumericalSemigroupGorenstein
#	=> IsMpureNumericalSemigroup
#	=> IsPureNumericalSemigroup
InstallTrueMethod(IsMpureNumericalSemigroup,IsGradedAssociatedRingNumericalSemigroupGorenstein);
InstallTrueMethod(IsPureNumericalSemigroup,IsMpureNumericalSemigroup);

#IsGradedAssociatedRingNumericalSemigroupCI
#	=> IsAperySetGammaRectangular
InstallTrueMethod(IsAperySetGammaRectangular,IsGradedAssociatedRingNumericalSemigroupCI);

#IsGradedAssociatedRingNumericalSemigroupGorenstein
#	=> IsSymmetricNumericalSemigroup
InstallTrueMethod(IsSymmetricNumericalSemigroup, IsGradedAssociatedRingNumericalSemigroupGorenstein);

#IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity
#	=> IsAperySetAlphaRectangular
#	=> IsAperySetBetaRectangular
#	=> IsAperySetGammaRectangular
#	=> IsFreeNumericalSemigroup
InstallTrueMethod(IsAperySetAlphaRectangular,IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity);
InstallTrueMethod(IsAperySetBetaRectangular,IsAperySetAlphaRectangular);
InstallTrueMethod(IsAperySetGammaRectangular,IsAperySetBetaRectangular);
InstallTrueMethod(IsFreeNumericalSemigroup,IsAperySetGammaRectangular);

#IsTelescopicNumericalSemigroup
# => IsAperySetBetaRectangular
#####
InstallTrueMethod(IsAperySetBetaRectangular,IsTelescopicNumericalSemigroup);



##############################################################################################################
##
#F  TypeSequenceOfNumericalSemigroup
##
##  Computes the type sequence of a numerical semigroup
##  Based on "Maximality properties in numerical semigroups and applications to one-dimensional analytically irreducible local domains"
##  V. Barucci, D. E. Dobbs, M. Fontana
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
InstallGlobalFunction(TypeSequenceOfNumericalSemigroup,function(S)
	local Ga,Sma,n,g,i,j,L,l,t,h;
	Ga:=GapsOfNumericalSemigroup(S);
	Sma:=SmallElementsOfNumericalSemigroup(S);
	n:=Length(Sma)-1;
	g:=Length(Ga);
	L:=[];
	for j in [1..g]  do
		i:=n+1;
		while BelongsToNumericalSemigroup(Ga[j]+Sma[i],S) do
			i:=i-1;
		od;
		L[j]:=i;
	od;
	t:=[];
	for h in [1..n] do
		t[h]:=0;
	od;
	for l in L do
		t[l]:=t[l]+1;
	od;
	return t;
end);

InstallMethod(TypeSequence,
    "of a numerical semigroup",
    [IsNumericalSemigroup],
    TypeSequenceOfNumericalSemigroup);

##########################################################
##
#F TorsionOfAssociatedGradedRingNumericalSemigroup(S)
## This function returns the set of elements in the numerical
## semigroup S corresponding to a K-basis of the torsion
## submodule of the associated graded ring of the numerical
## semigroup ring K[[S]]. It uses the Apery table
## as explained in [Benitez, Jafari, Zarzuela; Semigroup Forum, 2013]
##
## Implemented by A. Sammartano
###########################################################
InstallGlobalFunction(TorsionOfAssociatedGradedRingNumericalSemigroup,
        function(S)
    local AT, m, r, torsion, i, w, truelanding, j, q, l;
    AT:=AperyTableOfNumericalSemigroup(S);
    m:=Length(AT[1]);
    r:=Length(AT)-1;
    torsion:=[];
    for i in [1..m] do
        w:=AT[1][i];
        truelanding:= false;
        j:=r;
        while j > 0 and AT[j+1][i]>w do
            if AT[j][i]=AT[j+1][i] then
                truelanding:=true;
                break;
            fi;
            j:=j-1;
        od;
        if truelanding then
            q:=(AT[j][i]-w)/m;
            for l in [0..q-1] do
                Append(torsion,[w+m*l]);
            od;
        fi;
    od;
    return torsion;
end);


#################################################################################
##
#F BuchsbaumNumberOfAssociatedGradedRingNumericalSemigroup(S)
## This function returns the smallest non-negative integer k for which the
## associated graded ring G of a given numerical semigroup ring is k-Buchsbaum,
## that is, the least k for which the torsion submodule of G is annihilated by
## the k-th power of the homogeneous maximal ideal of G.
##
##  Implemented by A. Sammartano
##################################################################################
InstallGlobalFunction(BuchsbaumNumberOfAssociatedGradedRingNumericalSemigroup,
        function(S)
    local T, M, n, D, H, s, c, j, tbr;
    T:=TorsionOfAssociatedGradedRingNumericalSemigroup(S);
    if Length(T)=0 then
        return 0;
    fi;
    M:=MaximalIdealOfNumericalSemigroup(S);
    n:=0;
    while Length(T)>0 do
        n:=n+1;
        D:=DifferenceOfIdealsOfNumericalSemigroup(n*M,(n+1)*M);
        H:=Length(D);
        tbr:=[];
        for s in T do
            c:=MaximumDegreeOfElementWRTNumericalSemigroup(s,S);
            j:=1;
            while j<=H and MaximumDegreeOfElementWRTNumericalSemigroup(s+D[j],S)>c+n   do
                j:=j+1;
            od;
            if j=H+1 then
                Append(tbr,[s]);
            fi;
        od;
        T:=Set(T);
        for s in tbr do
            RemoveSet(T,s);
        od;
        T:=List(T);
    od;
    return n;
end);






#############################################################################
##
#F  OmegaPrimalityOfElementListInNumericalSemigroup(l,s)
##
##  Computes the omega primality of a list of elements l in S,
##  Implemented by Chris O'Neill.
##
#############################################################################
InstallGlobalFunction(OmegaPrimalityOfElementListInNumericalSemigroup,function(l,s)
    local frob, msg, values, bullets, omegas, n, i, j, b, w, toadd, omegaval, bl, bli, getbullets, setbullets, getomega, setomega;

    # Form of a bullet: (v,l)
    # v - value of bullet expression
    # l - sum of the components

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup");
    fi;
    if not ForAll(l, e -> e in s) then
        Error("The first argument must be a list of elements of the second");
    fi;


    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    frob:=FrobeniusNumberOfNumericalSemigroup(s);

    values := [-frob .. Maximum(l)];
    bullets := List(values, x->[]);
    omegas := List(values, x->0);

    getbullets:=function(n)  #add base case
        if 0-n in s then
            return [0];
        fi;
        return bullets[n+frob+1];
    end;

    getomega:=function(n)
        if n < -frob then
            return 0;
        fi;
        return omegas[n+frob+1];
    end;

    for n in values do
        bl := [];

        for i in [1..Length(msg)] do
            bli:=getbullets(n - msg[i]);
            for j in [1..Length(bli)] do
                if IsBound(bli[j]) then
                    b:=j;
                    w:=bli[j];
                    if w <> 0 then
                        b:=b+(n-msg[i]);
                    fi;

                    if not(b-1-n in s) then
                        b:=b+msg[i];
                        w:=w+1;
                    fi;
                    if IsBound(bl[b-n]) then
                        bl[b-n]:=Maximum([w,bl[b-n]]);
                    else
                        bl[b-n]:=w;
                    fi;
                fi;
            od;
        od;

        bullets[n+frob+1]:=bl;
        if n-msg[Length(msg)]+frob+1 > 0 then
            bullets[n-msg[Length(msg)]+frob+1]:=[];
        fi;

        omegas[n+frob+1]:=Maximum(bl);

        # for garbage collection
        # GASMAN("coillect");
    od;

    return List(l,x->getomega(x));

end);


#############################################################################
##
#F  FactorizationsElementListWRTNumericalSemigroup(l,s)
##
##  Computes the factorizations of a list of elements l in S,
##  Implemented by Chris O'Neill
##
#############################################################################
InstallGlobalFunction(FactorizationsElementListWRTNumericalSemigroup,function(l,s)
	local msg, factorizations, n, i, f, facts, toadd;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	factorizations:=[];

	for n in [1 .. Maximum(l)] do
		factorizations[n]:=[];

		for i in [1 .. Length(msg)] do
			if n-msg[i] >= 0 then
				facts:=[List(msg,x->0)];
				if n-msg[i] > 0 then
					facts:=factorizations[n-msg[i]];
				fi;

				for f in facts do
					toadd:=List(f);
					toadd[i]:=toadd[i]+1;
					Add(factorizations[n],toadd);
				od;
			fi;
		od;

		factorizations[n]:=Set(factorizations[n]);

		# allow garbage collection
		#if n > Maximum(msg) then
		#	factorizations[n-Maximum(msg)]:=[];
		#fi;
	od;

	return List(l,x->factorizations[x]);
end);

#############################################################################
##
#F  DeltaSetPeriodicityBoundForNumericalSemigroup(s)
##
##  Returns a bound on the start of periodic behavior for the delta sets of elements of S.
##  Implemented by Chris O'Neill
##
#############################################################################
InstallGlobalFunction(DeltaSetPeriodicityBoundForNumericalSemigroup,function(s)
	local msg, d, l, i, g, S, Sp;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	d:=Gcd(DeltaSetOfSetOfIntegers(msg));
	#d:=Gcd(List(BettiElementsOfNumericalSemigroup(s),x->Minimum(LengthsOfFactorizationsElementWRTNumericalSemigroup(x,s))));
	l:=Length(msg);
	S:=[];
	Sp:=[];

	if l <= 2 then
		return Product(msg);
	fi;

	for i in [2 .. l-1] do
		g:=Gcd([msg[i]-msg[1],msg[1]-msg[l],msg[l]-msg[i]]);
		S[i]:=CeilingOfRational(-(msg[2]*(msg[1]*d*g + (l-2)*(msg[1]-msg[i])*(msg[1]-msg[l])))/((msg[1]-msg[2])*g));
		Sp[i]:=CeilingOfRational((msg[l-1]*((l-2)*(msg[1]-msg[l])*(msg[l]-msg[i]) - d*msg[l]*g))/((msg[l-1]-msg[l])*g));
	od;

	return Maximum(Union(S,Sp));
end);


#############################################################################
##
#F  DeltaSetPeriodicityStartForNumericalSemigroup(n,s)
##
##  Returns the exact start of periodicity for the delta sets of elements of S.
##  Implemented by Chris O'Neill
##
#############################################################################
InstallGlobalFunction(DeltaSetPeriodicityStartForNumericalSemigroup,function(s)
	local msg, period, lengths, m, n, lens, deltas, delta, diss;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	period:=Lcm(msg[1],msg[Length(msg)]);
	n:=DeltaSetPeriodicityBoundForNumericalSemigroup(s);

	lengths:=List([1 .. msg[Length(msg)]],x->[]);
	lengths[1]:=[0];
	deltas:=List([1 .. period],x->[]);
	delta:=[];
	diss:=1;

	for m in [1 .. n+period+1] do
		lens:=Union(List([1 .. Length(msg)],i->List(lengths[Int((m-msg[i]) mod msg[Length(msg)])+1],l->l+1)));
		if Length(lens) > 1 then
			delta:=DeltaSetOfSetOfIntegers(lens);
			if delta <> deltas[Int(m mod period)+1] then
				diss:=m-period;
			fi;
		fi;

		lengths[Int(m mod msg[Length(msg)])+1]:=lens;
		deltas[Int(m mod period)+1]:=delta;
	od;

	return diss;
end);



#############################################################################
##
#F  DeltaSetListUpToElementWRTNumericalSemigroup(n,s)
##
##  Computes the delta sets of the elements of S up to and including n.
##  Implemented by Chris O'Neill
##
#############################################################################
InstallGlobalFunction(DeltaSetListUpToElementWRTNumericalSemigroup,function(n,s)
	local msg, lengths, deltas, m, lens;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	lengths:=List([1 .. msg[Length(msg)]],x->[]);
	lengths[1]:=[0];
	deltas:=List([1 .. n],x->[]);

	for m in [1 .. n] do
		lens:=Union(List([1 .. Length(msg)],i->List(lengths[Int((m-msg[i]) mod msg[Length(msg)])+1],l->l+1)));
		if Length(lens) > 0 then
			deltas[m]:=DeltaSetOfSetOfIntegers(lens);
		fi;
		lengths[Int(m mod msg[Length(msg)])+1]:=lens;
	od;

	return deltas;
end);


#############################################################################
##
#F  DeltaSetUnionUpToElementWRTNumericalSemigroup(n,s)
##
##  Computes the union of the delta sets of the elements of S up to and including n,
##  using a ring buffer to conserve memory.
##  Implemented by Chris O'Neill
##
#############################################################################
InstallGlobalFunction(DeltaSetUnionUpToElementWRTNumericalSemigroup,function(n,s)
	local msg, lengths, delta, m, lens;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	lengths:=List([1 .. msg[Length(msg)]],x->[]);
	lengths[1]:=[0];
	delta:=[];

	for m in [1 .. n] do
		lens:=Union(List([1 .. Length(msg)],i->List(lengths[Int((m-msg[i]) mod msg[Length(msg)])+1],l->l+1)));
		if Length(lens) > 0 then
			delta:=Union(delta,DeltaSetOfSetOfIntegers(lens));
		fi;
		lengths[Int(m mod msg[Length(msg)])+1]:=lens;
	od;

	return delta;
end);

#############################################################################
##
#F  DeltaSetOfNumericalSemigroup(s)
##
##  Computes the union of the delta sets of the elements of S up to the bound given in [TODO],
##  Implemented by Chris O'Neill
##
#############################################################################
InstallGlobalFunction(DeltaSetOfNumericalSemigroup,function(s)
	local msg;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if 1 in msg then 
        return [];
    fi;
	return DeltaSetUnionUpToElementWRTNumericalSemigroup(DeltaSetPeriodicityBoundForNumericalSemigroup(s)+msg[Length(msg)]-1,s);
end);

InstallMethod(DeltaSet,
    "for numerical semigroups",
    [IsNumericalSemigroup],
    DeltaSetOfNumericalSemigroup);

#############################################################################
##
#F  IsAdmissiblePattern(p)
##
##  p is the list of integers that are the coefficients of a pattern
##  returns true or false depending if p is admissible or not
##  see cite [BA-GS]
##
##  Implemented with Klara Stokes
##
#############################################################################
InstallGlobalFunction("IsAdmissiblePattern",function(p)
     local len;

     if not(IsListOfIntegersNS(p)) then
         Error("The argument must be a list of integers containing the coefficients of a pattern");
     fi;

     len:=Length(p);

     return ForAll([1..len], i->Sum(p{[1..i]})>=0);
end);


#############################################################################
##
#F  IsStronglyAdmissiblePattern(p)
##
##  p is the list of integers that are the coefficients of a pattern
##  returns true or false depending if p is strongly admissible or not
##  see cite [BA-GS]
##
#############################################################################
InstallGlobalFunction("IsStronglyAdmissiblePattern",function(p)
     local len, pp;

     if not(IsAdmissiblePattern(p)) then return false;
     fi;

     len:=Length(p);
     pp:=ShallowCopy(p);

     if p[1]>0 then
         pp[1]:=p[1]-1;
     else
         pp:=pp{[2..len]};
     fi;

     return IsAdmissiblePattern(pp);
end);



#############################################################################
##
#F  AsIdealOfNumericalSemigroup(I,T)
##  For an ideal I of a numerical semigroup S, and a numerical semigroup T,
##  detects if I is an ideal of T, and if so, returns I as an ideal of T
##  (otherwise it returns fail)
##
##  Implented with Klara Stokes  (see [Stokes])
##
#############################################################################
InstallGlobalFunction("AsIdealOfNumericalSemigroup", function(I,T)
     local seI, Ci, Ct;

     if not(IsNumericalSemigroup(T)) then
         Error("The second argument must be a numerical semigroup");
     fi;
     if not(IsIdealOfNumericalSemigroup(I)) then
         Error("The first argument must be an ideal of a numerical semigroup");
     fi;
     if not(IsIntegralIdealOfNumericalSemigroup(I)) then
         Error("The first argument must be an integral ideal of a numerical semigroup");
     fi;

     seI:=SmallElementsOfIdealOfNumericalSemigroup(I);
     Ci:=ConductorOfIdealOfNumericalSemigroup(I);
     Ct:=ConductorOfNumericalSemigroup(T);
     if IsSubset(T,seI) and (Ci >= Ct) then
         return seI+T;
     fi;
     Info(InfoNumSgps,2,"The first argument is not included in the second");
     return fail;
end);

#############################################################################
##
#F  BoundForConductorOfImageOfPattern(p, C)
##  Takes an admissible pattern p and calculates an upper bound of the
##  smallest element K in p(I) such that all integers larger than K is
##  contained in p(I), where I is an ideal of a numerical semigroup.
##  Instead of taking I as parameter, the function takes C, which is assumed
##  to be the smallest element in I such that all integers larger than C is
##  contained in I.
##
##  Implemented by Klara Stokes (see [Stokes])
##
#############################################################################
InstallGlobalFunction("BoundForConductorOfImageOfPattern", function(p,C)
     local a,b,n,i,s;
     if not IsAdmissiblePattern(p) then
         Error("The first argument must be an admissible pattern.");
     fi;
     if not C in Integers or C< 0 then
         Error("The second argument must be a positive integer.");
     fi;
     s:=[];
     n:=Length(p);
     b:=GcdRepresentation(p );
     a:=Sum(p)-1;
     s[n]:=C-Minimum(0,a*b[n]);
     for i in [1..n-1] do
         s[n-i]:=s[n-i+1]+Maximum(0,a*(b[n-i+1]-b[n-i]));
     od;
     return p*s;
end);

#############################################################################
##
#F ApplyPatternToIdeal(p,I)
## Takes a strongly  admissible pattern p and calculates p(I), where I is
## an ideal of a numerical semigroup
##
## Implemented by Klara Stokes (see [Stokes])
##
#############################################################################
InstallGlobalFunction("ApplyPatternToIdeal",function(p,I)
     local C,S,q,d,n,s2,s1,stop,y,sum, i,m,s,ni,a,N;
     if not IsIdealOfNumericalSemigroup(I) then
         Error("The second argument must be an ideal of a numerical semigroup.");
     fi;
     if not(IsIntegralIdealOfNumericalSemigroup(I)) then
         Error("The second argument must be an integral ideal of a numerical semigroup");
     fi;
     if not IsStronglyAdmissiblePattern(p ) then
         Error("The first argument must be an admissible pattern.");
     fi;
     n:=Length(p);
     d:=Gcd(p);
     q:=p/d;
     C:=BoundForConductorOfImageOfPattern(q,ConductorOfIdealOfNumericalSemigroup(I));
     m:=0;
     S:=[];
     if 0 in I then
         Add(S,q*(0*[1..n]));
     fi;
     repeat
         m:=m+1;
         if not (m-1 in I) then stop:=false; continue; fi;
         stop:=true;
         a:=0;
         repeat
             s:=[1..n];
             ni:=true;
             s[1]:=m-1;
             for i in [2..n-1] do
                 s[i]:=RemInt(QuoInt(a,m^(n-i)),m);
                 if s[i]>s[i-1] or not s[i] in I then ni:=false; break; fi;
             od;
             s[n]:=RemInt(a,m);
             if s[n]>s[n-1] or not s[n] in I then ni:=false;fi;
             if ni then
                 y:=q*s;
                 if y<=C then
                     stop:=false;
                     Add(S,y);
                 fi;
             fi;
             a:=a+1;
         until a=m^(n-1);
     until stop;
     if 0 in S then
         return [d,0+NumericalSemigroupBySmallElements(Set(S))];
     else
         Add(S,0);
         N:=NumericalSemigroupBySmallElements(Set(S));
         return [d,MaximalIdealOfNumericalSemigroup(N)];
     fi;
end);

#############################################################################
##
#F ApplyPatternToNumericalSemigroup(p,S)
## Takes a strongly  admissible pattern p and calculates p(S), where S is
## a numerical semigroup
##
## Implemented by Klara Stokes (see [Stokes])
##
#############################################################################
InstallGlobalFunction("ApplyPatternToNumericalSemigroup",function(p,S)
     if not IsNumericalSemigroup(S) then
         Error("The second argument must be a numerical semigroup");
     fi;
     if not IsStronglyAdmissiblePattern(p) then
         Error("The first argument must be an strongly admissible pattern");
     fi;

     return ApplyPatternToIdeal(p,0+S);
 end);

#############################################################################
##
#F  IsAdmittedPatternByIdeal(p,I,J)
##
##  Takes astrongly admissible pattern p and tests whether p(I) is
##  contained in J, for I and J ideals of numerical semigroups
##  (not necessarily the same one)
##
##  Implemented by Klara Stokes  (see [Stokes])
##
#############################################################################
InstallGlobalFunction("IsAdmittedPatternByIdeal", function(p,I,J)
     local C,S,q,d,n,s2,s1,stop,y,sum,  i, s,m,a,ni;
     if not IsIdealOfNumericalSemigroup(I) then
         Error("The second argument must be an ideal of a numerical semigroup.");
     fi;
     if not(IsIntegralIdealOfNumericalSemigroup(I)) then
         Error("The second argument must be an integral ideal of a numerical semigroup");
     fi;
     if not IsIdealOfNumericalSemigroup(J) then
         Error("The third argument must be an ideal of a numerical semigroup.");
     fi;
     if not(IsIntegralIdealOfNumericalSemigroup(J)) then
         Error("The third argument must be an integral ideal of a numerical semigroup");
     fi;
     if not IsStronglyAdmissiblePattern(p ) then
         Error("The first argument must be a strongly admissible pattern.");
     fi;
     n:=Length(p);
     C:=ConductorOfIdealOfNumericalSemigroup(J);
     m:=0;
     if 0 in I then
         if not p*(0*[1..n]) in J then return false; fi;
     fi;
     repeat
         m:=m+1;
         if not (m-1 in I) then stop:=false; continue; fi;
         stop:=true;
         a:=0;
         repeat
             s:=[1..n];
             ni:=true;
             s[1]:=m-1;
             for i in [2..n-1] do
                 s[i]:=RemInt(QuoInt(a,m^(n-i)),m);
                 if s[i]>s[i-1] or not s[i] in I then ni:=false; break; fi;
             od;
             s[n]:=RemInt(a,m);
             if s[n]>s[n-1] or not s[n] in I then ni:=false;fi;
             if ni then
                 y:=p*s;
                 if y<C then
                     stop:=false;
                     if not y in J then return false; fi;
                 fi;
             fi;
             a:=a+1;
         until a=m^(n-1);
     until stop;
     return true;
end);

#############################################################################
##
#F  IsAdmittedPatternByNumericalSemigroup(p,S,T)
##  Takes a strongly  admissible pattern p and tests whether p(S) is
##  contained in T, for S and T numerical semigroups.
##
##  Implemented by Klara Stokes  (see [Stokes])
##
#############################################################################
InstallGlobalFunction("IsAdmittedPatternByNumericalSemigroup",function(p,S,T)
     if not IsNumericalSemigroup(S) then
         Error("The second argument must be a numerical semigroup");
     fi;
     if not IsNumericalSemigroup(T) then
         Error("The third argument must be a numerical semigroup");
     fi;
     return IsAdmittedPatternByIdeal(p,0+S,0+T);
end);
