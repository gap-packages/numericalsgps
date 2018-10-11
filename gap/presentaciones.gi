#############################################################################
##
#W  presentaciones.gi       Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
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
#F  FortenTruncatedNCForNumericalSemigroups(l)
##
##  l contains the list of coefficients of a
##  single linear equation. fortenTruncated gives a minimal generator
##  of the affine semigroup of nonnegative solutions of this equation
##  with the first coordinate equal to one.
##
##  Used for computing minimal presentations.
##
#############################################################################
InstallGlobalFunction(FortenTruncatedNCForNumericalSemigroups, function(l)
    local   leq,  m,  solutions,  explored,  candidates,  x,  tmp;


    #  leq(v1,v2)
    #  Compares vectors (lists) v1 and v2, returning true if v1 is less than or
    #  equal than v2 with the usual partial order.
    leq := function(v1,v2)
        local v;
        #one should make sure here that the lengths are the same
        v:=v2-v1;
        return (First(v,n->n<0)=fail);
    end;
    ##  End of leq()  --

    m:=IdentityMat(Length(l));
    solutions:=[];
    explored:=[];
    candidates:=[m[1]];
    m:=m{[2..Length(m)]};
    while (not(candidates=[])) do
        x:=candidates[1];
        explored:=Union([x],explored);
        candidates:=candidates{[2..Length(candidates)]};
        if(l*x=0) then
            return x;
        else
            tmp:=List(Filtered(m,n->((l*x)*(l*n)<0)),y->y+x);
            tmp:=Difference(tmp,explored);
            tmp:=Filtered(tmp,n->(First(solutions,y->leq(y,n))=fail));
            candidates:=Union(candidates,tmp);
        fi;
    od;
    return fail;
end);


#############################################################################
##
#F  GraphAssociatedToElementInNumericalSemigroup(n,s)
##
##  Computes the graph associated to the element n
##  the numerical semigroup s.
##  Its vertices are those minimal generators m such that
##      n-m in s
##  Its edges are those pairs (m1,m2) of minimal generators
##      such that n-(m1+m2) in s.
#############################################################################
InstallGlobalFunction(GraphAssociatedToElementInNumericalSemigroup, function(n,s)
    local vertices, edges,msg;

    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;
    if(not(BelongsToNumericalSemigroup(n,s))) then
        Error(n," must be an element of ",s,".\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    vertices:=Filtered(msg,m->(BelongsToNumericalSemigroup(n-m,s)));
    edges:=Filtered(Filtered(Cartesian(vertices,vertices),e->e[1]<e[2]),
                   e->(BelongsToNumericalSemigroup(n-Sum(e),s)));

    return [vertices,edges];
end);





#############################################################################
##
#F  MinimalPresentationOfNumericalSemigroup(s)
##
##  For a numerical semigroup s, give a minimal presentation
##  the output is a list of pairs showing the relationship
##  between the minimal generators of s
##  the algorithm is the one given in
##  -J. C. Rosales, {\em An algorithmic method to compute a minimal
##  relation for any numerical semigroup}, Internat. J. Algebra Comput.
##  {\bf 6} (1996), no. 4, 441--455.
#############################################################################
InstallGlobalFunction(MinimalPresentationOfNumericalSemigroup, function(s)
    local   candidates,  pairs,  presentation,  pair,  n,  rclasses, msg;

    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if(msg=[1]) then
     return [];
    fi;

    candidates:=BettiElementsOfNumericalSemigroup(s);

    presentation:=[];
	for n in candidates do
		rclasses:=RClassesOfSetOfFactorizations(FactorizationsIntegerWRTList(n,msg));
		pairs:=List([2..Length(rclasses)],i-> [rclasses[1][1],rclasses[i][1]]);
		presentation:=Union(presentation,pairs);
	od;

    return presentation;

end);

InstallMethod(MinimalPresentation,
"Computes a minimal presentation of the numerical semigroup",
[IsNumericalSemigroup],
MinimalPresentationOfNumericalSemigroup
);

#############################################################################
##
#F  BettiElementsOfNumericalSemigroup(s)
##
##  For a numerical semigroup s, returns the elements whose associated graphs
##  are non-connected, or in other words, whose factorizations are used to
##  construct any minimal presentation for s
##
#############################################################################
InstallGlobalFunction(BettiElementsOfNumericalSemigroup, function(s)
    local   isconnected,  msg,  ap,  candidates;

    if(not(IsNumericalSemigroup(s))) then
        Error(s," must be a numerical semigroup.\n");
    fi;

    ##  detects if the graph associated to n is connected
	##  it uses adjacency matrix
	isconnected:=function(n)
		local i,j,k, adj, aa, c, vert;
		vert:=Filtered(msg, x->n-x in s);
		k:=Length(vert);
		adj:=NullMat(k,k);
		for i in [1..k-1] do
			for j in [i+1..k] do
				if (n-vert[i]-vert[j] in s) then
					adj[i][j]:=1;
					adj[j][i]:=1;
				fi;
			od;
		od;
		c:=IdentityMat(k);
		aa:=IdentityMat(k);
		for i in [1..k-1] do
			aa:=aa*adj;
			c:=c+aa;
		od;
		for i in [1..k] do
			for j in [i+1..k] do
				if c[i][j]=0 then
					return false;
				fi;
			od;
		od;
		return true;
	end;
    ##  End of isconnected()  --

    msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
    if(msg=[1]) then
     return [];
    fi;
    ap:=AperyListOfNumericalSemigroupWRTElement(s,msg[1]);
    ap:=ap{[2..Length(ap)]};    # I remove the zero,
                                #   minimal generators yield conneted graphs
    candidates:=Union(List(msg,n->List(ap,m->m+n)));
                                # Gn not conneted implies n=wi+minimalgenerator
                                #    thus these are the candidates
    return Filtered(candidates,n->not(isconnected(n)));
                                # choose n with nonconnected graphs
end);

InstallMethod(BettiElements,
"Computes the Betti elements of the numerical semigroup",
[IsNumericalSemigroup],
BettiElementsOfNumericalSemigroup
);

#############################################################################
##
#P  IsUniquelyPresentedNumericalSemigroup(s)
##
##  For a numerical semigroup s, checks it it has a unique minimal presentation
##  Based in GS-O
##
#############################################################################
InstallMethod(IsUniquelyPresentedNumericalSemigroup,
  "Tests if the semigroup ha essentialy a unique minimal presentation",
  [IsNumericalSemigroup],
  function(s)
	return ForAll(BettiElementsOfNumericalSemigroup(s), b->Length(FactorizationsElementWRTNumericalSemigroup(b,s))=2);
end);

#############################################################################
##
#P  IsGenericNumericalSemigroup(s)
##
##  For a numerical semigroup s, checks it it has a generic presentation,
##  that is, in every relation all minimal generators appear.
##  These semigroups are uniquely presented; see B-GS-G.
##
#############################################################################
InstallMethod(IsGenericNumericalSemigroup,
  "Tests if the semigroup has a generic presentation", [IsNumericalSemigroup],
  function(s)
	local mp;
	mp:=MinimalPresentationOfNumericalSemigroup(s);
	return ForAll(mp,p->Product(p[1]+p[2])<>0);
end);

InstallTrueMethod(IsUniquelyPresentedNumericalSemigroup, IsGenericNumericalSemigroup);

#############################################################################
##
#F ShadedSetOfElementInNumericalSemigroup(x,s)
## computes the shading set of x in s as defined in
## -  Székely, L. A.; Wormald, N. C. Generating functions for the Frobenius problem
##      with 2 and 3 generators. Math. Chronicle 15 (1986), 49–57.
#############################################################################
InstallGlobalFunction(ShadedSetOfElementInNumericalSemigroup, function(x,s)

	local msg;

    if not IsNumericalSemigroup(s) then
        Error("The second argument must be a numerical semigroup.\n");
    fi;

    if not ( x in s ) then
        Error("The first argument must be an element of the second.\n");
    fi;

	msg:=MinimalGeneratingSystemOfNumericalSemigroup(s);
	return Filtered(Combinations(msg), c-> (x-Sum(c)) in s);

end);

############################################################################
##
#F  DegreesOfPrimitiveElementsOfNumericalSemigroup(s)
##
## Computes the sets of elements in s, such that there exists a minimal
## solution to msg*x-msg*y = 0,  such that x,y are factorizations of s
##
#############################################################################
InstallGlobalFunction(DegreesOfPrimitiveElementsOfNumericalSemigroup,function(s)
# 	local l, n, facs, mat, ncone, nmzcone,nmzconeproperty;

#     if not IsNumericalSemigroup(s) then
#         Error("The argument must be a numerical semigroup.\n");
#     fi;

# 	if not IsPackageMarkedForLoading("NormalizInterface","0.0") then
# 		Error("The package NormalizInterface is not loaded.\n");
# 	fi;

# 	l:=ShallowCopy(MinimalGeneratingSystemOfNumericalSemigroup(s));
# 	n:=Length(l);
# 	mat:=[Concatenation(l,-l)];
# 	nmzcone:=ValueGlobal("NmzCone");
# 	ncone:=nmzcone(["equations",mat]);
# 	nmzconeproperty:=ValueGlobal("NmzConeProperty");
# 	facs:=nmzconeproperty(ncone,"HilbertBasis");
# 	facs:=Set(facs,m->m{[1..n]});
# 	return Set(facs, f-> f*l);
# end);
    local a;

    if not IsNumericalSemigroup(s) then
        Error("The argument must be a numerical semigroup.\n");
    fi;

    a:=AsAffineSemigroup(s);
    return Union(DegreesOfPrimitiveElementsOfAffineSemigroup(a));
end);
