##
#if not TestPackageAvailability("4ti2Interface") = fail then
#    LoadPackage("4ti2");
#fi;
##


# using the parent InfoNumSgps
#InfoAffSgps:=NewInfoClass("InfoAffSgps");;
#SetInfoLevel(InfoAffSgps,1);;


######################################################################
# Computes the set of primitive elements of an affine semigroup, that
# is, the set of elements whose factorizations are involved in the
# minimal generators of the congruence associated to the monod
# (generators as a monoid; not to be confused with minimal presentations
# to this end, use BettiElementsOfAffineSemigroup)
# # REQUERIMENTS: 4ti2Interface
######################################################################

#InstallGlobalFunction(PrimitiveElementsOfAffineSemigroup,function(ls)
#    local dir, filename, exec, filestream, matrix,
#				 facs, mat, trunc;# ls;


	#if not(IsAffineSemigroup(a)) then
	#	Error("The argument must be an affine semigroup");
	#fi;

	#ls:=GeneratorsAS(a);

#    dir := DirectoryTemporary();
#    filename := Filename( dir, "gap_4ti2_temp_matrix" );
#
#	mat:=TransposedMat(ls);
#    4ti2Interface_Write_Matrix_To_File( mat, Concatenation( filename, ".mat" ) );
#    exec := IO_FindExecutable( "graver" );
#    filestream := IO_Popen2( exec, [ filename ]);
#    while IO_ReadLine( filestream.stdout ) <> "" do od;
#    matrix := 4ti2Interface_Read_Matrix_From_File( Concatenation( filename, ".gra" ) );
#
#    trunc:=function(ls)
#		return List(ls, y->Maximum(y,0));
#	end;

#	matrix:=Set(matrix,trunc);
#    return Set(matrix, x->x*ls);
#end);


#########
#InstallGlobalFunction(ElasticityOfAffineSemigroup,
#        function(ls)
#     local dir, filename, exec, filestream, matrix,
# 				  mat, truncplus, truncminus;

# 	if not(IsHomogeneousList(ls)) then
# 		Error("The argument must be a homogeneous list.");
# 	fi;

# 	if not(ForAll(ls,IsListOfIntegersNS)) then
# 		Error("The argument must be a list of lists of integers.");
# 	fi;

# 	if not(Length(Set(ls, Length))=1) then
# 		Error("All lists in the first argument must have the same length.");
# 	fi;

#     dir := DirectoryTemporary();
#     filename := Filename( dir, "gap_4ti2_temp_matrix" );

# 	mat:=TransposedMat(ls);
#     4ti2Interface_Write_Matrix_To_File( mat, Concatenation( filename, ".mat" ) );
#     exec := IO_FindExecutable( "graver" );
#     filestream := IO_Popen2( exec, [ filename ]);
#     while IO_ReadLine( filestream.stdout ) <> "" do od;
#     matrix := 4ti2Interface_Read_Matrix_From_File( Concatenation( filename, ".gra" ) );

#     truncplus:=function(ls)
# 		return Sum(List(ls, y->Maximum(y,0)));
# 	end;

#     truncminus:=function(ls)
# 		return Sum(List(ls, y->-Minimum(y,0)));
# 	end;

# 	return Maximum(Set(matrix, y->truncplus(y)/truncminus(y)));
# end);
#####





####################################################################
# Decides if the vector v belongs to the affine semigroup a
#
####################################################################
InstallMethod( \in,
        "for affine semigroups",
        [ IsHomogeneousList, IsAffineSemigroup],
        function( v, a )
    return BelongsToAffineSemigroup(v,a);
end);

InstallMethod(BelongsToAffineSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [ IsHomogeneousList, IsAffineSemigroup and HasGeneratorsAS],50,

        function(v,a)
    local belongs, gen;

      #determines if an element x is in the affine semigroup
      # spanned by gen
    belongs:=function(x,gen)
	if gen=[] then
            return false;
	fi;

	if ForAll(x,i->i=0) then
            return true;
	fi;

	if ForAny(x,i->i<0) then
            return false;
	fi;

	return belongs(x-gen[1],gen) or belongs(x,gen{[2..Length(gen)]});
    end;

    if not(IsAffineSemigroup(a)) then
        Error("The first argument must be an affine semigroup.");
    fi;

    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;

    gen:=GeneratorsAS(a);
    if not(IsRectangularTable(Concatenation(gen,[v]))) then
        Error("The dimension of the vector and the affine semigroup do not coincide.");
    fi;

    return belongs(v,gen);

end);

InstallMethod(BelongsToAffineSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [ IsHomogeneousList, IsAffineSemigroup and HasEquationsAS],100,

        function(v,a)
    local equ,eq,md,ev,i;


    if not(IsAffineSemigroup(a)) then
        Error("The first argument must be an affine semigroup.");
    fi;

    equ:=EquationsAS(a);
    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;
    if ForAny(v,x->x<0) then
        return false;
    fi;

    eq:=equ[1];
    md:=equ[2];
    if Length(eq[1])<>Length(v) then
        Error("The dimension of the vector and the affine semigroup do not coincide.");
    fi;
    ev:=ShallowCopy(eq*v);

    Info(InfoNumSgps,2,"Testing membership with equations.");

    for i in [1..Length(md)] do
        ev[i]:=ev[i] mod md[i];
    od;

    return ForAll(ev,x->x=0);

end);

InstallMethod(BelongsToAffineSemigroup,
        "To test whether an integer belongs to a numerical semigroup",
        true,
        [ IsHomogeneousList, IsAffineSemigroup and HasInequalitiesAS],70,

        function(v,a)
    local equ,ev;


    if not(IsAffineSemigroup(a)) then
        Error("The first argument must be an affine semigroup.");
    fi;

    equ:=InequalitiesAS(a);
    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;
    if ForAny(v,x->x<0) then
        return false;
    fi;

    if Length(equ[1])<>Length(v) then
        Error("The dimension of the vector and the affine semigroup do not coincide.");
    fi;
    ev:=equ*v;

    Info(InfoNumSgps,2,"Testing membership with inequalities.");

    return ForAll(ev,x->x>=0);

end);


#############################################################
# Computes a basis of a subgroup of Z^n with defining equations
# Ax =0 \in Z_m1\times\Z_mt \times Z^k, k is n-length(m),
# m=[m1,...,mt]
#############################################################
InstallGlobalFunction(BasisOfGroupGivenByEquations,function(A,m)
    local n,r, AA, i, er, b, homEqToBasis;

    # Compute a basis of a subgroup of Z^n with defining equations Ax=0
    homEqToBasis:=function(A)
	local snf, b, r, n;
	snf:= SmithNormalFormIntegerMatTransforms(A);
	r:=snf.rank;
	n:=Length(A[1]);
	b:=TransposedMat(snf.coltrans);
	return b{[r+1..n]};
    end;


    if not(IsRectangularTable(A)) then
        Error("The first argument must be a matrix.");
    fi;
    if not(IsInt(A[1][1])) then
        Error("The first argument must be a matrix of integers.");
    fi;

    n:=Length(A[1]);
    r:=Length(A);

    if m=[] then
        AA:=ShallowCopy(TransposedMat(A));
    else
        if not(IsListOfIntegersNS(m)) then
            Error("The second argument must be a lists of integers.");
        fi;

        if not(ForAll(m,x->x>0)) then
            Error("The second argument must be a list of positive integers");
        fi;

        AA:=ShallowCopy(TransposedMat(A));
        er:=List([1..r],_->0);
        for i in [1..Length(m)] do
            if m[i]<>0 then
                er[i]:=m[i];
                Add(AA,ShallowCopy(er));
                er[i]:=0;
            fi;
        od;
    fi;
    AA:=TransposedMat(AA);

    b:=TransposedMat(homEqToBasis(AA));

    b:=b{[1..n]};
    return LLLReducedBasis(TransposedMat(b)).basis;
end);

#############################################################
#  Computes the defining equations of the group of Z^n
#  generated by M
#  the output is [A,m] such that Ax=0 mod m are the equations
############################################################
InstallGlobalFunction(EquationsOfGroupGeneratedBy,function(M)
    local A, m, snf, nones;

    if not(IsRectangularTable(M)) then
        Error("The first argument must be a matrix.");
    fi;
    if not(IsInt(M[1][1])) then
        Error("The first argument must be a matrix of integers.");
    fi;

    snf:=SmithNormalFormIntegerMatTransforms(M);
    A:=TransposedMat(snf.coltrans);
    m:=DiagonalOfMat(snf.normal);
    nones:=Length(Filtered(m,x->x=1));

    m:=Filtered(DiagonalOfMat(snf.normal),x->x<>0);
    A:=A{[nones+1..Length(A)]};
    m:=m{[nones+1..Length(m)]};

    return [A,m];
end);


##############################################################
# Determines if there is a gluing of the two affine semigroups,
# and if so, returns the gluing of them; fail otherwise
##############################################################
InstallGlobalFunction(GluingOfAffineSemigroups,function(a1,a2)
    local int, d, intersectionOfSubgroups, g1, g2;


    #computes the intersection of two groups of Z^n
    # given by generators
    intersectionOfSubgroups:=function(g1,g2)
        local eq1, eq2, A, m;

        eq1:=EquationsOfGroupGeneratedBy(g1);
        eq2:=EquationsOfGroupGeneratedBy(g2);
        A:=Concatenation(eq1[1],eq2[1]);
        m:=Concatenation(eq1[2],eq2[2]);

        return BasisOfGroupGivenByEquations(A,m);
    end;

    if not(IsAffineSemigroup(a1)) or not(IsAffineSemigroup(a2)) then
        Error("The arguments must be affine semigroups.");
    fi;

    g1:=GeneratorsOfAffineSemigroup(a1);
    g2:=GeneratorsOfAffineSemigroup(a2);
    if not(IsRectangularTable(Concatenation(g1,g2)))then
        Error("The semigroups must have the same dimension.");
    fi;

    int:=intersectionOfSubgroups(g1,g2);
    if Length(int)<> 1 then
        return false;
    fi;

    d:=int[1];
    if ForAny(d, i->i<0) then
        d:=-d;
    fi;
    if BelongsToAffineSemigroup(d,a1) and
       BelongsToAffineSemigroup(d,a2) then
        return AffineSemigroup(Concatenation(g1,g2));
    fi;
    return fail;

end);

###################### ContejeanDevieAlgorithm

#############################################################################################
# l contains the list of coefficients of a system of linear equations. forten gives the
#  set of minimal generators of the affine semigroup of nonnegative soultions of this equation
##############################################################################################
InstallMethod(HilbertBasisOfSystemOfHomogeneousEquations,
        "Computes the Hilbert basis of a system of linear Diophantine equations, some evetually in congruences.",[IsMatrix,IsHomogeneousList],1,
  function(ls,md)
  local  contejeanDevieAlgorithm, contejeanDevieAlgorithmWithCongruences, leq;

  ## local functions ...
    #less than or equal to with the usual partial order
  leq:= function(v1,v2)
      local v;
      v:=v2-v1;
      return (First(v,n->n<0)=fail);
  end;

  contejeanDevieAlgorithm:= function(l)
    local solutions, m, x, explored, candidates, tmp, k,zero, lx;


    Info(InfoNumSgps,2,"Using Contejean and Devie algorithm.");


    solutions:=[];
    explored:=[];

    if not(IsRectangularTable(l)) then
      Error("The argument must be a matrix.");
    fi;
    if not(IsInt(l[1][1])) then
      Error("The matrix must be of integers.");
    fi;


    m:=IdentityMat(Length(l[1]));
    zero:=List([1..Length(l)],_->0);
    candidates:=m;
    while (not(candidates=[])) do
      x:=candidates[1];
      explored:=Union([x],explored);
      candidates:=candidates{[2..Length(candidates)]};
      lx:=l*x;
      if(lx=zero) then
        solutions:=Union([x],solutions);
        #    Print(x);
      else
        tmp:=Set(Filtered(m,n->(lx*(l*n)<0)),y->y+x);
        tmp:=Difference(tmp,explored);
        tmp:=Filtered(tmp,n->(First(solutions,y->leq(y,n))=fail));
        candidates:=Union(candidates,tmp);
      fi;
    od;
    return solutions;
  end;

  contejeanDevieAlgorithmWithCongruences:=function(ls,md)
    local l,n,m,diag,dim,d, hil, zero;

    if not(IsRectangularTable(ls)) then
      Error("The first argument must be a matrix.");
    fi;

    if not(IsListOfIntegersNS(md)) or ForAny(md, x->not(IsPosInt(x))) then
      Error("The second argument must be a list of positive integers.");
    fi;

    n:=Length(ls);
    dim:=Length(ls[1]);
    m:=Length(md);
    if m>n then
      Error("There are more modulus than equations.");
    fi;

    diag:=Concatenation(md,List([1..n-m],_->0));
    d:=DiagonalMat(diag);
    l:=TransposedMat(Concatenation(TransposedMat(ls),d,-d));
    zero:=List([1..dim],_->0);

    hil:=Difference(List(contejeanDevieAlgorithm(l), x->x{[1..dim]}),[zero]);
    return hil;

    return Filtered(hil, y->Filtered(hil,x->leq(x,y))=[y]);
  end;
  ## end of local functions ...

  #ls := arg[1][1];
  #md := arg[1][2];
  if md = [] then
    return contejeanDevieAlgorithm(ls);
  else
    return contejeanDevieAlgorithmWithCongruences(ls,md);

  fi;
end);

##############################################################################################
#
# ls is a matrix of integers. It computes the set minimal nonzero nonnegative integer solutions
# of ls*x>=0
#
InstallMethod(HilbertBasisOfSystemOfHomogeneousInequalities,
        "Computes the Hilbert basis of a set of inequalities",
        [IsMatrix],1,
        function(ls)
    local mat, neq, dim, id, hil,zero ;
    if not(IsRectangularTable(ls)) then
      Error("The argument must be a matrix.");
    fi;
    if not(IsInt(ls[1][1])) then
      Error("The matrix must be of integers.");
    fi;

    neq:=Length(ls);
    dim:=Length(ls[1]);
    zero:=List([1..dim],_->0);

    id:=IdentityMat(neq);
    mat:=TransposedMat(Concatenation(TransposedMat(ls),-id));
    hil:=HilbertBasisOfSystemOfHomogeneousEquations(mat,[]);
    return List(hil,x->x{[1..dim]});


end);

########################################################################
# Computes the set of factorizations of v in terms of the elements of ls
# That is, a Hilbert basis for ls*X=v
# If ls contains vectors that generate a nonreduced monoid, then it
# may enter in an infinite loop
########################################################################
InstallMethod(FactorizationsVectorWRTList,
        "Computes the set of factorizations of the first argument in terms of the elements of the second",
        [IsHomogeneousList, IsMatrix],1,
        function(v,ls)
    local len, e1, opt1, opt2, i, mat, dim;
    # REQUERIMENTS: NormalizInterface
    #if NumSgpsCanUseNI then
    #    TryNextMethod();
    #fi;

    mat:=TransposedMat(Concatenation(ls,[-v]));

    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;

    if not(ForAll(ls,IsListOfIntegersNS)) then
        Error("The second argument must be a list of lists of integers.");
    fi;

    if not(IsRectangularTable(mat)) then
        Error("The list in the second argument must have the same length as the lists in the first argument.");
    fi;

    len:=Length(ls);
    if ls=[] then
        return [];
    fi;
    if ForAll(v,x->x=0) then
        return [List([1..len],_->0)];
    fi;
    if ForAny(v,x->x<0) then
        return [];
    fi;

    if Length(ls)=1 then
        dim:=Length(ls[1]);

        i:=First([1..dim],x->ls[1][x]<>0);

        if i=fail then
            Error("The second argument cannot contain the zero vector.");
        fi;
        if (v[i] mod ls[1][i]=0) and v=v[i]/ls[1][i]*ls[1] then
            return [ [v[i]/ls[1][i]] ];
        fi;
        return [];
    fi;
    e1:=List([1..len-1],_->0);
    e1:=Concatenation([1],e1);
    opt1:=[];
    if ForAll(v-ls[1],x->x>=0) then
        opt1:=List(FactorizationsVectorWRTList(v-ls[1],ls), x->x+e1);
    fi;
    opt2:=List(FactorizationsVectorWRTList(v,ls{[2..len]}),
               x->Concatenation([0],x));
    return Concatenation(opt1,opt2);
end);

############################################################
# computes a set of generators of the kernel congruence
# of the monoid morphism associated to the matrix m with
# nonnegative integer coefficients
############################################################
InstallMethod(GeneratorsOfKernelCongruence,
	"Computes the minimal presentation of an affine semigroup",
	[IsRectangularTable],1,
	function( m )

    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
          R,id, ie, vars, mingen, exps, bintopair, dim, zero, gen,
          pres,c, rclass;

    # REQUERIMENTS: SingularInterface or Singular
    #if NumSgpsCanUseSI or NumSgpsCanUseSingular then
    #    TryNextMethod();
    #fi;

    ##computes the s degree of a monomial in the semigroup ideal
    sdegree:=function(m)
        local exp;
        exp:=List([1..ed], i->DegreeIndeterminate(m,i));
        return exp*msg;
    end;

    bintopair:=function(p)
        local m1,m2, d1, d2;
        m1:=LeadingMonomialOfPolynomial(p, MonomialLexOrdering());
        m2:=m1-p;
        d1:=List([1..ed], i->DegreeIndeterminate(m1,i));;
        d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
        return [d1,d2];
    end;

    if not(ForAll(m, l->ForAll(l, x->(x=0) or IsPosInt(x)))) then
        Error("The argument must be a matrix of nonnegative integer.");
    fi;

    msg:=ShallowCopy(m);
    ed:=Length(msg);
    if ed=0 then
        return [];
    fi;
    zero:=List([1..ed],_->0);
    dim:=Length(msg[1]);
    vars:=List([1..ed+dim],i->X(Rationals,i));
    R:=PolynomialRing(Rationals,vars);
    p:=List([1..ed], i->X(Rationals,i)-
            Product(List([1..dim], j->X(Rationals,j+ed)^msg[i][j])));
    rgb:=ReducedGroebnerBasis( p,
                 EliminationOrdering(List([1..dim],i->X(Rationals,i+ed))));
    rgb:=Filtered(rgb,
                 q->ForAll([1..dim], i->DegreeIndeterminate(q,i+ed)=0));
    candidates:=Set(rgb,q->bintopair(q));
    return candidates;
end);


############################################################
# computes a minimal presentation of a
############################################################
InstallMethod(MinimalPresentationOfAffineSemigroup,
	"Computes the minimal presentation of an affine semigroup",
	[IsAffineSemigroup],1,
	function( a )

    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
          R,id, ie, vars, mingen, exps, bintopair, dim, zero, gen,
          pres,c, rclass;


    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;

    msg:=GeneratorsOfAffineSemigroup(a); #for now we do not check minimality of the generators
    ed:=Length(msg);
    if ed=0 then
        return [];
    fi;
    dim:=Length(msg[1]);
    candidates:=GeneratorsOfKernelCongruence(msg);
    candidates:=Set(candidates,c->c[1]*msg);
    Info(InfoNumSgps,2, "Candidates to Betti elements",candidates);
    pres:=[];
    for c in candidates do
        exps:=FactorizationsVectorWRTList(c,msg);
        rclass:=RClassesOfSetOfFactorizations(exps);
        if Length(rclass)>1 then
            pres:=Concatenation(pres,List([2..Length(rclass)],
                          i->[rclass[1][1],rclass[i][1]]));
        fi;
    od;
    return pres;
end);

###################################################################
# Betti elements of the affine semigroup a
###################################################################
InstallMethod(BettiElementsOfAffineSemigroup,
	"Computes the Betti elements of an affine semigroup",
	[IsAffineSemigroup],1,
	function(a)
    local msg, pr;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;

    msg:=GeneratorsOfAffineSemigroup(a);

    pr:=MinimalPresentationOfAffineSemigroup(a);

    return Set(pr, p->p[1]*msg);

end);


#############################################################################
##
#F  IsUniquelyPresentedAffineSemigroup(a)
##
##  For an affine semigroup a, checks it it has a unique minimal presentation
##  Based in GS-O
##
#############################################################################
InstallGlobalFunction(IsUniquelyPresentedAffineSemigroup,function(a)
    local gs;
    if not IsAffineSemigroup(a) then
        Error("The second argument must be an affine semigroup.\n");
    fi;
    gs:=GeneratorsOfAffineSemigroup(a);
    return ForAll(BettiElementsOfAffineSemigroup(a),
                  b->Length(FactorizationsVectorWRTList(b,gs))=2);
end);

#############################################################################
##
#F  IsGenericAffineSemigroup(a)
##
##  For an affine semigroup a, checks it it has a generic presentation,
##  that is, in every relation all generators appear.
##  These semigroups are uniquely presented; see B-GS-G.
##
#############################################################################
InstallGlobalFunction(IsGenericAffineSemigroup,function(a)
	local mp;
    if not IsAffineSemigroup(a) then
        Error("The second argument must be an affine semigroup.\n");
    fi;
    mp:=MinimalPresentationOfAffineSemigroup(a);
    return ForAll(mp,p->Product(p[1]+p[2])<>0);
end);

#############################################################################
##
#F ShadedSetOfElementInAffineSemigroup(x,a)
## computes the shading set of x in a as defined in
## -  Székely, L. A.; Wormald, N. C. Generating functions for the Frobenius problem
##      with 2 and 3 generators. Math. Chronicle 15 (1986), 49–57.
#############################################################################
InstallGlobalFunction(ShadedSetOfElementInAffineSemigroup, function(x,a)

    local msg;

    if not IsAffineSemigroup(a) then
        Error("The second argument must be an affine semigroup.\n");
    fi;

    if not ( x in a ) then
        Error("The first argument must be an element of the second.\n");
    fi;

    msg:=GeneratorsOfAffineSemigroup(a);
    return Filtered(Combinations(msg), c-> (x-Sum(c)) in a);
end);



######################################################################
# Computes the catenary degree of the affine semigroup a
######################################################################
InstallGlobalFunction(CatenaryDegreeOfAffineSemigroup,
        function(a)
    local betti, b, max, c, ls;
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);

    Info(InfoNumSgps,2,"Computing the Betti elements of the affine semigroup.");
    betti:=BettiElementsOfAffineSemigroup(a);
    Info(InfoNumSgps,2,"The Betti elements are ",betti);
    max:=0;
    for b in betti do
        Info(InfoNumSgps,2,"Computing the catenary degree of ",b);
        c:=CatenaryDegreeOfSetOfFactorizations(
                   FactorizationsVectorWRTList(b,ls));
        Info(InfoNumSgps,2,"which equals ",c);
        if c>max then max:=c; fi;
    od;
    return max;
end);

######################################################################
# Computes the equal catenary degree of the affine semigroup a
# uses [GSOSN]
######################################################################
InstallGlobalFunction(EqualCatenaryDegreeOfAffineSemigroup,
        function(a)
    local ls, lsh, ah, primeq;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);
    lsh:=List(ls, x-> Concatenation(x,[1]));
    ah:=AffineSemigroup(lsh);
    primeq:=BettiElementsOfAffineSemigroup(ah);

    return Maximum(Set(primeq, x->CatenaryDegreeOfSetOfFactorizations(
            FactorizationsVectorWRTList(x,lsh))));

end);

######################################################################
# Computes the homogeneous catenary degree of the affine semigroup a
# uses [GSOSN]
######################################################################
InstallGlobalFunction(HomogeneousCatenaryDegreeOfAffineSemigroup,
        function(a)
    local ls, lsh, ah, primeq, one;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);
    if ls=[] then return 0;
    fi;

    lsh:=List(ls, x-> Concatenation(x,[1]));
    one:=List(ls[1],_->0);
    Add(one,1);
    Add(lsh,one);

    ah:=AffineSemigroup(lsh);
    primeq:=BettiElementsOfAffineSemigroup(ah);

    return Maximum(Set(primeq, x->CatenaryDegreeOfSetOfFactorizations(
            FactorizationsVectorWRTList(x,lsh))));

end);

######################################################################
# Computes the monotone catenary degree of the affine semigroup a
# uses [PH] and Alfredo Sanchez-R.-Navarro thesis
######################################################################
InstallGlobalFunction(MonotoneCatenaryDegreeOfAffineSemigroup,
        function(a)
    local ls, lsh, ah, primeq, one, dim;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);

    if ls=[] then return 0;
    fi;
    dim:=Length(ls[1]);

    lsh:=List(ls, x-> Concatenation(x,[1]));
    one:=List(ls[1],_->0);
    Add(one,1);
    Add(lsh,one);

    ah:=AffineSemigroup(lsh);
    primeq:=PrimitiveElementsOfAffineSemigroup(ah);
    primeq:=Set(primeq, x->x{[1..dim]});

    return Maximum(Set(primeq, x->MonotoneCatenaryDegreeOfSetOfFactorizations(
            FactorizationsVectorWRTList(x,ls))));

end);


###############################################################################
##
#O OmegaPrimalityOfElementInAffineSemigroup
#
# Computes the omega-primality of v in the monoid a
###########################################################################
InstallMethod(OmegaPrimalityOfElementInAffineSemigroup,
        "Computes the omega-primality of x in the monoid s",
        [IsHomogeneousList,IsAffineSemigroup],1,
        function(x,s)

    local i, j, p, rel, rgb, msg, pol, ed,  degree, monomial,  facts, fact, mp,id, reduce, nonnegative,
          mu1,A,B,C, lt, tl, exp, new;

    msg:=GeneratorsOfAffineSemigroup(s);
    ed:=Length(msg);
    mp:=MinimalPresentationOfAffineSemigroup(s);
    p := [];
    # list of exponents to monomial
    monomial:=function(l)
        local i;
        pol:=1;
        for i in [1..ed] do
            pol:=pol*Indeterminate(Rationals,i)^l[i];
        od;
        return pol;
    end;
    ## monomial to exponents
    exp:=function(mon)
        return List([1..ed],i-> DegreeIndeterminate(mon,i));
    end;
    ##computes the degree of a monomial
    degree:=function(mon)
        return Sum(exp(mon));
    end;
    ##nonnegative
    nonnegative:=function(l)
        return ForAll(l, x-> x>=0);
    end;

    for rel in mp do
        Add( p, monomial(rel[1])-monomial(rel[2]));
    od;

    facts:=FactorizationsVectorWRTList(x,msg);
    if facts=[] then
        return 0;
    fi;
    Info(InfoNumSgps,2,"Factorizations of the element :", facts);
    fact:=facts[Length(facts)];
    id:=IdentityMat(ed);

    for i in [1..ed] do
        Add(p,monomial(fact+id[i])-monomial(fact));
    od;

    # for j in [2..Length(facts)] do
    #     for i in [1..ed] do
    #         Add(p,monomial(facts[j]+id[i])-monomial(facts[j]));
    #     od;
    # od;

    Info(InfoNumSgps,2,"Computing a Groebner basis");
    #a canonical system of generators of sigma_I
    rgb := ReducedGroebnerBasis( p, MonomialGrevlexOrdering() );


    #normal form wrt rgb
    reduce:=function(r)
        return PolynomialReducedRemainder(r,rgb, MonomialGrevlexOrdering());
    end;

    #leading term
    lt:=function(r)
        return LeadingMonomialOfPolynomial(r,MonomialGrevlexOrdering());
    end;

    #tail
    tl:=function(r)
        return lt(r)-r;
    end;

    mu1:=reduce(monomial(fact));
    #A:=Set([mu1]);
    A:=Union(Set([mu1]),Set(facts,monomial));

    Info(InfoNumSgps,2,"Computing minimal elements of the ideal.");
    while true do
        B:=[];
        for i in A do
            for rel in rgb do
                new:=Lcm(i,tl(rel))/tl(rel)*lt(rel);
                if First(A, a->nonnegative(exp(new)-exp(a)))=fail then
                    AddSet(B,new);
                    Info(InfoNumSgps,2,"New possible minimal element: ",exp(new));
                fi;
            od;
        od;
        if IsSubset(A,B) then
            A:=Filtered(A, i->First(Difference(A,[i]), j-> nonnegative(exp(i)-exp(j)))=fail);
            return Maximum(Set(Set(A,exp),Sum));
        fi;
        A:=Union(A,B);
    od;
end);

######################################################################
# Computes the omega primality of the affine semigroup a
######################################################################
InstallGlobalFunction(OmegaPrimalityOfAffineSemigroup,
        function(a)
    local ls;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;

    ls:=GeneratorsOfAffineSemigroup(a);
    return Maximum(Set(ls, v-> OmegaPrimalityOfElementInAffineSemigroup(v,a)));
end);

#####################################################################
# Computes the elasticity of the affine semigroup a
#####################################################################
InstallGlobalFunction(ElasticityOfAffineSemigroup,
         function(a)

    local circuits, gens, positive, negative, cir, el;

    #computes the circuits as explained in Eisenbud-Sturmfels Lemma 8.8
    circuits:=function(a)
        local cols,rows, e, comb, c, i, circ,mat, matt, sum;
        rows:=Length(a);
        cols:=Length(a[1]);
        e:=IdentityMat(cols);
        comb:=Combinations([1..cols],rows+1);
        #Print("Combinations ",comb,"\n");
        circ:=[];
        for c in comb do
            sum:=0;
            for i in [1..rows+1] do
                mat:=TransposedMat(a);
                matt:=mat{Difference(c,[c[i]])};
                #Print("c ",c," da ",matt,"\n");
                sum:=sum+(-1)^(i+1)*DeterminantIntMat(matt)*e[c[i]];
            od;
            if ForAny(sum, x->x<>0) then
                Add(circ,sum/Gcd(sum));
            fi;

        od;
        return circ;
    end;

    # computes x^+
    positive:=function(x)
        local p,i;

        p:=[];
        for i in [1..Length(x)] do
            p[i]:=Maximum(x[i],0);
        od;

        return p;
    end;

    # computes x^-
    negative:=function(x)
        local p,i;

        p:=[];
        for i in [1..Length(x)] do
            p[i]:=-Minimum(x[i],0);
        od;

        return p;
    end;


    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;
    gens:=GeneratorsOfAffineSemigroup(a);
    cir:=circuits(TransposedMat(gens));
    cir:=Union(cir,-cir);
    return Maximum(Set(cir, c->Sum(positive(c))/Sum(negative(c))));
end);

###################################################################
#lawrence Lifting
###################################################################
InstallGlobalFunction(LawrenceLiftingOfAffineSemigroup,function(a)
	local dim,ed, msg, id, lft, zero, zeroes, aid, zeroid;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;

    msg:=GeneratorsOfAffineSemigroup(a);
    ed:=Length(msg);
    dim:=Length(msg[1]);
    id:=IdentityMat(ed);
    zero:=List([1..ed],_->0);
    zeroes:=List([1..dim],_->zero);

    msg:=TransposedMat(msg);
    aid:=TransposedMat(Concatenation(msg,id));
    zeroid:=TransposedMat(Concatenation(zeroes,id));



    lft:=(Concatenation(aid,zeroid));
    return AffineSemigroup(lft);
end);

#####################################################
# primitiveElements with Lawrence lifting
#####################################################
InstallMethod(PrimitiveElementsOfAffineSemigroup,
        "Computes the set of primitive elements of an affine semigroup",
        [IsAffineSemigroup],1,
        function(a)
	local msg, mgs, ed, dim, prlft, lft,zero, zeroes, id, aid, zeroid;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;

    Info(InfoNumSgps,2,"Using Lawrence lifting for computing primitive elements.");
    mgs:=GeneratorsOfAffineSemigroup(a);
    ed:=Length(mgs);
    dim:=Length(mgs[1]);
    #lft:=LawrenceLiftingOfAffineSemigroup(a);
    #prlft:=MinimalPresentationOfAffineSemigroup(lft);
    id:=IdentityMat(ed);
    zero:=List([1..ed],_->0);
    zeroes:=List([1..dim],_->zero);

    msg:=TransposedMat(mgs);
    aid:=TransposedMat(Concatenation(msg,id));
    zeroid:=TransposedMat(Concatenation(zeroes,id));

    lft:=(Concatenation(aid,zeroid));
    prlft:=GeneratorsOfKernelCongruence(lft);
    Info(InfoNumSgps,2,"The kernel congruence is ", prlft);

    return Union(Set(prlft, p->(p[1]{[ed+1..ed+ed]})*mgs),mgs);
end);

#####################################################################
# Computes the tame degree of the affine semigroup a
#####################################################################
InstallMethod(TameDegreeOfAffineSemigroup,
        "Computes the tame degree of an affine semigroup",
        [IsAffineSemigroup],1,
        function(a)
  local prim, tams, p, max, ls;

  if not(IsAffineSemigroup(a)) then
    Error("The argument must be an affine semigroup");
  fi;

  ls:=GeneratorsOfAffineSemigroup(a);

  Info(InfoNumSgps,2,"Computing primitive elements of ", ls);
  prim:=PrimitiveElementsOfAffineSemigroup(a);
  Info(InfoNumSgps,2,"Primitive elements of ", ls, ": ",prim);
  max:=0;
  for p in prim do
    Info(InfoNumSgps,2,"Computing the tame degree of ",p);
    tams:=TameDegreeOfSetOfFactorizations(
                  FactorizationsVectorWRTList(p,ls));
    Info(InfoNumSgps,2,"The tame degree of ",p, " is ",tams);
    if tams>max then
      max:=tams;
    fi;
  od;
  return max;
end);



###############################################################################
##
###############################################################################
###############################################################################
##
#F RandomAffineSemigroup(n,d[,m])
# Returns an affine semigroup generated by a n'*d' matrix where d' (the dimension) is randomly choosen from [1..d] and n' (the number of generators) is randomly choosen from [1..n]. The entries of the matrix are randomly choosen from [0..m] (when the third argument is not present, m is taken as n'*d')
###########################################################################
InstallGlobalFunction(RandomAffineSemigroup,function(arg)
  local  rn, rd, max;

  rn := Random([1..arg[1]]);
  rd := Random([1..arg[2]]);
  if Length(arg) = 3 then
    max := arg[3];
  else
    max := rn*rd;
  fi;

  return AffineSemigroup("generators",RandomMat(rn,rd,[0..max]));

end);

###############################################################################
##
#F RandomFullAffineSemigroup(n,d[,m, string])
# Returns a full affine semigroup either given by equations or inequalities (when no string is given, one is choosen at random). The matrix is an n'*d' matrix where d' (the dimension) is randomly choosen from [1..d] and n' is randomly choosen from [1..n]. When it is given by equations, the moduli are choosen at random. The entries of the matrix (and moduli) are randomly choosen from [0..m] (when the third integer is not present, m is taken as n'*d')
###########################################################################
InstallGlobalFunction(RandomFullAffineSemigroup,function(arg)
  local  type, nums, rn, rd, max;

  if First(arg, IsString) <> fail then
    type := First(arg, IsString);
  else
    type := RandomList(["equations","inequalities"]);
  fi;

  nums := Filtered(arg,IsInt);

  rn := Random([1..nums[1]]);
  rd := Random([1..nums[2]]);
  if Length(nums) = 3 then
    max := nums[3];
  else
    max := rn*rd;
  fi;


  if type = "equations" then
    return AffineSemigroup(type,[RandomMat(rn,rd,[0..max]),RandomMat(1,rd,[0..max])[1]]);
  fi;
  return AffineSemigroup(type,RandomMat(rn,rd,[0..max]));
end);

##########################################################################
##
#F NumSgpsUseNormaliz
#  Loads the package NormalizInterface and reads affine-extra-ni
##########################################################################
InstallGlobalFunction(NumSgpsUseNormaliz, function()
    if LoadPackage("NormalizInterface")=true then
        ReadPackage("numericalsgps/gap/affine-extra-ni.gi");
        NumSgpsCanUseNI:=true;
        return true;
    else
        return fail;
    fi;

end);


##########################################################################
##
#F NumSgpsUseSingular
#  Loads the package singular and reads affine-extra-s
##########################################################################
InstallGlobalFunction(NumSgpsUseSingular, function()
    if IsPackageMarkedForLoading("SingularInterface","0.0") then
        Print("SingularInterface is already loaded and it is incompatible with Singular.\n");
        return fail;
    fi;

    if LoadPackage("singular")=true then
        ReadPackage("numericalsgps/gap/affine-extra-s.gi");
        NumSgpsCanUseSingular:=true;
        return true;
    else
        return fail;
    fi;

end);

##########################################################################
##
#F NumSgpsUseSingularInterface
#  Loads the package SingularInterface and reads affine-extra-si
##########################################################################
InstallGlobalFunction(NumSgpsUseSingularInterface, function()
    if IsPackageMarkedForLoading("Singular","0.0") then
        Print("Singular is already loaded and it is incompatible with SingularInterface.\n");
        return fail;
    fi;

    if LoadPackage("SingularInterface")=true then
        ReadPackage("numericalsgps/gap/affine-extra-si.gi");
        NumSgpsCanUseSI:=true;
        return true;
    else
        return fail;
    fi;

end);

##########################################################################
##
#F NumSgpsUse4ti2
#  Loads the package 4ti2Interface and reads affine-extra-4ti2
##########################################################################
InstallGlobalFunction(NumSgpsUse4ti2, function()
    if LoadPackage("4ti2Interface")=true then
        ReadPackage("numericalsgps/gap/affine-extra-4ti2.gi");
        NumSgpsCanUse4ti2:=true;
        return true;
    else
        return fail;
    fi;

end);

##########################################################################
##
#F NumSgpsUse4ti2gap
#  Loads the package 4ti2gap and reads affine-extra-4ti2gap
##########################################################################
InstallGlobalFunction(NumSgpsUse4ti2gap, function()
    if LoadPackage("4ti2gap")=true then
        ReadPackage("numericalsgps/gap/affine-extra-4ti2gap.gi");
        NumSgpsCanUse4ti2gap:=true;
        return true;
    else
        return fail;
    fi;

end);

##########################################################################
##
#F NumSgpsUseGradedModules
#  Loads the package GradedModules and reads affine-extra-gm
##########################################################################
InstallGlobalFunction(NumSgpsUseGradedModules, function()
    if LoadPackage("GradedModules")=true then
        ReadPackage("numericalsgps/gap/affine-extra-gm.gi");
        NumSgpsCanUseGradedModules:=true;
        return true;
    else
        return fail;
    fi;

end);
