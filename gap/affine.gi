LoadPackage("singular");
#SetInfoLevel(InfoNumSgps,2);

sing_exec:=`sing_exec;
sing_exec_options:=`sing_exec_options;
SingularTempDirectory:=`SingularTempDirectory;
SingularNr:=AtomicRecord(SingularNr);
SingularNamesThisRing:=AtomicRecord(SingularNamesThisRing);
SingularNames:=AtomicRecord(SingularNames);
SingularDataTypes:=`SingularDataTypes;
SingularDataTypeTestOrder:=`SingularDataTypeTestOrder;
SingularLimitations:=`SingularLimitations;
Sing_Proc:=`Sing_Proc;

LoadPackage("4ti2gap");

LoadPackage("NormalizInterface");


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

#############################################################################
##
#F  HilbertBasisOfSystemOfHomogeneousEquations
#
# Computes the Hilbert basis of the system A X=0 mod md, where the rows
# of A are the elements of ls.
# md can be empty of have some modulus, if the length of md is smaller than 
# the lengths of the elements of ls, then the rest of equations are considered
# to be homogeneous linear Diophantine equations
##########################################################################

# NormalizInterface implementation

InstallGlobalFunction(HilbertBasisOfSystemOfHomogeneousEquations_Normaliz, function(ls,md)
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

# now with 4ti2gap

InstallGlobalFunction(HilbertBasisOfSystemOfHomogeneousEquations_4ti2, function(ls,md)
    local  homogeneous, withCongruences;
    
    homogeneous:= function(l)
        local  problem, matrix,mat,sign;

        Info(InfoNumSgps,2,"Using 4ti2gap for Hilbert.");

        if not(IsRectangularTable(l)) then 
            Error("The argument must be a matrix.");
        fi;
        if not(IsInt(l[1][1])) then
            Error("The matrix must be of integers.");
        fi;

	mat:=l;
        sign:=[List(l[1],_->1)];
        problem:=["mat",mat, "sign", sign];
        
        matrix := HilbertBasis4ti2(problem);
        return matrix;
        
    end; 

  withCongruences:=function(ls,md)
      local l,n,m,diag,dim,d, hil, zero, leq;
      
      leq:= function(v1,v2)
          local v;
          v:=v2-v1;
          return (First(v,n->n<0)=fail);
      end;

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

      hil:=Difference(List(homogeneous(l), x->x{[1..dim]}),[zero]);
      return hil;

      return Filtered(hil, y->Filtered(hil,x->leq(x,y))=[y]);
  end;
  ## end of local functions ...

  #ls := arg[1][1];
  #md := arg[1][2];
  if md = [] then
      return homogeneous(ls);
  else
      return withCongruences(ls,md);

  fi;
    
end);


##############################################################################################
#
# ls is a matrix of integers. It computes the set minimal nonzero nonnegative integer solutions
# of ls*x>=0
#

# 4ti2 implementation

InstallGlobalFunction(HilbertBasisOfSystemOfHomogeneousInequalities_4ti2, function(l)
    local  problem, matrix,mat,sign,rel;

    Info(InfoNumSgps,2,"Using 4ti2gap for Hilbert.");

    if not(IsRectangularTable(l)) then 
        Error("The argument must be a matrix.");
    fi;
    if not(IsInt(l[1][1])) then
        Error("The matrix must be of integers.");
    fi;
    
    mat:=l;
    sign:=[List(l[1],_->1)];
    rel:=[List(l[1],_->">")];
    problem:=["mat",mat,"rel",rel,"sign",sign];
    matrix:=HilbertBasis4ti2(problem);
    return matrix;
    
end); 


# Normaliz implementation

InstallGlobalFunction(HilbertBasisOfSystemOfHomogeneousInequalities_Normaliz, function(ls)
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
    
    cone:=NmzCone(["inequalities",ls]);
    NmzCompute(cone,"DualMode"); 	
    
    return NmzHilbertBasis(cone);
end);


########################################################################
# Computes the set of factorizations of v in terms of the elements of ls 
# That is, a Hilbert basis for ls*X=v
# If ls contains vectors that generate a nonreduced monoid, then it 
# may enter in an infinite loop
########################################################################
# InstallGlobalFunction(FactorizationsVectorWRTList, function(v,ls)
#     local mat, len, which, tasks, result, i;
    
#     mat:=TransposedMat(Concatenation(ls,[-v]));
        
#     if not(IsListOfIntegersNS(v)) then
#         Error("The first argument must be a list of integers.");
#     fi;
    
#     if not(ForAll(ls,IsListOfIntegersNS)) then 
#         Error("The second argument must be a list of lists of integers.");
#     fi;
    
#     if not(IsRectangularTable(mat)) then
#         Error("The list in the second argument must have the same length as the lists in the first argument.");
#     fi;
    
#     len:=Length(ls);
#     if ls=[] then
#         return [];
#     fi;
#     if ForAll(v,x->x=0) then 
#         return [List([1..len],_->0)];
#     fi;
#     if ForAny(v,x->x<0) then 
#         return [];
#     fi;
    
#     tasks:=[DelayTask(FactorizationsVectorWRTList_gap,v,ls),
#             DelayTask(FactorizationsVectorWRTList_4ti2,v,ls),
#             DelayTask(FactorizationsVectorWRTList_Normaliz,v,ls)];
    
#     which:=WaitAnyTask(tasks);
#     if which=1 then
#         Info(InfoNumSgps,2,"gap used to factor", v);
#     fi;
    
#     if which=2 then
#         Info(InfoNumSgps,2,"4ti2 used to factor", v);
#     fi;
    
#     if which=3 then
#         Info(InfoNumSgps,2,"Normaliz used to factor", v);
#     fi;
    
#     result:= TaskResult(tasks[which]);
      
#     for i in [1..3] do 
#         if i<> which then 
#             CancelTask(tasks[i]);
#         fi;
#     od;
     
    
#     return result;
    
# end);

    
# basic implementation    
InstallGlobalFunction(FactorizationsVectorWRTList_gap, function(v,ls)
    local len, e1, opt1, opt2, i, mat, dim;
    
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
    
    OnTaskCancellation(ReturnFail);

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
        opt1:=List(FactorizationsVectorWRTList_gap(v-ls[1],ls), x->x+e1);
    fi;
    opt2:=List(FactorizationsVectorWRTList_gap(v,ls{[2..len]}), 
               x->Concatenation([0],x));
    return Concatenation(opt1,opt2);
end);

# Normaliz implementation

InstallGlobalFunction(FactorizationsVectorWRTList_Normaliz, function(v,ls)
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

#4ti2 implementation

InstallGlobalFunction(FactorizationsVectorWRTList_4ti2,
        function(v,l)
    local  matrix,mat,rhs,sign,problem, n;

    Info(InfoNumSgps,2,"Using 4ti2gap for factorizations.");
    
    if not(IsListOfIntegersNS(v)) then
        Error("The first argument must be a list of integers.");
    fi;
    
    if not(IsInt(l[1][1])) then
        Error("The matrix must be of integers.");
    fi;

    mat:=TransposedMat(Concatenation(l,[-v]));
    if not(IsRectangularTable(mat)) then
        Error("The list in the second argument must have the same length as all the lists in the first argument.");
    fi;
 
    sign:=[List(l,_->1)];
    rhs:=[v];
    problem:=["mat",TransposedMat(l),"sign",sign,"rhs",rhs];
    matrix := ZSolve4ti2(problem);
    return matrix.zinhom;
    
end); 


############################################################
# computes a set of generators of the kernel congruence 
# of the monoid morphism associated to the matrix m with 
# nonnegative integer coefficients
############################################################

# 4ti2 implementation

InstallGlobalFunction(GeneratorsOfKernelCongruence_4ti2, function(m)
    local positivenegative, gr;
    
    positivenegative:=function(p)
        local d1, d2;
        d1:=List(p, i->Maximum(i,0));
        d2:=List(p, i->-Minimum(0,i));
        return [d1,d2];
    end;
    
    if not(ForAll(m, l->ForAll(l, x->(x=0) or IsPosInt(x)))) then
        Error("The argument must be a matrix of nonnegative integers.");
    fi;
    
    OnTaskCancellation(ReturnFail);

    gr:=GroebnerBasis4ti2(TransposedMat(m));
    Info(InfoNumSgps,2,"4ti output:",gr);
    
    return List(gr, x->positivenegative(x));
end);

#Singular implementation

InstallGlobalFunction(GeneratorsOfKernelCongruence_Singular, function(m)
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens;

    
    Info(InfoNumSgps,2,"Using singular to compute minimal presentations.");

    ##computes the s degree of a monomial in the semigroup ideal 
    sdegree:=function(m) 
        local exp;
        exp:=List([1..ed], i->DegreeIndeterminate(m,i));
        return exp*msg;
    end;
    
    bintopair:=function(pp)
        local m1,m2, d1, d2, p;
        p:=pp/LeadingCoefficient(pp);
        m1:=LeadingMonomialOfPolynomial(p, MonomialGrlexOrdering());
        m2:=m1-p;
        d1:=List([1..ed], i->DegreeIndeterminate(m1,i));; 
        d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
        return [d1,d2];
    end;
    
    if not(ForAll(m, l->ForAll(l, x->(x=0) or IsPosInt(x)))) then
        Error("The argument must be a matrix of nonnegative integers.");
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
    SetTermOrdering(R,"dp");
    SingularSetBaseRing(R);
    p:=List([1..ed], i->X(Rationals,i)-Product(List([1..dim], j->X(Rationals,j+ed)^msg[i][j])));
    id:=Ideal(R,p);
    ie:=SingularInterface("eliminate",[id,Product(List([1..dim], j->X(Rationals,j+ed)))],"ideal");
    gens:=GeneratorsOfIdeal(ie);
    vars:=vars{[1..ed]};
    R:=PolynomialRing(Rationals,vars);
    SetTermOrdering(R, ["wp",List(msg, m->Sum(m))] );
    SingularSetBaseRing(R);
    ie:=Ideal(R,gens);
    mingen:=GeneratorsOfIdeal(SingularInterface("minbase",[ie],"ideal"));
    return Set([1..Length(mingen)],i->bintopair(mingen[i]));
end);


############################################################
# computes a minimal presentation of a 
############################################################
InstallGlobalFunction(MinimalPresentationOfAffineSemigroup,function( a )
	
    local msg, ed, dim, rel, pres, rclass, exps, c, candidates;
    
    
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;
    
    msg:=GeneratorsOfAffineSemigroup(a); #for now we do not check minimality of the generators
    ed:=Length(msg);
    if ed=0 then 
        return [];
    fi;
    dim:=Length(msg[1]);
    rel:=GeneratorsOfKernelCongruence(msg);
    candidates:=Set(rel,q->q[1]*msg);
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
InstallGlobalFunction(BettiElementsOfAffineSemigroup, function(a)
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
InstallGlobalFunction(CatenaryDegreeOfAffineSemigroup, function(a)
    local betti, cats, tasks, ls, lfac;
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;
    
    ls:=GeneratorsOfAffineSemigroup(a);

    Info(InfoNumSgps,2,"Computing the Betti elements of the affine semigroup.");
    betti:=BettiElementsOfAffineSemigroup(a);
    Info(InfoNumSgps,2,"The Betti elements are ",betti);
    
    tasks:=List(betti,g->RunTask(x->(FactorizationsVectorWRTList(x,ls)),g));
    lfac:=List(tasks,TaskResult);
    tasks:=List(lfac,l->RunTask(x->(CatenaryDegreeOfSetOfFactorizations(x)),l));
    cats:=Set(tasks,TaskResult);
    
    # Sequential version
    # lfac:=List(betti, b->FactorizationsVectorWRTList(b,ls));
    # cats:=Set(lfac, l->CatenaryDegreeOfSetOfFactorizations(l));
    return Maximum(cats);
    
   
end);


InstallGlobalFunction(CatenaryDegreeOfAffineSemigroup_seq, function(a)
    local betti, cats, tasks, ls, lfac;
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;
    
    ls:=GeneratorsOfAffineSemigroup(a);

    Info(InfoNumSgps,2,"Computing the Betti elements of the affine semigroup.");
    betti:=BettiElementsOfAffineSemigroup(a);
    Info(InfoNumSgps,2,"The Betti elements are ",betti);
    
    
    #Sequential version
    lfac:=List(betti, b->FactorizationsVectorWRTList(b,ls));
    cats:=Set(lfac, l->CatenaryDegreeOfSetOfFactorizations(l));
    return Maximum(cats);
    
   
end);

###############################################################################
##
#F OmegaPrimalityOfElementInAffineSemigroup
#
# Computes the omega-primality of v in the monoid a
###########################################################################

#Normaliz implementation

InstallGlobalFunction(OmegaPrimalityOfElementInAffineSemigroup,
        function(v,a)
    
    local mat, cone, n, hom, par, tot, le, ls, one;

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

    if (HasEquationsAS(a)) then 
        Info(InfoNumSgps,2,"Using that the semigroup is full.");
    
        one:=[List([1..n],_->1)];
        mat:=TransposedMat(Concatenation(ls,[-v]));
        cone:=NmzCone(["inhom_inequalities",mat,"signs",one]);
        NmzCompute(cone,"DualMode"); 	
        par:=Set(NmzModuleGenerators(cone), f->f{[1..n]});

        Info(InfoNumSgps,2,"Minimals of v+ls=",par);
        return Maximum(Set(par, Sum)); 
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

# 4ti2 implementation

InstallGlobalFunction(OmegaPrimalityOfElementInAffineSemigroup_4ti2, function(v,a)
    local  ls, n, mat,extfact,par,tot,le;
    
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
    
    extfact:=FactorizationsVectorWRTList(v,Concatenation(ls,-ls));
    
    par:=Set(extfact, f->f{[1..n]});
    tot:=Filtered(par, f-> Filtered(par, g-> le(g,f))=[f]);
    Info(InfoNumSgps,2,"Minimals of v+ls =",tot);
    if tot=[] then 
        return 0;      
    fi;
    
    return Maximum(Set(tot, Sum));

    
end);

######################################################################
# Computes the omega primality of the affine semigroup a
######################################################################
InstallGlobalFunction(OmegaPrimalityOfAffineSemigroup,
        function(a)
    local ls, omegas, tasks;
    
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;
    
    ls:=GeneratorsOfAffineSemigroup(a);
    tasks:=List(ls, g->RunTask(x->OmegaPrimalityOfElementInAffineSemigroup(x,a),g));
    omegas:=Set(tasks,TaskResult);
    return Maximum(omegas);
    
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

# (basic setting)

InstallGlobalFunction(PrimitiveElementsOfAffineSemigroup_gap,
        # "Computes the set of primitive elements of an affine semigroup",
        # [IsAffineSemigroup],1,
        function(a)
    local msg, ed, dim, prlft, lft;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;
    
    Info(InfoNumSgps,2,"Using Lawrence lifting for computing primitive elements.");
    msg:=GeneratorsOfAffineSemigroup(a);
    ed:=Length(msg);
    dim:=Length(msg[1]);
    lft:=LawrenceLiftingOfAffineSemigroup(a);
    prlft:=MinimalPresentationOfAffineSemigroup(lft);
    return Set(prlft, p->(p[1]{[ed+1..ed+ed]})*msg);
end);

# same with 4ti2gap, but now with graver basis

InstallGlobalFunction(PrimitiveElementsOfAffineSemigroup_4ti2,
        # "Computes the set of primitive elements of an affine semigroup",
        # [IsAffineSemigroup],4, 
        function(a)
    local  matrix, facs, mat, trunc, ls;
    
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;
    
    OnTaskCancellationReturn(true);
    
      
    ls:=GeneratorsOfAffineSemigroup(a);
    
    Info(InfoNumSgps,2,"Using 4ti2gap for Graver.");
    
    mat:=TransposedMat(ls);
    matrix := GraverBasis4ti2(["mat",mat]);

    trunc:=function(ls)
        return List(ls, y->Maximum(y,0));
    end;
    
    matrix:=Set(matrix,trunc);
    return Set(matrix, x->x*ls);
end);

#now with Normaliz using (A|-A)

InstallGlobalFunction(PrimitiveElementsOfAffineSemigroup_Normaliz,
        # "Computes the primitive elements of an affine semigroup",
        # [IsAffineSemigroup],5,
        function(a)
    local mat, n, cone, facs, ls;
    
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;
    
    OnTaskCancellationReturn(a);   
      
    ls:=GeneratorsOfAffineSemigroup(a);
    
    Info(InfoNumSgps,2,"Using NormalizInterface for primitive elements.");
    
    n:=Length(ls);
    mat:=TransposedMat(Concatenation(ls,-ls));
    cone:=NmzCone(["equations",mat]);
    NmzCompute(cone,"DualMode"); 	
    facs:=Set(NmzHilbertBasis(cone), f->f{[1..n]});
    
    return Set(facs, f->f*ls);	
end);

#now with Lawrence lifting and singular
InstallGlobalFunction(PrimitiveElementsOfAffineSemigroup_Singular, function(a)
    
    local msg, ed, dim, prlft, lft;
    
    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;
    
    Info(InfoNumSgps,2,"Using Lawrence lifting for computing primitive elements.");
    msg:=GeneratorsOfAffineSemigroup(a);
    ed:=Length(msg);
    dim:=Length(msg[1]);
    lft:=LawrenceLiftingOfAffineSemigroup(a);
    prlft:=MinimalPresentationOfAffineSemigroup(lft);
    return Set(prlft, p->(p[1]{[ed+1..ed+ed]})*msg);
end);


#####################################################################
# Computes the tame degree of the affine semigroup a
#####################################################################
InstallGlobalFunction(TameDegreeOfAffineSemigroup,function(a)
  local prim, tams, ls, tasks;

  if not(IsAffineSemigroup(a)) then
    Error("The argument must be an affine semigroup");
  fi;

  ls:=GeneratorsOfAffineSemigroup(a);

  Info(InfoNumSgps,2,"Computing primitive elements of ", ls);	
  prim:=PrimitiveElementsOfAffineSemigroup(a);
  Info(InfoNumSgps,2,"Primitive elements of ", ls, ": ",prim);
  
  tasks:=List(prim, g->RunTask(x->TameDegreeOfSetOfFactorizations(
                  FactorizationsVectorWRTList(x,ls)),g));
  tams:=Set(tasks,TaskResult);
  return Maximum(tams);
  
end);

  
  
###############################################################################
## ramdom stuff
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



