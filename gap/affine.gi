if not TestPackageAvailability("NormalizInterface") = fail then
    LoadPackage("NormalizInterface");
fi;
##
if not TestPackageAvailability("4ti2Interface") = fail then
    LoadPackage("4ti2");
fi;
##
if not TestPackageAvailability("SingularInterface") = fail then
    LoadPackage("SingularInterface");  
fi;
##
#if not TestPackageAvailability("libsing") = fail then
#    LoadPackage("libsing");
#fi;
##

##########################################################################
# Computes the Hilbert basis of the system A X=0 mod md, where the rows
# of A are the elements of ls.
# md can be empty of have some modulus, if the length of md is smaller than 
# the lengths of the elements of ls, then the rest of equations are considered
# to be homogeneous linear Diophantine equations
# REQUERIMENTS: NormalizInterface
##########################################################################
InstallGlobalFunction(HilbertBasisOfSystemOfHomogeneousEquations,
        function(ls,md)
	local matcong, cone, ncong, ncoord, nequ, matfree;

	if not(IsHomogeneousList(ls)) or not(IsHomogeneousList(md)) then
		Error("The arguments must be homogeneous lists.");
	fi;

	if not(ForAll(ls,IsListOfIntegersNS)) then 
		Error("The first argument must be a list of lists of integers.");
	fi;
        
        if not(IsListOfIntegersNS(md)) then 
            Error("The second argument must be a lists of integers.");
	fi;
        
        if not(ForAll(md,x->x>0)) then
            Error("The second argument must be a list of positive integers");
        fi;
        
	if not(Length(Set(ls, Length))=1) then
		Error("The first argument must be a list of lists all with the same length.");
	fi;

	ncong:=Length(md);
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

##########################################################################
# Computes the Hilbert basis of the system ls*X>=0 over the nonnegative 
# integers
# REQUERIMENTS: NormalizInterface
##########################################################################
InstallGlobalFunction(HilbertBasisOfSystemOfHomogeneousInequalities,
        function(ls)
	local cone,  ncoord;

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
# deprecates the infinite part of the solutions, or in other words, it
# returns only the minimal solutions of the above system of equations
# REQUERIMENTS: NormalizInterface
########################################################################
InstallGlobalFunction(FactorizationsVectorWRTList,
        function(v,ls)
	local mat, cone, n, facs;
	
	n:=Length(ls);
	mat:=TransposedMat(Concatenation(ls,[-v]));

	if not(IsHomogeneousList(mat)) then
		Error("The arguments must be homogeneous lists.");
	fi;

	if not(IsListOfIntegersNS(v)) then
		Error("The first argument must be a list of integers.");
	fi;

	if not(ForAll(ls,IsListOfIntegersNS)) then 
		Error("The second argument must be a list of lists of integers.");
	fi;

	if not(Length(Set(mat, Length))=1) then
		Error("All lists must in the second argument have the same length as the first argument.");
	fi;

	
	cone:=NmzCone(["inhom_equations",mat]);
	NmzCompute(cone,"DualMode"); 	
	facs:=List(NmzConeProperty(cone,"ModuleGenerators"), f->f{[1..n]});
	return facs;
end);

#####################################################################
# Computes the omega-primality of v in the affine semigroup a
# REQUERIMENTS: NormalizInterface
#####################################################################

InstallGlobalFunction(OmegaPrimalityOfElementInAffineSemigroup,
        function(v,a)
	local mat, cone, n, hom, par, tot, le, ls;

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
	
	ls:=GeneratorsAS(a);
	n:=Length(ls);
	mat:=TransposedMat(Concatenation(ls,-ls,[-v]));

	if not(IsHomogeneousList(mat)) then
		Error("The first argument has not the dimension of the second.");
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

######################################################################
# Computes the omega primality of the affine semigroup a
# REQUERIMENTS: NormalizInterface
######################################################################

InstallGlobalFunction(OmegaPrimalityOfAffineSemigroup,
        function(a)
	local ls;


	if not(IsAffineSemigroup(a)) then
		Error("The argument must be an affine semigroup");
	fi;

	ls:=GeneratorsAS(a);
	return Maximum(Set(ls, v-> OmegaPrimalityOfElementInAffineSemigroup(v,a)));
end);

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

#####################################################################
# An implementation of PrimitiveElementsOfAffineSemigroup using 
# Normaliz
# REQUERIMENTS: NormalizInterface
#####################################################################

#labelled Normaliz, since this one is slower than with 4ti2
InstallGlobalFunction(PrimitiveElementsOfAffineSemigroup,
        function(a)
	local mat, n, cone, facs, ls;

	if not(IsAffineSemigroup(a)) then
		Error("The argument must be an affine semigroup");
	fi;

	ls:=GeneratorsAS(a);

	n:=Length(ls);
	mat:=TransposedMat(Concatenation(ls,-ls));
	cone:=NmzCone(["equations",mat]);
	NmzCompute(cone,"DualMode"); 	
	facs:=Set(NmzHilbertBasis(cone), f->f{[1..n]});
	
	return Set(facs, f->f*ls);	
end);

#####################################################################
# Computes the tame degree of the affine semigroup a
# REQUERIMENTS: NormalizInterface
#####################################################################

InstallGlobalFunction(TameDegreeOfAffineSemigroup,
        function(a)
	local prim, tams, p, max, ls;

	if not(IsAffineSemigroup(a)) then
		Error("The argument must be an affine semigroup");
	fi;

	ls:=GeneratorsAS(a);

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

#####################################################################
# Computes the elasticity of the affine semigroup a
# REQUERIMENTS: NormalizInterface
#####################################################################
InstallGlobalFunction(ElasticityOfAffineSemigroup,
        function(a)
	local mat, n, cone, facs, ls;


	if not(IsAffineSemigroup(a)) then
		Error("The argument must be an affine semigroup");
	fi;

	ls:=GeneratorsAS(a);

	n:=Length(ls);
	mat:=TransposedMat(Concatenation(ls,-ls));
	cone:=NmzCone(["equations",mat]);
	NmzCompute(cone,"DualMode"); 	
	facs:=Set(NmzHilbertBasis(cone), f->[f{[1..n]},f{[n+1..2*n]}]);
	
	return Maximum(Set(facs, y->Sum(y[1])/Sum(y[2])));
end);

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


######################################################################
# Computes the set of Betti elements of an affine semigroup, that
# is, the set of elements whose factorizations are involved in a 
# minimal presentation of a
# REQUERIMENTS: SingularInterface
######################################################################
InstallGlobalFunction(BettiElementsOfAffineSemigroup,
        function( a )
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		R,id, ie, vars, mingen, exps, bintopair, dim, zero, ls;
    
    

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup");
    fi;
    
    ls:=GeneratorsAS(a);
    
	##computes the s degree of a pol in the semigroup ideal 
    sdegree:=function(r) 
        local exp;
        exp:=_SI_Plistintvec(SI_leadexp(SI_leadmonom(r)));
        return exp*msg;
    end;

    #translates binomial to pair of exponents 
    bintopair:=function(p)
        local m1,m2, d1, d2;
        m1:=p[1];#SI_leadmonom(p);
        m2:=p[2];#m1-p;
        d1:=_SI_Plistintvec(SI_leadexp(m1)); 
        d2:=_SI_Plistintvec(SI_leadexp(m2));
        return [d1{[1..ed]},d2{[1..ed]}];
    end;

    msg:=ls; #for now we do not check minimality of the generators
    ed:=Length(msg);
    if ed=0 then 
        return [];
    fi;
    zero:=List([1..ed],_->0);
    dim:=Length(ls[1]);
    vars:=List([1..ed+dim],i->Concatenation("x",String(i)));
    R:=SI_ring(0,vars,[["wp",List(msg, m->Sum(m))],["dp",dim]]);
    p:=List([1..ed], i->SI_var(R,i)-
            SI_monomial(R,SI_intvec(Concatenation(zero,msg[i]))));
    id:=SI_ideal(p);
    Info(InfoNumSgps,2,"Eliminating the variables ", Product(List([1..dim],i-> SI_var(R,ed+i))),
         " from the ideal ", id);
    ie:= SI_eliminate(id, Product(List([1..dim],i-> SI_var(R,ed+i)) ));	
    Info(InfoNumSgps,2,"and we obtain ",ie);
    vars:=vars{[1..ed]};
    R:=SI_ring(0,vars,[["wp",List(msg, m->Sum(m))]]);
    p:=[];
    for i in [1..SI_ncols(ie)] do
        exps:=bintopair(ie[i]);
        Add( p, SI_monomial(R,SI_intvec(exps[1]))-
             SI_monomial(R,SI_intvec(exps[2]))); 
    od;
    id:=SI_ideal(p);
    mingen:=SI_minbase(id);
    return Set([1..SI_ncols(mingen)],i->sdegree(mingen[i]));
end);

######################################################################
# Computes a minimal presentation of the affine semigroup a 
# REQUERIMENTS: SingularInterface
######################################################################
InstallGlobalFunction(MinimalPresentationOfAffineSemigroup,
  function( a )
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		R,id, ie, vars, mingen, exps, bintopair, dim, zero, ls;


	if not(IsAffineSemigroup(a)) then
		Error("The argument must be an affine semigroup");
	fi;

	ls:=GeneratorsAS(a);


	##computes the s degree of a pol in the semigroup ideal 
        sdegree:=function(r) 
            local exp;
            exp:=_SI_Plistintvec(SI_leadexp(SI_leadmonom(r)));
            return exp*msg;
        end;

	#tanslates binomial to pair of exponents
	bintopair:=function(p)
            local m1,m2, d1, d2;
            m1:=p[1];#SI_leadmonom(p);
            m2:=p[2];#m1-p;
            d1:=_SI_Plistintvec(SI_leadexp(m1)); 
            d2:=_SI_Plistintvec(SI_leadexp(m2));
            return [d1{[1..ed]},d2{[1..ed]}];
	end;

        msg:=ls; #for now we do not check minimality of the generators
        ed:=Length(msg);
	if ed=0 then 
            return [];
	fi;
	zero:=List([1..ed],_->0);
	dim:=Length(ls[1]);
	vars:=List([1..ed+dim],i->Concatenation("x",String(i)));
	R:=SI_ring(0,vars,[["wp",List(msg, m->Sum(m))],["dp",dim]]);
	p:=List([1..ed], i->SI_var(R,i)-
		SI_monomial(R,SI_intvec(Concatenation(zero,msg[i]))));
	id:=SI_ideal(p);
	Info(InfoNumSgps,2,"Eliminating the variables ", Product(List([1..dim],i-> SI_var(R,ed+i))),
             " from the ideal ", id);
	ie:= SI_eliminate(id, Product(List([1..dim],i-> SI_var(R,ed+i)) ));	
	Info(InfoNumSgps,2,"and we obtain ",ie);
	vars:=vars{[1..ed]};
	R:=SI_ring(0,vars,[["wp",List(msg, m->Sum(m))]]);
	p:=[];
	for i in [1..SI_ncols(ie)] do
            exps:=bintopair(ie[i]);
            Add( p, SI_monomial(R,SI_intvec(exps[1]))-
                 SI_monomial(R,SI_intvec(exps[2]))); 
	od;
	id:=SI_ideal(p);
	mingen:=SI_minbase(id);
        return Set([1..SI_ncols(mingen)],i->bintopair(mingen[i]));
end);

######################################################################
# Computes the catenary degree of the affine semigroup a 
# REQUERIMENTS: SingularInterface
######################################################################
InstallGlobalFunction(CatenaryDegreeOfAffineSemigroup,
        function(a)
	local betti, b, max, c, ls;
	if not(IsAffineSemigroup(a)) then
		Error("The argument must be an affine semigroup");
	fi;

	ls:=GeneratorsAS(a);

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


