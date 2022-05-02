#############################################################################
##
#W  affine-extra-s.gi
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################
#if not TestPackageAvailability("singular") = fail then
#    LoadPackage("singular");
#fi;

# we will always use Gröbner by Singular package which is faster

#GBASIS:= SINGULARGBASIS;


############################################################
# computes a set of generators of the kernel congruence
# of the monoid morphism associated to the matrix m with
# nonnegative integer coefficients
############################################################
InstallOtherMethod(GeneratorsOfKernelCongruence,
        "Computes a set of generators of the kernel congruence of the monoid morphism associated to a matrix",
        [IsHomogeneousList],6,
        function(m)
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		Rtmp, R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens, GBASIStmp;


    Info(InfoNumSgps,2,"Using singular to compute minimal presentations.");

    ##computes the s degree of a monomial in the semigroup ideal
    sdegree:=function(m)
        local exp;
        exp:=List([1..ed], i->DegreeIndeterminate(m,i));
        return exp*msg;
    end;

    bintopair:=function(pp)
        local m1,m2, d1, d2, p;
        p:=pp/LeadingCoefficientOfPolynomial(pp,MonomialLexOrdering());
        m1:=LeadingMonomialOfPolynomial(p, MonomialLexOrdering());
        m2:=m1-p;
        d1:=List([1..ed], i->DegreeIndeterminate(m1,i));;
        d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
        return Set([d1,d2]);
    end;

    if not(IsRectangularTable(m) and ForAll(m, l->ForAll(l, x->(x=0) or IsPosInt(x)))) then
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
    Rtmp:=SingularBaseRing;
    GBASIStmp:=GBASIS;
    GBASIS:=SINGULARGBASIS;
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
    SingularSetBaseRing(Rtmp);
    GBASIS:=GBASIStmp;
    if Zero(R) in mingen then
      return [];
    fi;
    return Set([1..Length(mingen)],i->bintopair(mingen[i]));
end);

############################################################
# computes a canonical basis of the kernel congruence
# of the monoid morphism associated to the matrix m with
# nonnegative integer coefficients wrt the term ordering
# the kernel is the pairs (x,y) such that xm=ym
############################################################
InstallMethod(CanonicalBasisOfKernelCongruence,
"Computes a canonical basis for the congruence of of the monoid morphism associated to the matrix",
	[IsHomogeneousList, IsMonomialOrdering],6,
  function(m,ord)
  	local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
  	Rtmp, R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens, GBASIStmp;


  	Info(InfoNumSgps,2,"Using singular to compute kernels.");

  	##computes the s degree of a monomial in the semigroup ideal
  	sdegree:=function(m)
  		local exp;
  		exp:=List([1..ed], i->DegreeIndeterminate(m,i));
  		return exp*msg;
  	end;

  	bintopair:=function(pp)
  		local m1,m2, d1, d2, p;
  		p:=pp/LeadingCoefficientOfPolynomial(pp,ord);
  		m1:=LeadingMonomialOfPolynomial(p, ord);
  		m2:=m1-p;
  		d1:=List([1..ed], i->DegreeIndeterminate(m1,i));;
  		d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
  		return [d1,d2];
  	end;

  	if not(IsRectangularTable(m) and ForAll(m, l->ForAll(l, x->(x=0) or IsPosInt(x)))) then
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
  	Rtmp:=SingularBaseRing;
    GBASIStmp:=GBASIS;
    GBASIS:=SINGULARGBASIS;
  	SingularSetBaseRing(R);
  	p:=List([1..ed], i->X(Rationals,i)-Product(List([1..dim], j->X(Rationals,j+ed)^msg[i][j])));
  	id:=Ideal(R,p);
  	ie:=SingularInterface("eliminate",[id,Product(List([1..dim], j->X(Rationals,j+ed)))],"ideal");
  	gens:=GeneratorsOfIdeal(ie);
  	vars:=vars{[1..ed]};
  	R:=PolynomialRing(Rationals,vars);
  	SingularSetBaseRing(R);
  	SetTermOrdering(R,ord);
  	ie:=Ideal(R,gens);
  	gens:=GroebnerBasis(ie, ord);
  	SingularSetBaseRing(Rtmp);
    GBASIS:=GBASIStmp;
  	return Set([1..Length(gens)],i->bintopair(gens[i]));
  end);


  # InstallOtherMethod(PrimitiveElementsOfAffineSemigroup,
  #         "Computes the set of primitive elements of an affine semigroup",
  #         [IsAffineSemigroup],4,
  #         function(a)
  #     local  matrix, facs, mat, trunc, ls, GBASIStmp;
  #
  #     ls:=GeneratorsOfAffineSemigroup(a);
  #
  #     Info(InfoNumSgps,2,"Using singular 4ti2 interface for Graver.");
  #
  #     mat:=TransposedMat(ls);
  #     GBASIStmp:=GBASIS;
  #     GBASIS:=SINGULARGBASIS;
  #     matrix := GraverBasis(mat);
  #     GBASIS:=GBASIStmp;
  #
  #     trunc:=function(ls)
  #         return List(ls, y->Maximum(y,0));
  #     end;
  #
  #     matrix:=Set(matrix,trunc);
  #     return Union(Set(matrix, x->x*ls),ls);
  # end);


############################################################
# computes the Graver basis of matrix with integer entries
############################################################
# InstallMethod(GraverBasis,
#         "Computes the Graver basis of the matrix",
#         [IsRectangularTable],4,
# function(a)
#           #singular implementation
#   local graver, T, R, bintopair, ed, c;
#
#   bintopair:=function(pp)
#       local m1,m2, d1, d2, p;
#       p:=pp/LeadingCoefficientOfPolynomial(pp,MonomialLexOrdering());
#       m1:=LeadingMonomialOfPolynomial(p, MonomialLexOrdering());
#       m2:=m1-p;
#       d1:=List([1..ed], i->DegreeIndeterminate(m1,i));;
#       d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
#       return [d1,d2];
#   end;
#
#   if not(IsRectangularTable(a)) then
#     Error("The argument must be a matrix.");
#   fi;
#
#   if not(IsInt(a[1][1])) then
#     Error("The entries of the matrix must be integers.");
#   fi;
#
#   ed:=Length(a[1]);
#   T:=SingularBaseRing;
# 	R:=PolynomialRing(Rationals,ed);
# 	SingularSetBaseRing(R);
# 	SingularLibrary("sing4ti2");
# 	c:=SingularInterface("graver4ti2",[a],"ideal");
# 	graver:=List(GeneratorsOfTwoSidedIdeal(c), x-> bintopair(x));
#   graver:=List(graver,x->x[1]-x[2]);
# 	SingularSetBaseRing( T );
#   return Union(graver,-graver);
# end);


######################################################################
# Computes a minimal presentation of the affine semigroup a
######################################################################
InstallOtherMethod(MinimalPresentationOfAffineSemigroup,
	"Computes the minimal presentation of an affine semigroup",
	[IsAffineSemigroup],2,
        function(a)
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		Rtmp,R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens, GBASIStmp;


    Info(InfoNumSgps,2,"Using singular to compute minimal presentations.");

    ##computes the s degree of a monomial in the semigroup ideal
    sdegree:=function(m)
        local exp;
        exp:=List([1..ed], i->DegreeIndeterminate(m,i));
        return exp*msg;
    end;

    bintopair:=function(pp)
        local m1,m2, d1, d2, p;
        p:=pp/LeadingCoefficientOfPolynomial(pp,MonomialLexOrdering());
        m1:=LeadingMonomialOfPolynomial(p, MonomialLexOrdering());
        m2:=m1-p;
        d1:=List([1..ed], i->DegreeIndeterminate(m1,i));;
        d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
        return Set([d1,d2]);
    end;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;

    msg:=MinimalGenerators(a); 
    ed:=Length(msg);
    if ed=0 then
        return [];
    fi;
    zero:=List([1..ed],_->0);
    dim:=Length(msg[1]);
    vars:=List([1..ed+dim],i->X(Rationals,i));
    Rtmp:=SingularBaseRing;
    GBASIStmp:=GBASIS;
    GBASIS:=SINGULARGBASIS;
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
    SingularSetBaseRing(Rtmp);
    GBASIS:=GBASIStmp;
    if Zero(R) in mingen then
      return [];
    fi;
    return Set([1..Length(mingen)],i->bintopair(mingen[i]));
end);


##########################################################################
# Computes the Hilbert basis of the system A X=0 mod md, where the rows
# of A are the elements of ls.
# md can be empty of have some modulus, if the length of md is smaller than
# the lengths of the elements of ls, then the rest of equations are considered
# to be homogeneous linear Diophantine equations
# REQUERIMENTS: Singular with the library normaliz
##########################################################################
# InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousEquations,
#         "Computes the Hilbert basis of a system of linear Diophantine equations, some of them can be in congruences",
#         [IsMatrix,IsHomogeneousList],3,
#         function(ls,md)
#     local matcong, hb, ncong, ncoord, nequ, matfree, T, R;
#
#     Info(InfoNumSgps,2,"Using singular with normaliz.lib to find the Hilbert basis.");
#
#     if not(IsHomogeneousList(ls)) or not(IsHomogeneousList(md)) then
#         Error("The arguments must be homogeneous lists.");
#     fi;
#
#     if not(ForAll(ls,IsListOfIntegersNS)) then
#         Error("The first argument must be a list of lists of integers.");
#     fi;
#
#     ncong:=Length(md);
#
#     if ncong>0 and not(IsListOfIntegersNS(md)) then
#         Error("The second argument must be a lists of integers.");
#     fi;
#
#     if not(ForAll(md,x->x>0)) then
#         Error("The second argument must be a list of positive integers");
#     fi;
#
#     nequ:=Length(ls);
#     ncoord:=Length(ls[1]);
#     matcong:=[];
#     matfree:=[];
#
#     if ncoord=0 then
#         return [];
#     fi;
#
#     if ncong>0 and not(IsListOfIntegersNS(md)) then
#         Error("The second argument must be either an empty list or a list of integers");
#     fi;
#
#     if ncong>nequ then
#         Error("More mudulus than equations");
#     fi;
#
#     T:=SingularBaseRing;
#     R:=PolynomialRing(Rationals,1);
#     SingularSetBaseRing(R);
#     SingularLibrary("normaliz");
#
#     if nequ>ncong and ncong>0 then
#         matcong:=ls{[1..ncong]};
#         matcong:=TransposedMat(
#                          Concatenation(TransposedMat(matcong),[md]));
#         matfree:=ls{[ncong+1..nequ]};
#         hb:=SingularInterface("normaliz",[matfree,5,matcong,6],"matrix");
#         #NmzCone(["congruences",matcong,"equations",matfree]);
#     fi;
#
#     if nequ=ncong then
#         matcong:=TransposedMat(Concatenation(
#                          TransposedMat(ls),[md]));
#         hb:=SingularInterface("normaliz",[matcong,6],"matrix");
#         #NmzCone(["congruences",matcong]);
#     fi;
#     if ncong=0 then
#         matfree:=ls;
#         hb:=SingularInterface("normaliz",[matfree,5],"matrix");
#         #NmzCone(["equations",matfree]);
#     fi;
#
#     #NmzCompute(cone,"DualMode");
#     SingularSetBaseRing(T);
#     return Set(hb);#NmzHilbertBasis(cone);
# end);

##########################################################################
# Computes the Hilbert basis of the system ls*X>=0 over the nonnegative
# integers
# REQUERIMENTS: Singular with the library normaliz
##########################################################################
# InstallOtherMethod(HilbertBasisOfSystemOfHomogeneousInequalities,
#         "Computes the Hilbert basis of a system of inequalities",
#         [IsMatrix],3,
#         function(ls)
#     local hb,  ncoord, R, T;
#
#     Info(InfoNumSgps,2,"Using singular with normaliz.lib to find the Hilbert basis.");
#
#     if not(IsHomogeneousList(ls)) then
#         Error("The argument must be a homogeneous lists.");
#     fi;
#
#     if not(ForAll(ls,IsListOfIntegersNS)) then
#         Error("The argument must be a list of lists of integers.");
#     fi;
#
#     if not(Length(Set(ls, Length))=1) then
#         Error("The first argument must be a list of lists all with the same length.");
#     fi;
#
#     ncoord:=Length(ls[1]);
#
#     if ncoord=0 then
#         return [];
#     fi;
#
#     T:=SingularBaseRing;
#     R:=PolynomialRing(Rationals,1);
#     SingularSetBaseRing(R);
#     SingularLibrary("normaliz");
#
#     hb:=SingularInterface("normaliz",[ls,4],"matrix");
#     #NmzCone(["inequalities",ls,"signs",[List([1..ncoord],_->1)]]);
#     #NmzCompute(cone,"DualMode");
#
#     SingularSetBaseRing(T);
#
#     return Set(hb);
#     #NmzHilbertBasis(cone);
# end);
