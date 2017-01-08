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

############################################################
# computes a set of generators of the kernel congruence
# of the monoid morphism associated to the matrix m with
# nonnegative integer coefficients
############################################################
InstallOtherMethod(GeneratorsOfKernelCongruence,
        "Computes a set of generators of the kernel congruence of the monoid morphism associated to a matrix",
        [IsRectangularTable],6,
        function(m)
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		Rtmp, R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens;


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
    Rtmp:=SingularBaseRing;
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
	[IsRectangularTable, IsMonomialOrdering],6,
  function(m,ord)
  	local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
  	Rtmp, R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens;


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
  	Rtmp:=SingularBaseRing;
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
  	gens:=GroebnerBasis(ie);
  	SingularSetBaseRing(Rtmp);
  	return Set([1..Length(gens)],i->bintopair(gens[i]));
  end);


############################################################
# computes the Graver basis of matrix with integer entries
############################################################
InstallMethod(GraverBasis,
        "Computes the Graver basis of the matrix",
        [IsRectangularTable],4,
function(a)
          #singular implementation
  local graver, T, R, bintopair, ed, c;

  bintopair:=function(pp)
      local m1,m2, d1, d2, p;
      p:=pp/LeadingCoefficientOfPolynomial(pp,MonomialLexOrdering());
      m1:=LeadingMonomialOfPolynomial(p, MonomialLexOrdering());
      m2:=m1-p;
      d1:=List([1..ed], i->DegreeIndeterminate(m1,i));;
      d2:=List([1..ed], i->DegreeIndeterminate(m2,i));;
      return [d1,d2];
  end;

  if not(IsRectangularTable(a)) then
    Error("The argument must be a matrix.");
  fi;

  if not(IsInt(a[1][1])) then
    Error("The entries of the matrix must be integers.");
  fi;

  ed:=Length(a[1]);
  T:=SingularBaseRing;
	R:=PolynomialRing(Rationals,ed);
	SingularSetBaseRing(R);
	SingularLibrary("sing4ti2");
	c:=SingularInterface("graver4ti2",[a],"ideal");
	graver:=List(GeneratorsOfTwoSidedIdeal(c), x-> bintopair(x));
  graver:=List(graver,x->x[1]-x[2]);
	SingularSetBaseRing( T );
  return Union(graver,-graver);
end);


######################################################################
# Computes a minimal presentation of the affine semigroup a
######################################################################
InstallOtherMethod(MinimalPresentationOfAffineSemigroup,
	"Computes the minimal presentation of an affine semigroup",
	[IsAffineSemigroup],2,
        function(a)
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		Rtmp,R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens;


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
        return [d1,d2];
    end;

    if not(IsAffineSemigroup(a)) then
        Error("The argument must be an affine semigroup.");
    fi;

    msg:=GeneratorsOfAffineSemigroup(a); #for now we do not check minimality of the generators
    ed:=Length(msg);
    if ed=0 then
        return [];
    fi;
    zero:=List([1..ed],_->0);
    dim:=Length(msg[1]);
    vars:=List([1..ed+dim],i->X(Rationals,i));
    Rtmp:=SingularBaseRing;
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
    return Set([1..Length(mingen)],i->bintopair(mingen[i]));
end);
