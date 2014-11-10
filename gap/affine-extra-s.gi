if not TestPackageAvailability("singular") = fail then
    LoadPackage("singular");  
fi;


######################################################################
# Computes a minimal presentation of the affine semigroup a 
# REQUERIMENTS: SingularInterface
######################################################################
InstallOtherMethod(MinimalPresentationOfAffineSemigroup,
	"Computes the minimal presentation of an affine semigroup",
	[IsAffineSemigroup],2,
    local i, p, rel, rgb, msg, pol, ed,  sdegree, monomial, candidates, mp,
		R,id, ie, vars, mingen, exps, bintopair, dim, zero, gens;


    # REQUERIMENTS: SingularInterface   
    if IsPackageMarkedForLoading("SingularInterface","0.0") then
        TryNextMethod();
    fi;

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
end;

