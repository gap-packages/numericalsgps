#############################################################################
##
#W  polynomials.gi              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2015 by Manuel Delgado and  Pedro Garcia-Sanchez
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#################################################
##
#F NumericalSemigroupPolynomial(s,x)
## s is a numerical semigroup, and x a variable (or a value)
## returns the polynomial (1-x)\sum_{s\in S}x^s
##
##################################################
InstallGlobalFunction(NumericalSemigroupPolynomial, function(s,x)
	local gaps, p;

    if not IsNumericalSemigroup(s) then
        Error("The first argument must be a numerical semigroup.\n");
    fi;

	gaps:=GapsOfNumericalSemigroup(s);
	p:=List(gaps, g-> x^g);
	return 1+(x-1)*Sum(p);
end);

##############################################################
##
#F IsNumericalSemigroupPolynomial(f) detects
## if there exists S a numerical semigroup such that P_S(x)=f
## f is a polynomial
##############################################################
InstallGlobalFunction(IsNumericalSemigroupPolynomial, function(f)
local x, coef, gaps, small,  s, p,c;

if not(IsUnivariatePolynomial(f)) then
		Error("The argument must be a univariate polynomial");
fi;

if not(IsListOfIntegersNS(CoefficientsOfUnivariatePolynomial(f))) then
		return false;
fi;

if not(IsOne(LeadingCoefficient(f))) then
		return false;
fi;


x:=IndeterminateOfUnivariateRationalFunction(f);
p:=(f-1)/(x-1);
if not(IsUnivariatePolynomial(p)) then
		return false;
fi;
coef:=CoefficientsOfUnivariatePolynomial(p);
if Set(coef)<>Set([0,1]) then
		return false;
fi;
c:=Length(coef);
gaps:=Filtered([0..c-1], i->coef[i+1]<>0);
#Print("gaps ", gaps,"\n");
small:=Difference([0..c],gaps);
#Print("small ",small,"\n");

return RepresentsSmallElementsOfNumericalSemigroup(small);
end);

##############################################################
##
#F NumericalSemigroupFromNumericalSemigroupPolynomial(f) outputs
## a numerical semigroup S such that P_S(x)=f; error if no such
## S exists
## f is a polynomial
##############################################################
InstallGlobalFunction(NumericalSemigroupFromNumericalSemigroupPolynomial, function(f)
local x, coef, gaps,small, s, p,c;

if not(IsUnivariatePolynomial(f)) then
	Error("The argument must be a univariate polynomial");
fi;

if not(IsListOfIntegersNS(CoefficientsOfUnivariatePolynomial(f))) then
	Error("The argument is not a numerical semigroup polynomial");
fi;

if not(IsOne(LeadingCoefficient(f))) then
	Error("The argument is not a numerical semigroup polynomial");
fi;


x:=IndeterminateOfUnivariateRationalFunction(f);
p:=(f-1)/(x-1);
if not(IsUnivariatePolynomial(p)) then
	Error("The argument is not a numerical semigroup polynomial");
fi;
coef:=CoefficientsOfUnivariatePolynomial(p);
if Set(coef)<>Set([0,1]) then
	Error("The argument is not a numerical semigroup polynomial");
fi;
c:=Length(coef);
gaps:=Filtered([0..c-1], i->coef[i+1]<>0);
small:=Difference([0..c],gaps);
if not(RepresentsSmallElementsOfNumericalSemigroup(small)) then
	Error("The argument is not a numerical semigroup polynomial");
fi;
return NumericalSemigroupBySmallElements(small);
end);

###################################################
#F HilbertSeriesOfNumericalSemigroup(s,x)
## Computes the Hilber series of s in x : \sum_{s\in S}x^s
###################################################
InstallGlobalFunction(HilbertSeriesOfNumericalSemigroup,function(s,x)
	local m,ap;
    if not IsNumericalSemigroup(s) then
        Error("The first argument must be a numerical semigroup.\n");
    fi;

	if HasAperyList(s) then #uses J.Ramirez-Alfonsin trick
		m:=MultiplicityOfNumericalSemigroup(s);
		ap:=AperyListOfNumericalSemigroup(s);
		return 1/(1-x^m)*Sum(List(ap, w->x^w));
	fi;
	return NumericalSemigroupPolynomial(s,x)/(1-x);
end);


######################################################
##
#F Computes the Graeffe polynomial of p
##  see for instance [BD-cyclotomic]
##
######################################################
InstallGlobalFunction(GraeffePolynomial,function(p)
	local coef, h, g, i, f1,x;

	if not(IsUnivariatePolynomial(p)) then
		Error("The argument must be a univariate polynomial");
	fi;
	x:=IndeterminateOfUnivariateRationalFunction(p);
	coef:=CoefficientsOfUnivariatePolynomial(p);
	h:=[]; g:=[];
	for i in [1..Length(coef)] do
		if (i-1) mod 2 = 0 then
			Add(g,coef[i]);
		else
			Add(h,coef[i]);
		fi;
	od;
	#Print(g," ,",h,"\n");
	g:=UnivariatePolynomial(Rationals,g);
	h:=UnivariatePolynomial(Rationals,h);
	f1:=Value(g,x)^2-x*Value(h,x)^2;
	return f1/LeadingCoefficient(f1);
end);


#####################################################
##
#F IsCyclotomicPolynomial(f) detects
## if f is a cyclotomic polynomial using the method explained in
## BD-cyclotomic
#####################################################
InstallGlobalFunction(IsCyclotomicPolynomial,function(f)
	local f1, x, f2, mf;

	if not(IsUnivariatePolynomial(f)) then
		Error("The argument must be a univariate polynomial");
	fi;

	if not(IsListOfIntegersNS(CoefficientsOfUnivariatePolynomial(f))) then
		Error("The polynomial has not integer coefficients");
	fi;

	if not(IsOne(LeadingCoefficient(f))) then
		return false;
	fi;

	if not(IsIrreducible(f)) then
		return false;
	fi;

	x:=IndeterminateOfUnivariateRationalFunction(f);
	f1:=GraeffePolynomial(f);
	if (f=f1) then
		return true;
	fi;

	mf:=Value(f,-x);# Print(f1, ", ",f, ",", mf,"\n");

	if ((f1=mf) or (f1=-mf)) and IsCyclotomicPolynomial(mf/LeadingCoefficient(mf)) then
		return true;
	fi;

	f2:=Set(Factors(f1))[1];
	if f1=f2^2 and IsCyclotomicPolynomial(f2/LeadingCoefficient(f2)) then
		return true;
	fi;
	return false;
end);

########################################################################
##
#F IsKroneckerPolynomial(f) decides if
##   f is a Kronecker polynomial, that is,   a monic polynomial with integer coefficients
##   having all its roots in the unit circunference, equivalently, is a product of
##   cyclotomic polynomials
## New implementation with A. Herrera-Poyatos that does not need factoring
#########################################################################
InstallGlobalFunction(IsKroneckerPolynomial,function(f)

	local x, sf, f1, fs, fn, fp, factors_graeffe;

	if not(IsUnivariatePolynomial(f)) then
			Error("The argument must be a polynomial in one variable");
	fi;

	if IsZero(f) then
			return false;
	fi;

	x:=IndeterminateOfLaurentPolynomial(f);

	# Take the largest square free divisor.
	sf:=f/Gcd(f,Derivative(f));

	# Remove the factors x, x+1 and x-1.
	if Value(sf, 0) = 0 then
			sf := sf / x;
	fi;
	if Value(sf, 1) = 0 then
			sf := sf/(x-1);
	fi;
	if Value(sf, -1) = 0 then
			sf := sf/(x+1);
	fi;

	# Check if the polynomial is a constant.
	if Degree(sf) = 0 then
			return true;
	fi;

	# Check if the polynomial has even degree and is self reciprocal.
	if Degree(sf) mod 2 <> 0 or not(IsSelfReciprocalUnivariatePolynomial(sf)) then
			return false;
	fi;

	# Apply the graeffe method to sf. Note that if sf(+-x) = f1, then
	# sf is Kronecker.
	f1:= GraeffePolynomial(sf);
	if sf = f1 then
			return true;
	fi;
	if Value(sf,-x) = f1 then
			return true;
	fi;

	# Assume that sf is Kronecker.
	# Find the descomposition of sf = fs*fp*fn, where:
	# - fs is the part of sf which verifies Graeffe(fs) = (g(x))^2
	# - fp is the part of sf which verifies Graeffe(fs) = fs.
	# - fn is the part of sf which vefifies Graeffe(fs)(x) = fs(-x).
	fs := Value(Gcd(Derivative(f1), f1), x^2);
	fp := Gcd(sf / fs, f1);
	fn := sf / (fs * fp);

	factors_graeffe := Difference([fs, fp, fn], [1+0*x]);

	if Length(factors_graeffe) = 1 then
			if IsOne(fs) then
					# We must have f = fp or f = fs, but we obtained Graeffe(sf) != sf,
					# Graeffe(sf) != sf(-x), a contradiction. Hence sf is not Kronecker.
					return false;
			else
					# sf is Kronecker if and only if f1 / fs(\sqrt{x}) is Kronecker.
					return IsKroneckerPolynomial(f1 /  Gcd(f1,Derivative(f1)));
			fi;
	else
			# sf is Kronecker if and only if fs, fp and fn are Kronecker.
			return ForAll(factors_graeffe, IsKroneckerPolynomial);
	fi;
end);


###########################################
##
#P IsCyclotomicNumericalSemigroup(s)
## Checks if the polynomial fo s is Kronecker
###########################################
InstallMethod(IsCyclotomicNumericalSemigroup,
	"Detects if the polinomial semigroup of the semigroup is Kronecker",
	[IsNumericalSemigroup],
	function(s)
	local x, f;
	x:=X(Rationals,"x");
	f:=NumericalSemigroupPolynomial(s,x);
	return IsKroneckerPolynomial(f);# ForAll(Factors(f),IsCyclotomicPolynomial);
end);

InstallTrueMethod(IsCyclotomicNumericalSemigroup, IsACompleteIntersectionNumericalSemigroup);

#####################################################
##
#F IsSelfReciprocalUnivariatePolynomial(p)
## Checks if the univariate polynomial p is selfreciprocal
## New implementation by A. Herrera-Poyatos
#####################################################
InstallGlobalFunction(IsSelfReciprocalUnivariatePolynomial,function(p)
	local cf;

	if not(IsUnivariatePolynomial(p)) then
		Error("The argument must be a polynomial\n");
	fi;

	# Check if the polynomial is self-reciprocal.
	cf:=CoefficientsOfUnivariatePolynomial(p);
	return cf=Reversed(cf);
end);

#################################################################
##
# F SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(f)
##  Computes the semigroup of values {mult(f,g) | g curve} of a plane curve
##   with one place at the infinity in the variables X(Rationals,1) and X(Rationals,2)
##  f must be monic on X(Rationals(2))
##  SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity(f,"all")
##    The same as above, but the output are the approximate roots and
##    delta-sequence
##
#################################################################
InstallGlobalFunction(SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity, function(arg)
	local monomials, degree, degreepol, polexprc, polexprs, polexpr, app, tt1, sv;
	#returns the set of monomials of a polynomial
	monomials:=function(f)
		local term, out,temp;
		out:=[];
		if IsZero(f) then
			return ([0]);
		fi;
		temp:=f;
		while not(IsZero(temp)) do
			term:=LeadingTermOfPolynomial(temp,MonomialLexOrdering());
			Add(out,term);
			temp:=temp-term;
		od;
		return out;

	end;

	#returns the total weighted degree of a monomial wrt l
	degree:=function(mon,l)
		local n;
		n:=Length(l);
		if IsRat(mon) then
			return 0;
		fi;
		return Sum(List([1..n], i->DegreeIndeterminate(mon,i)*l[i]));
	end;

	#returns the total weighted degree of a polynomial wrt l
	degreepol:=function(pol,l)
		return Maximum(List(monomials(pol), t->degree(t,l)));
	end;
	#finds an expression of f in terms of p, and outputs it as a polynomial in x, var is the list of variables in f and p
	polexpr:=function(f,p,var,x)
		local rem, quo, pr;

		pr:=PolynomialDivisionAlgorithm(f,[p],MonomialLexOrdering(var));
		rem:=[pr[1]];
		quo:=pr[2][1];
		while not(IsZero(quo)) do
			pr:=PolynomialDivisionAlgorithm(quo,[p],MonomialLexOrdering(var));
			Add(rem,pr[1]);
			quo:=pr[2][1];
		od;
		return Sum(List([1..Length(rem)],i->rem[i]*x^(i-1)));
	end;
	#uses the previous function to apply recurrently the procedure for all polynomials in ps and variables in xs
	polexprs:=function(f,ps,var,xs)
		local i, len, out;

		len:=Length(ps);
		out:=f;
		for i in [1..len] do
			out:=polexpr(out,ps[i],var,xs[i]);
		od;
		return out;
	end;
	#expression of f in terms of p; both in variables x and y...
	polexprc:=function(f,p)
		local rem, quo, pr;

		pr:=PolynomialDivisionAlgorithm(f,[p],MonomialLexOrdering([X(Rationals,2),X(Rationals,1)]));
		rem:=[pr[1]];
		quo:=Value(pr[2][1],[],[]);
		while not(IsZero(quo)) do
			pr:=PolynomialDivisionAlgorithm(quo,[p],MonomialLexOrdering([X(Rationals,2),X(Rationals,1)]));
			Add(rem,pr[1]);
			quo:=pr[2][1];
		od;
		return Reversed(rem);
	end;
	##TT1
	tt1:=function(f,g,y)
		local d;
		d:=DegreeIndeterminate(f,y)/DegreeIndeterminate(g,y);
		if d>0 then
			return g+polexprc(f,g)[2]/d;
		fi;
		return f;
	end;
	#approximate root
	app:=function(f,y,d)
		local G,S,F,P,e,t,coef;

		e:=DegreeIndeterminate(f,y)/d;
		G:=tt1(f,y^e,y);
		S:=polexprc(f,G);
		P:=S[2];
		#tschirnhausen transform
		while not(IsZero(P)) do
			F:=G+P/e;
			G:=tt1(f,F,y);
			S:=polexprc(f,G);
			P:=S[2];
		od;
		return G;
	end;
	#semigroup of values
	sv:=function(f)
		local  m, d,dd, n,g,it, max, lt, e, coef, deg, tsti,rd, var, id, R, aroots;

		if not(IsPolynomial(f)) then
			Error("The argument must be a polynomial\n");
		fi;

		R:=PolynomialRing(Rationals,[X(Rationals,1),X(Rationals,2)]);
		if not(f in R) then
						Error("The argument must be a polynomial in ", R,"\n");
		fi;

		coef:=PolynomialCoefficientsOfPolynomial(f,X(Rationals,1));
		if not(IsConstantRationalFunction(coef[Length(coef)])) then
			Error("The polynomial does not have a single place at infinity or the leading coefficient in ", X(Rationals,1)," is not a rational number\n");
		fi;
		coef:=PolynomialCoefficientsOfPolynomial(f,X(Rationals,2));
		if not(IsConstantRationalFunction(coef[Length(coef)])) then
			Error("The polynomial does not have a single place at infinity or or the leading coefficient in ", X(Rationals,2)," is not a rational number\n");
		fi;
		f:=f/coef[Length(coef)];
		m:=[];
		coef:=PolynomialCoefficientsOfPolynomial(f,X(Rationals,2));
		Info(InfoNumSgps,2,"f ",f);
		Info(InfoNumSgps,2,"f ",coef);
		it:=coef[1];
		if IsZero(it) then
			Error("The polynomial is not irreducible\n");
		fi;

		m[1]:=DegreeIndeterminate(f,X(Rationals,2));
		m[2]:=DegreeIndeterminate(PolynomialCoefficientsOfPolynomial(f,X(Rationals,2))[1],X(Rationals,1));
		n:=2;
		d:=Gcd(m);
		e:=m[1]/d;
		g:=f;
		var:=[X(Rationals,2),X(Rationals,1)];
		aroots:=[X(Rationals,2)];
		while d>1  do
			Add(var,X(Rationals,n+1));
			rd:=app(f,X(Rationals,2),d);
			Add(aroots,rd);
			g:=polexprs(f,Reversed(aroots),var,Reversed(List([2..n+1],i->X(Rationals,i))));
			coef:=PolynomialCoefficientsOfPolynomial(g, X(Rationals,n+1));
			Info(InfoNumSgps,2,"We obtain coefficients: ",coef);
			it:=coef[1];
			Info(InfoNumSgps,2,"Independent term ", it);
			max:=degreepol(it,m/Gcd(m));
			if (max=0) then #or (d=Gcd(d,max)) or (max in m) then
				Error("The polynomial is not irreducible or it has not a single place at infinity (deg 0)\n");
			fi;
			#irreducibility test
			tsti:=First(coef{[2..Length(coef)]}, t->degreepol(t,m/d)>=max);
			if tsti<>fail then
				Info(InfoNumSgps,1,"Term ", tsti," produces error");
				Error("The polynomial is not irreducible", "\n");
			fi;
			dd:=Gcd(d,max);
			Info(InfoNumSgps,2,"New candidate: ",max);
			if (d=Gcd(d,max)) or (max in m) then
				Error("The polynomial is not irreducible or it has not a single place at infinity\n");
			fi;
			m[n+1]:=max;
			if(m[n]*Gcd(m{[1..n-1]})<=max*d)then
				Error("The polynomial is not irreducible (not subdescending sequence)", m,"\n");
			fi;
			e:=d/Gcd(d,max);
			d:=dd;
			n:=n+1;
		od;
		return [m,aroots];
	end;

	if Length(arg)=1 then
		return NumericalSemigroup(sv(arg[1])[1]);
	fi;
	if Length(arg)=2  and arg[2] = "all" then
		return sv(arg[1]);
	fi;
	Error("Wrong number of arguments\n");
end);


#########################################################################
##
#F IsDeltaSequence(l)
## tests whether or not l is a \delta-sequence (see for instancd [AGS14])
#########################################################################
InstallGlobalFunction(IsDeltaSequence,function(l)
	local d,e,h;

	if not(IsListOfIntegersNS(l)) then
		Error("The argument must be a list of positive integers\n");
	fi;
	if Gcd(l)<>1 then
		Info(InfoNumSgps,2,"The gcd of the list is not one");
		return false;
	fi;
	h:=Length(l)-1;
	d:=List([1..h+1], i->Gcd(l{[1..i]}));
	e:=List([1..h], i->d[i]/d[i+1]);
	if Length(l)=1 then
		return true;
	fi;
	if (l[1]<=l[2]) or (d[2]=l[2]) then
		Info(InfoNumSgps,2,"Either the first element is smaller than or equal to the second, or the gcd of the first to equals the second");
		return false;
	fi;
	if Length(Set(d))<h+1 then
		Info(InfoNumSgps,2,"The list of gcds is not strctitly decreasing");
		return false;
	fi;

	return ForAll([1..h], i->l[i+1]/Gcd(l{[1..i+1]}) in NumericalSemigroup(l{[1..i]}/Gcd(l{[1..i]})))
			  and
			  ForAll([2..h], i->l[i]*Gcd(l{[1..i-1]}) >l[i+1]*Gcd(l{[1..i]}));
end);


#########################################################
##
#F DeltaSequencesWithFrobeniusNumber(f)
##   Computes  the list of delta-sequences with Frobenius number f
##
#########################################################
InstallGlobalFunction(DeltaSequencesWithFrobeniusNumber,function(f)
	local abs, test;

	if not(IsInt(f)) then
		Error("The argument must be an integer\n");
	fi;

	if (f<-1) or IsEvenInt(f) then
		return [];
	fi;

	if f=-1 then
		return [[1]];
	fi;

	abs:=function(f)
		local dkcand, dk, rk, fp, candr, bound, total;

		if (f<-1) or (f mod 2=0) then
			return [];
		fi;

		if (f=-1) then
			return [[1]];
		fi;

		if (f=1) then
			return [[2,3],[3,2]];
		fi;

		total:=[[f+2,2],[2,f+2]];
		for rk in Reversed([2..f-1]) do
			bound:=(Int((f+1)/(rk-1)+1));
			dkcand:=Filtered([2..bound],d->(Gcd(d,rk)=1)and((f+rk) mod d=0));
			for dk in dkcand do
				fp:=(f+rk*(1-dk))/dk;
				candr:=Filtered(abs(fp), l->  rk in NumericalSemigroup(l));
				candr:=List(candr, l-> Concatenation(l*dk, [rk]));
				candr:=Filtered(candr, test);
				candr:=Filtered(candr,l->l[1]>l[2]);
				total:=Union(total,candr);
			od;
		od;
		return total;
	end;

	test:=function(l)
		local len;
		len:=Length(l);
		return ForAll([3..len], k-> l[k-1]*Gcd(l{[1..k-2]})>l[k]*Gcd(l{[1..k-1]}));
	end;

	return Difference(abs(f),[[2,f+2]]);
end);

#####################################################
##
#F CurveAssociatedToDeltaSequence(l)
##  computes the curve associated to a delta-sequence l in
##  the variables X(Rationals,1) and X(Rationals,2)
##  as explained in [AGS14]
##
#####################################################
InstallGlobalFunction(CurveAssociatedToDeltaSequence,function(l)
	local n, d, f, fact,facts, g, e, k, ll, len, dd, x,y;

	if not(IsDeltaSequence(l)) then
		Error("The sequence is not valid\n");
	fi;
	x:=X(Rationals,1);
	y:=X(Rationals,2);
	len:=Length(l);
	if len<2 then
		Error("The sequence must have at least two elements\n");
	fi;
	if Gcd(l)<>1 then
		Error("The sequence must have gcd equal to one\n");
	fi;
	if len=2 then
		return y^(l[1])-x^(l[2]);
	fi;

	e:=List([1..len-1],k->Gcd(l{[1..k]})/Gcd(l{[1..k+1]}));
	g:=[]; g[1]:=x; g[2]:=y;
	for k in [2..len] do
		d:=Gcd(l{[1..k-1]});
		dd:=Gcd(l{[1..k]});
		ll:=l{[1..k-1]}/d;
		fact:=First(FactorizationsIntegerWRTList(l[k]/dd, ll),ff->ForAll([2..k-1], i->ff[i]<e[i-1]));

		if fact=fail then
			Error("The sequence is not valid\n");
		fi;
		g[k+1]:=g[k]^e[k-1]-Product(List([1..k-1], i->g[i]^fact[i]));
		Info(InfoNumSgps,2,"Temporal curve: ",g[k+1]);
	od;
	return g[Length(g)];
end);

#################################################################
##
#F SemigroupOfValuesOfCurve_Local(arg)
## Computes the semigroup of values of R=K[pols],
## that is, the set of possible order of series in this ring
## pols is a set of polynomials in a single variable
## The semigroup of values is a numerical semigroup if l(K[[x]]/R) is finite
## If this length is not finite, the output is fail
## If the second argument "basis" is given, then the output is a basis B of
## R such that o(B) minimally generates o(R), and it is reduced
## If the second argument is an integer, then the output is a polynomial f in R
## with o(f) that value (if there is none, then the output is fail)
## Implementation based in [AGSM14]
###########################################################
InstallGlobalFunction(SemigroupOfValuesOfCurve_Local, function(arg)
    local G, F, n, T, max, s, d, newG, c, kernel,var, tomoniclocal, order, subduction, t,tt, TT, narg,val,facts,pols, msg,reduce;

    ### the order of the series (polynomial)
    order:=function(p)
        local cl;

        if IsInt(p) or IsZero(p) then
            return 0;
        fi;

        cl:=CoefficientsOfUnivariatePolynomial(p);
        return First([1..Length(cl)], i->cl[i]<>0)-1;
    end;

    #### transforms to monic
    tomoniclocal:=function(p)
        local cl;
        if IsZero(p) then
            return 0*X(Rationals,var);
        fi;
        if IsConstantRationalFunction(p) or IsRat(p) then
            return 1;
        fi;
        cl:=CoefficientsOfUnivariatePolynomial(p);
        return p/First(cl,i->i<>0);
    end;

    #### subduction: tries to express p as a polynomial in pols
    #### pols are supposed to be monic
    subduction:=function(p)
        local initial, facts, ord;

        ord:=order(p);

        if d<>1 and (ord mod d)<>0 then
          return p;
        fi;

        if order(p)=0 then
            return 0*X(Rationals,var);
        fi;

        if order(p)>c then
          if d=1 then
            return 0*X(Rationals,var);
          else
            return p;
          fi;
        fi;

        initial:=List(F,order);

        facts:=FactorizationsIntegerWRTList(order(p), initial);
        if facts=[] then
            return p;
        fi;

        Info(InfoNumSgps,2,"Reducing ",p, " to ",
             tomoniclocal(p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));

        return subduction(tomoniclocal(p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));
    end;

    #### reduces the elements in the minimal basis F, p is monic

    reduce:=function(p)
        local ords, cfs, cf, fact, reduced, head, tail;

        head:=X(Rationals,var)^order(p);
        tail:=p-head;

        ords:=List(F,order);

        repeat
            reduced:=false;
            #Print(p," ");

            cfs:=CoefficientsOfUnivariatePolynomial(tail);
            cf:=First([1..Length(cfs)], i->(cfs[i]<>0) and ((i-1) in s));


            if cf<>fail then
                fact:=FactorizationsIntegerWRTList(cf-1,ords)[1];
                tail:=(tail-cfs[cf]*Product(List([1..Length(F)],i->F[i]^(fact[i])))) mod X(Rationals,var)^c;
                cfs:=CoefficientsOfUnivariatePolynomial(tail);
            else
                reduced:=true;
            fi;
        until reduced;
        return head+tail;


    end;


    #### computes the relations among the polynomials in pols
    kernel:=function(pols)
        local  p,  msg,  ed,  mp, minp, eval, candidates, c, pres, rclass, exps;


        eval:=function(pair)
            local m1,m2;
            m1:=tomoniclocal(Product(List([1..ed],i-> pols[i]^pair[1][i])));
            m2:=tomoniclocal(Product(List([1..ed],i-> pols[i]^pair[2][i])));
            return tomoniclocal(-m1+m2);
        end;

        msg:=List(pols,order);
        ed:=Length(msg);
        if ed=0 then
            return [];
        fi;
        minp:=GeneratorsOfKernelCongruence(List(msg,x->[x]));
        Info(InfoNumSgps,2,"The exponents of the binomials of the kernel are ",minp);
        # Contrary to the global case, here it is faster not to compute a minimal presentation
        # candidates:=Set(minp, x->x[1]*msg);
        # pres:=[];
        # for c in candidates do
        #     exps:=FactorizationsIntegerWRTList(c,msg);
        #     rclass:=RClassesOfSetOfFactorizations(exps);
        #     if Length(rclass)>1 then
        #         pres:=Concatenation(pres,List([2..Length(rclass)],
        #                       i->[rclass[1][1],rclass[i][1]]));
        #     fi;
        # od;
        return List(minp,p->eval(p));
    end; #end of kernel

    narg:=Length(arg);

    if narg=2 then
        pols:=arg[1];
        val:=arg[2];
        if not(IsInt(val)) and not(val="basis") then
           Error("The second argument must be an integer or the string 'basis'");
        fi;
    fi;
    if narg=1 then
        pols:=arg[1];
    fi;

    if narg>2 then
        Error("Wrong number of arguments (two or one)");
    fi;



    if not(IsHomogeneousList(pols)) then
        Error("The argument must be a list of polynomials.");
    fi;

    if not(ForAll(pols, IsUnivariatePolynomial)) then
        Error("The argument must be a list of polynomials.");
    fi;
    if Length(Set(pols, IndeterminateOfLaurentPolynomial))<>1 then
        Error("The arguments must be polynomials in the same variable; constants not allowed nor the empty list.");
    fi;

    var:=IndeterminateNumberOfLaurentPolynomial(pols[1]);

    F:=ShallowCopy(pols);
    Sort(F,function(a,b) return order(a)< order(b); end);
    F:=List(F,tomoniclocal);
    G:=List(F,order);
    n:=0;

    while true do
        d:=Gcd(G);
        s:=NumericalSemigroup(G/d);
        c:=d*ConductorOfNumericalSemigroup(s);
        T:=kernel(F);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"The kernel evaluated in the polynomials is ",T);
        T:=Set(T,subduction);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"After subduction: ",T);
        if Gcd(G) = 1 then
            T:=Filtered(T, t->order(t)<c);
        fi;

        if T=[] or F=Union(F,T) then
            d:=Gcd(G);
            if d=1 then
                s:=NumericalSemigroup(G);
                if narg=1 then
                    return s;
                fi;
                msg:=MinimalGeneratingSystem(s);
                F:=Filtered(F,f->order(f) in msg);
                if val="basis" then
                    return List(F,reduce);
                fi;

                if IsInt(val) then
                    Info(InfoNumSgps,2,"The generators of the algebra are ",F);
                    facts:=FactorizationsIntegerWRTList(val,List(F,order));
                    if facts=[] then
                        return fail;
                    fi;
                    return reduce(Product(List([1..Length(facts[1])],i->F[i]^facts[1][i])));
                fi;
            fi;
            Info(InfoNumSgps,2,"The monoid is not a numerical semigroup and it is generated by ",
                 G);
            #d*MinimalGeneratingSystem(NumericalSemigroup(G/d)));
            #return fail;
        fi;
        Info(InfoNumSgps,2,"Adding ",T," to my list of polynomials");
        F:=Union(F,T);
        newG:=Set(T,order);
        G:=Union(G,newG);
        Info(InfoNumSgps,2,"The set of possible values uptates to ",G);
        n:=n+1;
        d:=Gcd(G);
        s:=NumericalSemigroup(G/d);
        Info(InfoNumSgps,2,"Small elements ",d*SmallElements(s));
    od;

    return fail;
end);

#################################################################
##
#F SemigroupOfValuesOfCurve_Global(arg)
## Computes the semigroup of values of R=K[pols],
## that is, the set of possible degrees of polynomials in this ring
## pols is a set of polynomials in a single variable
## The semigroup of values is a numerical semigroup if l(K[x]/R) is finite
## If this length is not finite, the output is fail
## If the second argument "basis" is given, then the output is a basis B of
## R such that deg(B) minimally generates deg(R), and it is reduced
## If the second argument is an integer, then the output is a polynomial f in R
## with deg(f) that value (if there is none, then the output is fail)
## Implementation based in [AGSM14]
###########################################################
InstallGlobalFunction(SemigroupOfValuesOfCurve_Global, function(arg)
    local G, F, n, T, max, s, d, newG, c, kernel,var, tomonicglobal, degree, subduction, pols, val, narg, degs, facts, reduce, msg;

    ### the degree of the polynomial
    degree:=function(p)
        return DegreeOfUnivariateLaurentPolynomial(p);

    end;

    #### transforms to monic
    tomonicglobal:=function(p)
        if IsZero(p) then
            return 0*X(Rationals,1);
        fi;
        return p/LeadingCoefficient(p);
    end;

    #### subduction: tries to express p as a polynomial in pols
    #### pols are supposed to be monic
    subduction:=function(p)
        local initial, facts;


        if IsZero(p) then
            return p;
        fi;

        if degree(p)=0 then
            return 0*X(Rationals,1);
        fi;

        initial:=List(F,degree);

        facts:=FactorizationsIntegerWRTList(degree(p), initial);
        if facts=[] then
            return p;
        fi;

        Info(InfoNumSgps,2,"Reducing ",p, " to ",
             tomonicglobal(p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));

        return subduction(tomonicglobal(
          p-Product(List([1..Length(F)],i->F[i]^(facts[1][i])))));
    end;

    #### reduces the elements in the minimal basis F, p is monic

    reduce:=function(p)
    local degs, cfs, cf, fact, reduced, head, tail;

    head:=X(Rationals,var)^degree(p);
    tail:=p-head;

    degs:=List(F,degree);

    repeat
      reduced:=false;
      #Print(p," ");

      cfs:=CoefficientsOfUnivariatePolynomial(tail);
      cf:=First([1..Length(cfs)], i->(cfs[i]<>0) and ((i-1) in s));
      #Error("blabla");
      #Print("cf",cf,"\n");
      if cf<>fail then
        fact:=FactorizationsIntegerWRTList(cf-1,degs)[1];
        tail:=(tail-cfs[cf]*Product(List([1..Length(F)],i->F[i]^(fact[i]))));
        cfs:=CoefficientsOfUnivariatePolynomial(tail);
      else
        reduced:=true;
      fi;
    until reduced;

    return head+tail;
    end;


    #### computes the relations among the polynomials in pols
    kernel:=function(pols)
        local  p,  msg,  ed,  mp, minp, eval, candidates, c, pres, rclass, exps;

        eval:=function(pair)
            local m1,m2;
            m1:=tomonicglobal(Product(List([1..ed],i-> pols[i]^pair[1][i])));
            m2:=tomonicglobal(Product(List([1..ed],i-> pols[i]^pair[2][i])));
            return tomonicglobal(-m1+m2);
        end;

        msg:=List(pols,degree);
        ed:=Length(msg);
        if ed=0 then
            return [];
        fi;

        minp:=GeneratorsOfKernelCongruence(List(msg,x->[x]));
        Info(InfoNumSgps,2,"The exponents of the binomials of the kernel are ",minp);
        candidates:=Set(minp, x->x[1]*msg);
        pres:=[];
        for c in candidates do
            exps:=FactorizationsIntegerWRTList(c,msg);
            rclass:=RClassesOfSetOfFactorizations(exps);
            if Length(rclass)>1 then
                pres:=Concatenation(pres,List([2..Length(rclass)],
                              i->[rclass[1][1],rclass[i][1]]));
            fi;
        od;
        return List(pres,p->tomonicglobal(eval(p)));


    end; #end of kernel

    narg:=Length(arg);

    if narg>2 then
        Error("Wrong number of arguments (two or one)");
    fi;
    if narg=1 then
        pols:=arg[1];
    fi;

    if narg=2 then
      pols:=arg[1];
      val:=arg[2];
      if not(IsInt(val)) and not(val="basis") then
        Error("The second argument must be an integer or the string 'basis'");
      fi;
    fi;

    if not(IsHomogeneousList(pols)) then
        Error("The argument must be a list of polynomials.");
    fi;

    if not(ForAll(pols, IsUnivariatePolynomial)) then
        Error("The argument must be a list of polynomials.");
    fi;
    if Length(Set(pols, IndeterminateOfLaurentPolynomial))<>1 then
        Error("The arguments must be polynomials in the same variable; constants not allowed nor the empty list.");
    fi;

    var:=IndeterminateNumberOfLaurentPolynomial(pols[1]);

    F:=ShallowCopy(pols);
    Sort(F,function(a,b) return degree(a)< degree(b); end);
    F:=List(F,tomonicglobal);
    G:=List(F,degree);
    n:=0;

    while true do
        T:=kernel(F);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"The kernel evaluated in the polynomials is ",T);
        T:=Set(T,subduction);
        T:=Filtered(T, x->not(IsZero(x)));
        Info(InfoNumSgps,2,"After subduction: ",T);
        if Gcd(G) = 1 then
            s:=NumericalSemigroup(G);
            c:=ConductorOfNumericalSemigroup(s);
            T:=Filtered(T, t->degree(t)<c);
        fi;

        if T=[] or F=Union(F,T) then
            d:=Gcd(G);
            if d=1 then
              s:=NumericalSemigroup(G);
              if narg=1 then
              return s;
              fi;
              msg:=MinimalGeneratingSystem(s);
              F:=Filtered(F,f->degree(f) in msg);
              if val="basis" then
                return List(F,reduce);
              fi;

              if IsInt(val) then
                Info(InfoNumSgps,2,"The generators of the algebra are ",F);
                facts:=FactorizationsIntegerWRTList(val,List(F,degree));
                if facts=[] then
                  return fail;
                fi;
                return reduce(Product(List([1..Length(facts[1])],i->F[i]^facts[1][i])));
              fi;
            fi;
            Info(InfoNumSgps,2,"The monoid is not a numerical semigroup and it is generated by ",
                 G);
            #d*MinimalGeneratingSystem(NumericalSemigroup(G/d)));
            return fail;
        fi;
        Info(InfoNumSgps,2,"Adding ",T," to my list of polynomials");
        F:=Union(F,T);
        newG:=Set(T,degree);
        G:=Union(G,newG);
        if narg=2 and val in G then
            return First(F,f->degree(f)=val);
        fi;

        Info(InfoNumSgps,2,"The set of possible values uptates to ",G);
        n:=n+1;
        d:=Gcd(G);
        s:=NumericalSemigroup(G/d);
        Info(InfoNumSgps,2,"Small elements ",d*SmallElements(s));
    od;
    return fail;
end);


##################################################################
##
#F GeneratorsModule_Global(A,M)
##
## A and M are lists of polynomials in the same variable
## Computes a basis of the ideal MK[A], that is, a set F such that
## deg(F) generates the ideal deg(MK[A]) of deg(K[A]), where deg
## stands for degree
##################################################################
InstallGlobalFunction(GeneratorsModule_Global, function(Al,M)

local S, gens, gM, a, b, da, db, i, j, rs, rd, rel, fcta, fctb, C, pair, reduction, n, reduce,  A, R, t;

R:=function(a,b,s)
	local i, mg;
	i:=IntersectionIdealsOfNumericalSemigroup(a+s,b+s);
	mg:=MinimalGenerators(i);
	return List(mg, m->[m-a,m-b]);
end;

# improve to reducing all terms, not only the leading term
reduce:=function(A,M,f)
	local gens,geni,cand,d, fact, c, r, s,a, ds, cs, csa;
	gens:=List(A, DegreeOfLaurentPolynomial);
	s:=NumericalSemigroup(gens);
	geni:=List(M,DegreeOfLaurentPolynomial);
	if IsZero(f) then
		return f;
	fi;
	r:=f;
	cs:=CoefficientsOfUnivariatePolynomial(r);
	ds:=Filtered([1..Length(cs)],i->not(IsZero(cs[i])))-1;
	c:=First([1..Length(geni)], i->ForAny(ds, d->d-geni[i] in s));
	if c<>fail then
		d:=First(ds,x->x-geni[c] in s);
	fi;
	while c<>fail do
		fact:=FactorizationsIntegerWRTList(d-geni[c],gens);
		a:=M[c]*Product(List([1..Length(gens)],i->A[i]^fact[1][i]));
		csa:=CoefficientsOfUnivariatePolynomial(a);
		r:=csa[Length(csa)]*r-cs[d+1]*a;#r-cs[d+1]*a/csa[Length(csa)];
		#Info(InfoNumSgps,2,"New reduction ",r," degree ",d," coeff ",cs[d+1]);
		if IsZero(r) then
			return r;
		fi;
		cs:=CoefficientsOfUnivariatePolynomial(r);
		ds:=Filtered([1..Length(cs)],i->not(IsZero(cs[i])))-1;
		c:=First([1..Length(geni)], i->ForAny(ds, d->d-geni[i] in s));
		if c<>fail then
			d:=First(ds,x->x-geni[c] in s);
		fi;
	od;

	return r/cs[Length(cs)];
end;

if not(IsHomogeneousList(Al)) then
		Error("The first argument must be a list of polynomials.");
fi;

if not(IsHomogeneousList(M)) then
		Error("The second argument must be a list of polynomials.");
fi;

if not(ForAll(Union(Al,M), IsUnivariatePolynomial)) then
		Error("The arguments must be lists of polynomials.");
fi;
if Length(Set(Union(Al,M), IndeterminateOfLaurentPolynomial))<>1 then
		Error("The arguments must be lists of polynomials in the same variable; constants not allowed nor the empty list.");
fi;

t:=IndeterminateNumberOfLaurentPolynomial(Al[1]);


A:=SemigroupOfValuesOfCurve_Global(Al,"basis");#List(A, DegreeOfLaurentPolynomial);
	gens:=List(A, DegreeOfLaurentPolynomial);
	S:=NumericalSemigroup(gens);
	n:=Length(A);

	gM:=ShallowCopy(M);
	C:=[];
	for i in [1..Length(gM)] do
		for j in [i+1..Length(gM)] do
			Add(C,[gM[i],gM[j]]);
		od;
	od;
	while C<>[] do
		pair:=Remove(C,1);
		a:=pair[1];
		b:=pair[2];
		da:=DegreeOfLaurentPolynomial(a);
		db:=DegreeOfLaurentPolynomial(b);
		rs:=R(da,db,S);
		reduction:=true;
		for rel in rs do
			fcta:=FactorizationsIntegerWRTList(rel[1],gens)[1];
			fctb:=FactorizationsIntegerWRTList(rel[2],gens)[1];
			#rd:=reduce(A,gM,a*Product(List(fcta, e->t^e))-b*Product(List(fctb, e->t^e)));
			rd:=reduce(A,gM,a*Product(List([1..n], i->A[i]^fcta[i]))-b*Product(List([1..n], i->A[i]^fctb[i])));
			if not(IsZero(rd)) then
					Info(InfoNumSgps,2,"new generator ",rd," of degreee ",DegreeIndeterminate(rd,t));
					C:=Union(C,List(gM, x->[x,rd]));
					Add(gM,rd);
					reduction:=false;
			fi;
		od;
		while not reduction do
			#Info(InfoNumSgps,2,"Reducing...");
			reduction:=true;
			a:=First(gM, x->x<>reduce(A,Difference(gM,[x]),x));
			if a<>fail then
				rd:=reduce(A,Difference(gM,[a]),a);
				if IsZero(rd) then
					gM:=Difference(gM,[a]);
					Info(InfoNumSgps,2,"Removing redundant generator: ",a);
				else
					gM:=Union(Difference(gM,[a]),[rd]);
					Info(InfoNumSgps,2,"Replacing ",a," by ",rd);
				fi;
				reduction:=false;
			fi;
		od;
	od;
	Info(InfoNumSgps,2,"Reducing...");
	reduction:=false;
	while not reduction do
		reduction:=true;
		a:=First(gM, x->x<>reduce(A,Difference(gM,[x]),x));
		if a<>fail then
			rd:=reduce(A,Difference(gM,[a]),a);
			if IsZero(rd) then
				gM:=Difference(gM,[a]);
				Info(InfoNumSgps,2,"Removing redundant:",a);
			else
				gM:=Union(Difference(gM,[a]),[rd]);
				Info(InfoNumSgps,2,"Replacing ",a," by ",rd);
			fi;
			reduction:=false;
		fi;
	od;
	return gM;
end);

##################################################################
##
#F GeneratorsKahlerDifferentials(A)
##
## A synonym for GeneratorsModule_Global(A,M), with M the set of
## derivatives of the elements in A
##################################################################
InstallGlobalFunction(GeneratorsKahlerDifferentials, function(A)
	local M, t;

	if not(IsHomogeneousList(A)) then
			Error("The argument must be a list of polynomials.");
	fi;


	if not(ForAll(A, IsUnivariatePolynomial)) then
			Error("The argument must be list of polynomials.");
	fi;
	if Length(Set(A, IndeterminateOfLaurentPolynomial))<>1 then
			Error("The argument must be a list of polynomials in the same variable; constants not allowed nor the empty list.");
	fi;

	t:=IndeterminateNumberOfLaurentPolynomial(A[1]);


	M:=List(A,p->Derivative(p,t));
	return GeneratorsModule_Global(A,M);
end);


##################################################################
##
#O CyclotomicExponentSequence(s,k)
##
## s is a numerical semigroup and k a positive integer, the 
## output is the list of the first k elements of the cyclotomic
## exponent sequence of s (see [C-GS-M]).
## The sequence will be truncated if the semigroup is cyclotomic
## and k is bigger than the last nonzero element in its sequence.
##################################################################
InstallMethod(CyclotomicExponentSequence,[IsNumericalSemigroup, IsPosInt],

function(s,k)
    local invs, i, sec, e, n, left, right,p, x;
		
		x:=Indeterminate(Rationals,"x");
		p:=NumericalSemigroupPolynomial(s,x);
    sec :=[];
		left:=1;
		right:=p;
    for i in [1..k] do
        invs:=p mod x^(i+2);
        for n in [1..i-1] do
            invs:=(invs*PowerMod((1-x^n),sec[n],x^(i+2))) mod x^(i+2);
        od;
        e:=CoefficientsOfUnivariatePolynomial(invs+x^(i+2))[i+1];
				if e>0 then 
					right:=right*(1-x^i)^e;
				else
					left:=left*(1-x^i)^-e;
				fi;
        sec[i]:=e;    

        if left = right then
            return -sec;
        fi; 
    od;  
    return -sec;
end

);

##################################################################
##
#O WittCoefficients(p,k)
##
## p is a univariate polynomial with integer coefficientas and 
## p(1)=1. Then p(x)=\prod_{n\ge 0}(1-x^n)^{e_n}.
## output is the list [e_1,..,e_k], and it is computed bu using 
## [C-GS-HP-M]
##################################################################
InstallMethod(WittCoefficients,[IsUnivariatePolynomial, IsPosInt],
function(p,k)
    local e, s, divisors, j, a;

    a:=CoefficientsOfUnivariatePolynomial(p);
    if a[1]<> 1 then
        Error("The value of the first argument in 1 must be 1");
    fi;
    
    if not(IsSubset(Integers,a)) then
        Error("The firs argument must be a polynomial with integer coefficients");
    fi;

    a:=Concatenation(a{[2..Length(a)]},List([Length(a)-1..k],_->0));
    s:=[-a[1]];
    for j in [2..k] do
        s[j]:=-Reversed(a{[1..j-1]})*s{[1..j-1]}-j*a[j];
    od;

    e:=[];
    for j in [1..k] do
        divisors:=DivisorsInt(j);
        e[j]:=1/j*s{divisors}*List(divisors, i->MoebiusMu(j/i));
    od;

    return e;
end);

##################################################################
##
#F LegendrianGenericNumericalSemigroup(n,m)
## n and m are coprime integers with m>=2n+1. The output is the 
## semigroup of a generic element in the class of irreducible 
## Legendrian singularities with equisingularity equial to the 
## topological type of y^n=x^m. 
################################################################
InstallGlobalFunction(LegendrianGenericNumericalSemigroup,
function(n,m)
    local gamma, i, ni, wi, c, s, li, smg;

    if not(IsPosInt(n) and IsPosInt(m) and Gcd(n,m)=1 and m>2*n) then
        Error("The arguments are two positive coprime integers such that the second is larger than twice the first");
    fi;

    s:=NumericalSemigroup(n,m-n);
    c:=Conductor(s);
    gamma:=MultipleOfNumericalSemigroup(NumericalSemigroup(1),n,c);
    i:=0;
    while(i<>m-n) do
        i:=Maximum(Difference(s,gamma));
        smg:=SmallElements(gamma);
        li:=Difference([i..Conductor(gamma)], smg);
        ni:=Minimum(NrRestrictedPartitions(i,[n,m,m-n]),Length(li));
        wi:=li[ni];
        gamma:=NumericalSemigroupBySmallElements(Union(smg,[i..wi]));
    od;

    return gamma;
end);