#############################################################################
##
#W  arf-med.gi              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: arf-med.gi,v 0.98 $
##
#Y  Copyright 2005 by Manuel Delgado,
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
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
	
	gaps:=GapsOfNumericalSemigroup(s);
	p:=List(gaps, g-> x^g);
	return 1+(x-1)*Sum(p);
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
#########################################################################
InstallGlobalFunction(IsKroneckerPolynomial,function(f)
	if LeadingCoefficient(f)<>1 then 
		return false;
	fi;
	return ForAll(Factors(f),IsCyclotomicPolynomial);	
end);


###########################################
##
#F IsCyclotomicNumericalSemigroup(s)
## Checks if the polynomial fo s is Kronecker
###########################################
InstallGlobalFunction(IsCyclotomicNumericalSemigroup,function(s)
	local x, f;
	x:=X(Rationals,"x");
	f:=NumericalSemigroupPolynomial(s,x);
	return IsKroneckerPolynomial(f);# ForAll(Factors(f),IsCyclotomicPolynomial);
end);

#####################################################
## 
#F IsSelfReciprocalUnivariatePolynomial(p)
## Checks if the univariate polynomial p is selfreciprocal
#####################################################
InstallGlobalFunction(IsSelfReciprocalUnivariatePolynomial,function(p)
	local x, d;

	if not(IsUnivariatePolynomial(p)) then 
		Error("The argument must be a polynomial\n");
	fi;
	
	x:=IndeterminateOfLaurentPolynomial(p);
	d:=DegreeOfUnivariateLaurentPolynomial(p);
	return p=x^d*Value(p,1/x);

end);


