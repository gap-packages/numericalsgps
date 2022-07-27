#############################################################################
##
#W  polynomials.gi              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2015 by Manuel Delgado and Pedro Garcia-Sanchez
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
DeclareGlobalFunction("NumericalSemigroupPolynomial");

##############################################################
##
#F IsNumericalSemigroupPolynomial(f) detects
## if there exists S a numerical semigroup such that P_S(x)=f
##
##############################################################
DeclareGlobalFunction("IsNumericalSemigroupPolynomial");

##############################################################
##
#F NumericalSemigroupFromNumericalSemigroupPolynomial(f) outputs
## a numerical semigroup S such that P_S(x)=f; error if no such
## S exists
##
##############################################################
DeclareGlobalFunction("NumericalSemigroupFromNumericalSemigroupPolynomial");

###################################################
#F HilbertSeriesOfNumericalSemigroup(s,x)
## Computes the Hilber series of s in x : \sum_{s\in S}x^s
###################################################
DeclareGlobalFunction("HilbertSeriesOfNumericalSemigroup");

######################################################
##
#F Computes the Graeffe polynomial of p
##  see for instance [BD-cyclotomic]
##
######################################################
DeclareGlobalFunction("GraeffePolynomial");

#####################################################
##
#F IsCyclotomicPolynomial(f) detects
## if f is a cyclotomic polynomial using the method explained in
## BD-cyclotomic
#####################################################
DeclareGlobalFunction("IsCyclotomicPolynomial");

########################################################################
##
#F IsKroneckerPolynomial(f) decides if
##   f is a Kronecker polynomial, that is,   a monic polynomial with integer coefficients
##   having all its roots in the unit circunference, equivalently, is a product of
##   cyclotomic polynomials
#########################################################################
DeclareGlobalFunction("IsKroneckerPolynomial");

###########################################
##
#P IsCyclotomicNumericalSemigroup(s)
## Checks if the polynomial fo s is Kronecker
###########################################
DeclareProperty("IsCyclotomicNumericalSemigroup",IsNumericalSemigroup);

#####################################################
##
#F IsSelfReciprocalUnivariatePolynomial(p)
## Checks if the univariate polynomial p is selfreciprocal
#####################################################
DeclareGlobalFunction("IsSelfReciprocalUnivariatePolynomial");

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
DeclareGlobalFunction("SemigroupOfValuesOfPlaneCurveWithSinglePlaceAtInfinity");

#########################################################################
##
#F IsDeltaSequence(l)
## tests whether or not l is a \delta-sequence (see for instancd [AGS14])
##
#########################################################################
DeclareGlobalFunction("IsDeltaSequence");

#########################################################
##
#F DeltaSequencesWithFrobeniusNumber(f)
##   Computes  the list of delta-sequences with Frobenius number f
##
#########################################################
DeclareGlobalFunction("DeltaSequencesWithFrobeniusNumber");

#####################################################
##
#F CurveAssociatedToDeltaSequence(l)
##  computes the curve associated to a delta-sequence l in
##  the variables X(Rationals,1) and X(Rationals,2)
##  as explained in [AGS14]
##
#####################################################
DeclareGlobalFunction("CurveAssociatedToDeltaSequence");


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
DeclareGlobalFunction("SemigroupOfValuesOfCurve_Global");


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
DeclareGlobalFunction("SemigroupOfValuesOfCurve_Local");

##################################################################
##
#F GeneratorsModule_Global(A,M)
##
## A and M are lists of polynomials in the same variable
## Computes a basis of the ideal MK[A], that is, a set F such that
## deg(F) generates the ideal deg(MK[A]) of deg(K[A]), where deg
## stands for degree
##################################################################
DeclareGlobalFunction("GeneratorsModule_Global");

##################################################################
##
#F GeneratorsKahlerDifferentials(A)
##
## A synonym for GeneratorsModule_Global(A,M), with M the set of
## derivatives of the elements in A
##################################################################
DeclareGlobalFunction("GeneratorsKahlerDifferentials");

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
DeclareOperation("CyclotomicExponentSequence",[IsNumericalSemigroup, IsPosInt]);

##################################################################
##
#O WittCoefficients(p,k)
##
## p is a univariate polynomial with integer coefficientas and 
## p(1)=1. Then p(x)=\prod_{n\ge 0}(1-x^n)^{e_n}.
## output is the list [e_1,..,e_k], and it is computed bu using 
## [C-GS-HP-M]
##################################################################
DeclareOperation("WittCoefficients",[IsUnivariatePolynomial, IsPosInt]);

##################################################################
##
#F LegendrianGenericNumericalSemigroup(n,m)
## n and m are coprime integers with m>=2n+1. The output is the 
## semigroup of a generic element in the class of irreducible 
## Legendrian singularities with equisingularity equial to the 
## topological type of y^n=x^m. 
################################################################
DeclareGlobalFunction("LegendrianGenericNumericalSemigroup");