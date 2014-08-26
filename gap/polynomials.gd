#############################################################################
##
#W  arf-med.gi              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#H  @(#)$Id: arf-med.gi,v 0.98 $
##
#Y  Copyright 2014 by Manuel Delgado and Pedro Garcia-Sanchez 
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
#F IsCyclotomicNumericalSemigroup(s)
## Checks if the polynomial fo s is Kronecker
###########################################
DeclareGlobalFunction("IsCyclotomicNumericalSemigroup");

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


