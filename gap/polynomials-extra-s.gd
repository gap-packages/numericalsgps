#############################################################################
##
#W  polynomials-extra-s.gi  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2016 by Manuel Delgado and Pedro Garcia-Sanchez
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#################################################################
##
# F SemigroupOfValuesOfPlaneCurve(f)
##  Computes the semigroup of values {mult(f,g) | g curve} of a plane curve
##   with one place at the infinity in the variables X(Rationals,1) and X(Rationals,2)
##  This function needs NumSgpsCanUseSingular to be enabled, either by loading the
##  package singular prior to numericalsgps, or by using NumSgpsUseSingular. 
##  The function makes use of `semigroup` in the `alexpoly` singular library
##
#################################################################
DeclareGlobalFunction("SemigroupOfValuesOfPlaneCurve");
