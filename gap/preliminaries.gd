#############################################################################
##
#W  preliminaries.gd        Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
##
#F  BezoutSequence(arg)
##
##  Computes a Bezout sequence with ends two given rational numbers which may
##  be given as a list of two rationals.
##  Warning: rational numbers are silently transformed into 
##  irreducible fractions.
##
#############################################################################
DeclareGlobalFunction( "BezoutSequence" );



#############################################################################
##
#F  IsBezoutSequence(L)
##
##  Tests if a sequence is a Bezout sequence.
##
#############################################################################
DeclareGlobalFunction( "IsBezoutSequence" );



#############################################################################
##
#F  RepresentsPeriodicSubAdditiveFunction(L)
##
##  Tests whether a list L of length m represents a subadditive function f
##  periodic of period m. To avoid defining f(0) (which we assume to be 0) we 
##  define f(m)=0 and so the last element of the list must be 0.
##  This technical need is due to the fact that <position>
##  in a list must be positive (not a 0).
##
#############################################################################
DeclareGlobalFunction( "RepresentsPeriodicSubAdditiveFunction" );




#############################################################################
##
#F  RepresentsSmallElementsOfNumericalSemigroup(L)
##
##  Tests if a list (which has to be a set) may represent the "small" 
##  elements of a numerical semigroup.
##
#############################################################################
DeclareGlobalFunction( "RepresentsSmallElementsOfNumericalSemigroup" );



#############################################################################
##
#F  CeilingOfRational(R)
##
##  Computes the smallest integer greater than a rational r/s.
##
#############################################################################
DeclareGlobalFunction( "CeilingOfRational" );

#############################################################################
##
#F  IsListOfIntegersNS(list)
##
##  Tests whether L is a list integers.
##
#############################################################################
DeclareGlobalFunction("IsListOfIntegersNS");

#############################################################################
##
#F  IsAperyListOfNumericalSemigroup(L)
##
##  Tests whether a list may represent the Apery set of a numerical semigroup.
##
#############################################################################
DeclareGlobalFunction( "IsAperyListOfNumericalSemigroup" );


