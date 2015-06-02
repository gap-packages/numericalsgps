#############################################################################
##
#W  random.gd               Manuel Delgado <mdelgado@fc.up.pt>
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
#F  RandomListForNS(n,m)
##
##  Returns a set of length not greater than n of random integers in [1..m] 
##  whose GCD is 1.
##  It is used to create "random" numerical semigroups.
##
#############################################################################
DeclareGlobalFunction( "RandomListForNS" );


#############################################################################
##
#F  RandomNumericalSemigroup(n,m)
##
##  Returns a "random" numerical semigroup  with no more 
##  than n generators in [1..m].
##
#############################################################################
DeclareGlobalFunction( "RandomNumericalSemigroup" );



#############################################################################
##
#F  RandomListRepresentingSubAdditiveFunction(m, a)
##
##  Produces a list representing a subadditive function which is periodic
##  with period m (or less). When possible, the images are in [a..20*a].
##  (Otherwise, the list of possible images is enlarged.)
##
#############################################################################
DeclareGlobalFunction( "RandomListRepresentingSubAdditiveFunction" );




#############################################################################
##
#F  RandomProportionallyModularNumericalSemigroup(k)
##
##  Produces a "random" proportionally modular semigroup.
##
#############################################################################
DeclareGlobalFunction( "RandomProportionallyModularNumericalSemigroup" );



#############################################################################
##
#F  RandomModularNumericalSemigroup(k)
##
##  Produces a "random" modular semigroup.
##
#############################################################################
DeclareGlobalFunction( "RandomModularNumericalSemigroup" );
