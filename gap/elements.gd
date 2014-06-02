#############################################################################
##
#W  elements.gd             Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: elements.gd,v 0.98 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Returns the list of elements in the numerical semigroup S,
##  not greater than the Frobenius number + 1.
##
#############################################################################
DeclareAttribute( "SmallElementsOfNumericalSemigroup", IsNumericalSemigroup);


#############################################################################
##
#A  GapsOfNumericalSemigroup(S)
##
##  Returns the list of the gaps of the numerical semigroup S.
##
#############################################################################
DeclareAttribute( "GapsOfNumericalSemigroup", IsNumericalSemigroup);

#############################################################################
##
#F  GenusOfNumericalSemigroup(S)
##
##  Returns the number of gaps of the numerical semigroup S.
##
#############################################################################
DeclareGlobalFunction("GenusOfNumericalSemigroup");
