#############################################################################
##
#W  presentaciones.gd       Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: presentaciones.gd,v 0.971 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
##
#F  FortenTruncatedNCForNumericalSemigroups(l)
##
##  l contains the list of coefficients of a 
##  single linear equation. FortenTruncatedNCForNumericalSemigroups
##  gives a minimal generator 
##  of the affine semigroup of nonnegative solutions of this equation
##  with the first coordinate equal to one.
##
##  Used for computing minimal presentations.
##
#############################################################################
DeclareGlobalFunction("FortenTruncatedNCForNumericalSemigroups");




#############################################################################
##
#F  GraphAssociatedToElementInNumericalSemigroup(n,s)
##
##  Computes the graph associated to the element n
##  the numerical semigroup s.
##  Its vertices are those minimal generators m such that
##      n-m in s
##  Its edges are those pairs (m1,m2) of minimal generators
##      such that n-(m1+m2) in s.
#############################################################################
DeclareGlobalFunction("GraphAssociatedToElementInNumericalSemigroup");




#############################################################################
##
#F  MinimalPresentationOfNumericalSemigroup(s)
##
##  For a numerical semigroup s, give a minimal presentation
##  the output is a list of pairs showing the relationship
##  between the minimal generators of s
##  the algorithm is the one given in 
##  -J. C. Rosales, {\em An algorithmic method to compute a minimal
##  relation for any numerical semigroup}, Internat. J. Algebra Comput. 
##  {\bf 6} (1996), no. 4, 441--455.
#############################################################################
DeclareGlobalFunction("MinimalPresentationOfNumericalSemigroup");

#############################################################################
##
#F  BettiElementsOfNumericalSemigroup(s)
##
##  For a numerical semigroup s, returns the elements whose associated graphs
##  are non-connected, or in other words, whose factorizations are used to 
##  construct any minimal presentation for s
##
#############################################################################
DeclareGlobalFunction("BettiElementsOfNumericalSemigroup");
