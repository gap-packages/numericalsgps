#############################################################################
##
#W  basics2.gd              Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: basics2.gd,v 0.98 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#O  IsSubsemigroupOfNumericalSemigroup(S,T)
## 
##  Test whether the numerical semigroup T is contained in the 
##  numerical semigroup S
##
#############################################################################
DeclareOperation( "IsSubsemigroupOfNumericalSemigroup",[IsNumericalSemigroup,IsNumericalSemigroup]);


#############################################################################
##
#F  IntersectionOfNumericalSemigroups(S,T)
## 
##  Returns the intersection of the numerical
##  semigroups S and T.
##
#############################################################################
DeclareGlobalFunction( "IntersectionOfNumericalSemigroups" );




#############################################################################
##
#F  RepresentsGapsOfNumericalSemigroup(L)
##
##  Tests if the given list L represents the gaps of
##  some numerical semigroup.
##
#############################################################################
DeclareGlobalFunction("RepresentsGapsOfNumericalSemigroup");


#############################################################################
##
#F  NumericalSemigroupsWithFrobeniusNumber(g)
##
##  Computes the set of numerical semigroups with Frobenius number g.
##  The algorithm is based on 
##  "Fundamental gaps in numerical semigroup".
##
#############################################################################
DeclareGlobalFunction("NumericalSemigroupsWithFrobeniusNumber");

##############################################################################
##
#F NumericalSemigroupsWithGenus 
##computes the set of numerical semigroups with genus g,
# that is, numerical semigroups with exactly g gaps
#
#
# numerical semigroups are encoded in lists containing the apery set with
# respect to the multiplicity removing the zero element. The multiplicity
# is thus the lenght of the list plus one. In this way deciding membership 
# to a numerical semigroup is straightforward (belongs). The computation of
# the Frobenius number is performed using Selmer's idea (frob). Removing a new
# generator is easy (removegen), as well as computing those minimal generators 
# greater than the Frobenius number (minimalgeneratorsf). 
# Given a numerical semigroup of genus g, removing minimal generators, one 
# obtains numerical semigroups of genus g+1. In order to avoid repetitions,
# we only remove minimal generators greater than the frobenius number of 
# the numerical semigroup (this is accomplished with the local function sons).
# References: 
# -J. C. Rosales, P. A. García-Sánchez, J. I. García-García and 
#  J. A. Jimenez-Madrid, The oversemigroups of a numerical semigroup. 
#  Semigroup Forum 67 (2003), 145--158.
# -M. Bras-Amorós, Fibonacci-like behavior of the number of numerical 
#  semigroups of a given genus. Semigroup Forum 76 (2008), 379--384.
##
#############################################################################
DeclareGlobalFunction("NumericalSemigroupsWithGenus");
