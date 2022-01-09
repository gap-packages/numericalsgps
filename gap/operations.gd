#############################################################################
##
#W  operations.gd           Manuel Delgado <mdelgado@fc.up.pt>
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
#F  QuotientOfNumericalSemigroup(S,p)
##
##  Computes S/p, where S is a numerical semigroup
##  and p a positive integer.
##
#############################################################################
DeclareGlobalFunction( "QuotientOfNumericalSemigroup" );

############################################################
##
#F MultipleOfNumericalSemigroup(s,a,b)
## s is a numerical semigroup; a and b are positive integers
## Computes a*s \cup [b,\infty)
##
############################################################
DeclareGlobalFunction("MultipleOfNumericalSemigroup");

#################################################################
##
#F InductiveNumericalSemigroup(a,b)
## a and b are lists of positive integers with b[i+1]\ge a[i]b[i]
## Computes inductively the semigroup
## S_0=N
## S_i=a_iS_{i-1}\cup \{a_ib_i,a_ib_i+1,->\}
## and outputs S_n, with n the length of a and b
##
##################################################################
DeclareGlobalFunction("InductiveNumericalSemigroup");

#############################################################################
##
#F  DilatationOfNumericalSemigroup(S,a)
##
##  Computes {0}\cup{a+s |s in S\{0}}; a must be in M-2M, 
##  with M the maximal ideal of S 
##
#############################################################################
DeclareGlobalFunction( "DilatationOfNumericalSemigroup" );
