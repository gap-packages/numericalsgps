#############################################################################
##
#W  elements.gd             Manuel Delgado <mdelgado@fc.up.pt>
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
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Returns the list of elements in the numerical semigroup S,
##  not greater than the Frobenius number + 1.
##
#############################################################################
DeclareAttribute( "SmallElementsOfNumericalSemigroup", IsNumericalSemigroup);
#############################################################################
##
#F  SmallElements(S)
##
##  If S is a numerical semigroup, then this function just passes the task of computing the minimal generating system to SmallElementsOfNumericalSemigroup
## If S is an ideal of numerical semigroup, then this function just passes the task of computing the minimal generating system to SmallElementsOfIdealOfNumericalSemigroup
##
#############################################################################
DeclareGlobalFunction("SmallElements");


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

#############################################################################
##
#F  WilfNumberOfNumericalSemigroup(S)
##
##  Let c,edim and se be the conductor, embedding dimension and number of
##  elements smaller than c in S. Returns the edim*se-c, which was conjetured
##  by Wilf to be nonnegative.
##
#############################################################################
DeclareGlobalFunction("WilfNumberOfNumericalSemigroup");


#############################################################################
##
#F  TruncatedWilfNumberOfNumericalSemigroup(S)
##
##  Returns W_0(S) (see [E])
##
#############################################################################
DeclareGlobalFunction("TruncatedWilfNumberOfNumericalSemigroup");


#############################################################################
##
#F  ProfileOfNumericalSemigroup(S)
##
##  Returns the profile of a numerical semigroup (see [E])
##
#############################################################################
DeclareGlobalFunction("ProfileOfNumericalSemigroup");

#############################################################################
##
#F  EliahouSliceOfNumericalSemigroup(S)
##
##  Returns a list of lists of integers, each list is the set of elements in 
##  S belonging to [jm-r, (j+1)m-r[ where m is the mulitiplicity of S,
##  and j in [1..q-1]; with q,r such that c=qm-r, c the conductor of S 
##  (see [E])
##
#############################################################################
DeclareGlobalFunction("EliahouSliceOfNumericalSemigroup");


