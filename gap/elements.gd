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
#F  ElementsUpTo(S,b)
##
##  Returns the elements of S up to the positive integer b
##
#############################################################################
DeclareGlobalFunction("ElementsUpTo");

#############################################################################
##
#A  SmallElements(S)
#A  SmallElementsOfNumericalSemigroup(S)
##
##  Returns the list of elements in the numerical semigroup S,
##  not greater than the Frobenius number + 1.
##
#############################################################################
#DeclareAttribute( "SmallElements", IsNumericalSemigroup);
DeclareSynonymAttr( "SmallElementsOfNumericalSemigroup", SmallElements);
#############################################################################
#A Length
##
## The number of left elements of the semigroup (the elements up to the conductor)
#############################################################################
DeclareAttribute( "Length", IsNumericalSemigroup);

#############################################################################
##
#F  SmallElements(S)
##
##  If S is a numerical semigroup, then this function just passes the task of computing the minimal generating system to SmallElementsOfNumericalSemigroup
## If S is an ideal of numerical semigroup, then this function just passes the task of computing the minimal generating system to SmallElementsOfIdealOfNumericalSemigroup
##
#############################################################################
##DeclareGlobalFunction("SmallElements");


#############################################################################
##
#A  Gaps(S)
#A  GapsOfNumericalSemigroup(S)
##
##  Returns the list of the gaps of the numerical semigroup S.
##
#############################################################################
#DeclareAttribute( "Gaps", IsNumericalSemigroup);
DeclareSynonymAttr( "GapsOfNumericalSemigroup", Gaps);

#############################################################################
##
#A  Weight(S)
##
##  Returns the sum of all  gaps of the numerical semigroup S.
##
#############################################################################
DeclareAttribute( "Weight", IsNumericalSemigroup);


#############################################################################
##
#F  DesertsOfNumericalSemigroup(S)
##
##  Returns the lists of runs of gaps of the numerical semigroup S
##
#############################################################################
DeclareGlobalFunction("DesertsOfNumericalSemigroup");
DeclareOperation("Deserts",[IsNumericalSemigroup]);

#############################################################################
##
#A  GenusOfNumericalSemigroup(S)
##
##  Returns the number of gaps of the numerical semigroup S.
##
#############################################################################
DeclareAttribute( "Genus", IsNumericalSemigroup);
DeclareSynonymAttr( "GenusOfNumericalSemigroup", Genus);

#############################################################################
##
#A  WilfNumberOfNumericalSemigroup(S)
##
##  Let c,edim and se be the conductor, embedding dimension and number of
##  elements smaller than c in S. Returns the edim*se-c, which was conjetured
##  by Wilf to be nonnegative.
##
#############################################################################
DeclareAttribute( "WilfNumber", IsNumericalSemigroup);
DeclareSynonymAttr( "WilfNumberOfNumericalSemigroup",WilfNumber);


#############################################################################
##
#A  TruncatedWilfNumberOfNumericalSemigroup(S)
#A  EliahouNumber
##
##  Returns W_0(S) (see [E])
##
#############################################################################
DeclareAttribute( "EliahouNumber", IsNumericalSemigroup);
DeclareSynonymAttr( "TruncatedWilfNumberOfNumericalSemigroup", EliahouNumber);


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
#F  EliahouSlicesOfNumericalSemigroup(S)
##
##  Returns a list of lists of integers, each list is the set of elements in
##  S belonging to [jm-r, (j+1)m-r[ where m is the mulitiplicity of S,
##  and j in [1..q-1]; with q,r such that c=qm-r, c the conductor of S
##  (see [E])
##
#############################################################################
DeclareGlobalFunction("EliahouSlicesOfNumericalSemigroup");


#########################################################
##
#F LatticePathAssociatedToNumericalSemigroup(s,p,q)
##
## s is a numerical semigroup, and p,q are elements in s
## Then s is an oversemigroup of <p,q> and all its gaps
## are gaps of <p,q>. If c is the conductor of <p,q>,
## every gap g in <p,q> is expressed uniquely as
## g=c-1-(ap+bq) for some nonnegative integers a and b,
## whence g has associated coordinates (a,b)
## The output is the path in N^2 such that every point
## in N^2 corresponding to a gap of <p,q> above the path
## correspond to gaps of s (see [K-W])
#########################################################
DeclareGlobalFunction("LatticePathAssociatedToNumericalSemigroup");
