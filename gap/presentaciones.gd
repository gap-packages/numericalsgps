#############################################################################
##
#W  presentaciones.gd       Manuel Delgado <mdelgado@fc.up.pt>
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
DeclareOperation("MinimalPresentation",[IsNumericalSemigroup]);

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
DeclareOperation("BettiElements", [IsNumericalSemigroup]);

#############################################################################
##
#P  IsUniquelyPresentedNumericalSemigroup(s)
##
##  For a numerical semigroup s, checks it it has a unique minimal presentation
##  Basado en GS-O
##
#############################################################################
DeclareProperty("IsUniquelyPresented", IsNumericalSemigroup);
DeclareSynonymAttr("IsUniquelyPresentedNumericalSemigroup", IsUniquelyPresented);

#############################################################################
##
#P  IsGenericNumericalSemigroup(s)
##
##  For a numerical semigroup s, checks it it has a generic presentation,
##  that is, in every relation all minimal generators appear. These semigroups are uniquely
##  presented véase B-GS-G.
##
#############################################################################
DeclareProperty("IsGeneric", IsNumericalSemigroup);
DeclareSynonymAttr("IsGenericNumericalSemigroup", IsGeneric);

#############################################################################
##
#F ShadedSetOfElementInNumericalSemigroup(x,s)
## computes the shading set of x in s as defined in
##  -Székely, L. A.; Wormald, N. C. Generating functions for the Frobenius
##    problem with 2 and 3 generators. Math. Chronicle 15 (1986), 49–57.
#############################################################################
DeclareGlobalFunction("ShadedSetOfElementInNumericalSemigroup");

############################################################################
##
#F  DegreesOfPrimitiveElementsOfNumericalSemigroup(s)
##
## Computes the sets of elements in s, such that there exists a minimal
## solution to msg*x-msg*y = 0,  such that x,y are factorizations of s
##
#############################################################################
DeclareGlobalFunction("DegreesOfPrimitiveElementsOfNumericalSemigroup");
