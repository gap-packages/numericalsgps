#############################################################################
##
#W  contributions.gd
##
##
#Y  The functions in this file have been implemented by researchers that do
#Y  not appear as authors of the package. References to its usage should be
#Y made as suggested in the manual
#Y
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
########################################################################################################################
###################################################################
##
#P  IsGradedAssociatedRingNumericalSemigroupBuchsbaum(S)
##
##  Test for the Buchsbaum property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Mezzasalma, M. and Micale, V. "On the Buchsbaumness of the Associated Graded Ring
##  of a One-Dimensional Local Ring", Communications in Algebra, 37: 5, 1594 — 1603
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsGradedAssociatedRingNumericalSemigroupBuchsbaum", IsNumericalSemigroup);

##############################################################################################################
##
#P  IsMpureNumericalSemigroup(S)
##
##  Test for the M-Purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsMpure",IsNumericalSemigroup);
DeclareSynonymAttr("IsMpureNumericalSemigroup",IsMpure);

##############################################################################################################
##
#P  IsPureNumericalSemigroup(S)
##
##  Test for the purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsPure",IsNumericalSemigroup);
DeclareSynonymAttr("IsPureNumericalSemigroup",IsPure);

##############################################################################################################
##
#P  IsGradedAssociatedRingNumericalSemigroupGorenstein(S)
##
##  Test for the Gorenstein property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Micale, V. and Sammartano, A. "On the Associated Ring of a Semigroup Ring",
##  preprint
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsGradedAssociatedRingNumericalSemigroupGorenstein",IsNumericalSemigroup);

##############################################################################################################
##
#P  IsGradedAssociatedRingNumericalSemigroupCI
##
##  Test for the Complete Intersection property of the associated graded ring of a numerical semigroup ring k[[S]]
##  Based on "When the associated graded ring of a semigroup ring is Complete Intersection"
##
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsGradedAssociatedRingNumericalSemigroupCI",IsNumericalSemigroup);

##############################################################################################################
##
#P  IsAperySetGammaRectangular
##
##  Test for the Gamma-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsAperySetGammaRectangular",IsNumericalSemigroup);

##############################################################################################################
##
#P  IsAperySetBetaRectangular
##
##  Test for the Beta-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsAperySetBetaRectangular", IsNumericalSemigroup);

##############################################################################################################
##
#F  IsAperySetAlphaRectangular
##
##  Test for the Alpha-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareProperty("IsAperySetAlphaRectangular", IsNumericalSemigroup);

##############################################################################################################
##
#F  TypeSequenceOfNumericalSemigroup
##
##  Computes the type sequence of a numerical semigroup
##  Based on "Maximality properties in numerical semigroups and applications to one-dimensional analytically irreducible local domains"
##  V. Barucci, D. E. Dobbs, M. Fontana
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("TypeSequenceOfNumericalSemigroup");
DeclareOperation("TypeSequence",[IsNumericalSemigroup]);

##########################################################
##
#F TorsionOfAssociatedGradedRingNumericalSemigroup(S)
## This function returns the set of elements in the numerical
## semigroup S corresponding to a K-basis of the torsion
## submodule of the associated graded ring of the numerical
## semigroup ring K[[S]]. It uses the Apery table
## as explained in [Benitez, Jafari, Zarzuela; Semigroup Forum, 2013]
##
## Implemented by A. Sammartano
###########################################################
DeclareGlobalFunction("TorsionOfAssociatedGradedRingNumericalSemigroup");

#################################################################################
##
#F BuchsbaumNumberOfAssociatedGradedRingNumericalSemigroup(S)
## This function returns the smallest non-negative integer k for which the
## associated graded ring G of a given numerical semigroup ring is k-Buchsbaum,
## that is, the least k for which the torsion submodule of G is annihilated by
## the k-th power of the homogeneous maximal ideal of G.
##
##  Implemented by A. Sammartano
##################################################################################
DeclareGlobalFunction("BuchsbaumNumberOfAssociatedGradedRingNumericalSemigroup");


#############################################################################
##
#F  OmegaPrimalityOfElementListInNumericalSemigroup(l,s)
##
##  Computes the omega primality of a list of elements l in S,
##  Implemented by Chris O'Neill.
##
#############################################################################
DeclareGlobalFunction("OmegaPrimalityOfElementListInNumericalSemigroup");

#############################################################################
##
#F  FactorizationsElementListWRTNumericalSemigroup(l,s)
##
##  Computes the factorizations of a list of elements l in S,
##  Implemented by Chris O'Neill
##
#############################################################################
DeclareGlobalFunction("FactorizationsElementListWRTNumericalSemigroup");

#############################################################################
##
#F  DeltaSetPeriodicityBoundForNumericalSemigroup(s)
##
##  Returns a bound on the start of periodic behavior for the delta sets of elements of S.
##  Implemented by Chris O'Neill
##
#############################################################################
DeclareGlobalFunction("DeltaSetPeriodicityBoundForNumericalSemigroup");

#############################################################################
##
#F  DeltaSetPeriodicityStartForNumericalSemigroup(n,s)
##
##  Returns the exact start of periodicity for the delta sets of elements of S.
##  Implemented by Chris O'Neill
##
#############################################################################
DeclareGlobalFunction("DeltaSetPeriodicityStartForNumericalSemigroup");

#############################################################################
##
#F  DeltaSetListUpToElementWRTNumericalSemigroup(n,s)
##
##  Computes the delta sets of the elements of S up to and including n.
##  Implemented by Chris O'Neill
##
#############################################################################
DeclareGlobalFunction("DeltaSetListUpToElementWRTNumericalSemigroup");

#############################################################################
##
#F  DeltaSetUnionUpToElementWRTNumericalSemigroup(n,s)
##
##  Computes the union of the delta sets of the elements of S up to and including n,
##  using a ring buffer to conserve memory.
##  Implemented by Chris O'Neill
##
#############################################################################
DeclareGlobalFunction("DeltaSetUnionUpToElementWRTNumericalSemigroup");

#############################################################################
##
#F  DeltaSetOfNumericalSemigroup(s)
##
##  Computes the union of the delta sets of the elements of S up to the bound given in [TODO],
##  Implemented by Chris O'Neill
##
#############################################################################
DeclareGlobalFunction("DeltaSetOfNumericalSemigroup");
DeclareOperation("DeltaSet",[IsNumericalSemigroup]);

#############################################################################
##
#F  IsAdmissiblePattern(p)
##
##  p is the list of integers that are the coefficients of a pattern
##  returns true or false depending if p is admissible or not
##  see cite [BA-GS]
##
##  Implemented by Klara Stokes
##
#############################################################################
DeclareGlobalFunction("IsAdmissiblePattern");

#############################################################################
##
#F  IsStronglyAdmissiblePattern(p)
##
##  p is the list of integers that are the coefficients of a pattern
##  returns true or false depending if p is strongly admissible or not
##  see cite [BA-GS]
##
#############################################################################
DeclareGlobalFunction("IsStronglyAdmissiblePattern");

#############################################################################
##
#F  AsIdealOfNumericalSemigroup(I,T)
##  For an ideal I of a numerical semigroup S, and a numerical semigroup T,
##  detects if I is an ideal of T, and if so, returns I as an ideal of T
##  (otherwise it returns fail)
##
##  Implented with Klara Stokes
##
#############################################################################
DeclareGlobalFunction("AsIdealOfNumericalSemigroup");

#############################################################################
##
#F  BoundForConductorOfImageOfPattern(p, C)
##  Takes an admissible pattern p and calculates an upper bound of the
##  smallest element K in p(I) such that all integers larger than K is
##  contained in p(I), where I is an ideal of a numerical semigroup.
##  Instead of taking I as parameter, the function takes C, which is assumed
##  to be the smallest element in I such that all integers larger than C is
##  contained in I.
##
##  Implemented by Klara Stokes
##
#############################################################################
DeclareGlobalFunction("BoundForConductorOfImageOfPattern");

#############################################################################
##
#F ApplyPatternToIdeal(p,I)
## Takes a strongly  admissible pattern p and calculates p(I), where I is
## an ideal of a numerical semigroup
##
## Implemented by Klara Stokes
##
#############################################################################
DeclareGlobalFunction("ApplyPatternToIdeal");

#############################################################################
##
#F ApplyPatternToNumericalSemigroup(p,S)
## Takes a strongly  admissible pattern p and calculates p(S), where S is
## a numerical semigroup
##
## Implemented by Klara Stokes (see [Stokes])
##
#############################################################################
DeclareGlobalFunction("ApplyPatternToNumericalSemigroup");

#############################################################################
##
#F  IsAdmittedPatternByIdeal(p,I,J)
##
##  Takes astrongly admissible pattern p and tests whether p(I) is
##  contained in J, for I and J ideals of numerical semigroups
##  (not necessarily the same one)
##
##  Implemented by Klara Stokes
##
#############################################################################
DeclareGlobalFunction("IsAdmittedPatternByIdeal");

#############################################################################
##
#F  IsAdmittedPatternByNumericalSemigroup(p,S,T)
##  Takes a strongly  admissible pattern p and tests whether p(S) is
##  contained in T, for S and T numerical semigroups.
##
##  Implemented by Klara Stokes
##
#############################################################################
DeclareGlobalFunction("IsAdmittedPatternByNumericalSemigroup");

##############################################################################################################
##
#P  IsHomogeneousNumericalSemigroup(S)
##
##  Tests if S is homogeneous, that is, for every element a in the Apéry set of its multiplicity,
##  all the factorizations of a have the same length
##  Implemented by Francesco Strazzanti
##
##############################################################################################################
DeclareProperty("IsHomogeneousNumericalSemigroup",IsNumericalSemigroup);
