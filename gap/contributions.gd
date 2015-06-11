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
#F  IsGradedAssociatedRingNumericalSemigroupBuchsbaum(S)
##
##  Test for the Buchsbaum property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Mezzasalma, M. and Micale, V. "On the Buchsbaumness of the Associated Graded Ring 
##  of a One-Dimensional Local Ring", Communications in Algebra, 37: 5, 1594 — 1603
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsGradedAssociatedRingNumericalSemigroupBuchsbaum");

##############################################################################################################
##
#F  IsMpureNumericalSemigroup(S)
##
##  Test for the M-Purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated 
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsMpureNumericalSemigroup");

##############################################################################################################
##
#F  IsPureNumericalSemigroup(S)
##
##  Test for the purity of the numerical semigroup S
##  Based on L. Bryant, "Goto Numbers of a Numerical Semigroup Ring and the Gorensteiness of Associated 
##  Graded Rings", Comm. Algebra 38 (2010), 2092--2128.
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsPureNumericalSemigroup");

##############################################################################################################
##
#F  IsGradedAssociatedRingNumericalSemigroupGorenstein(S)
##
##  Test for the Gorenstein property of the associated graded ring of a numerical semigroup ring
##  Based on D'Anna, M., Micale, V. and Sammartano, A. "On the Associated Ring of a Semigroup Ring", 
##  preprint
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsGradedAssociatedRingNumericalSemigroupGorenstein");
##############################################################################################################
## the functions below first appeared in version 0.98
##############################################################################################################
##
#F  IsGradedAssociatedRingNumericalSemigroupCI
##
##  Test for the Complete Intersection property of the associated graded ring of a numerical semigroup ring k[[S]]
##  Based on "When the associated graded ring of a semigroup ring is Complete Intersection"
## 
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsGradedAssociatedRingNumericalSemigroupCI");
##############################################################################################################
##
#F  IsAperySetGammaRectangular
##
##  Test for the Gamma-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsAperySetGammaRectangular");
##############################################################################################################
##
#F  IsAperySetBetaRectangular
##
##  Test for the Beta-Rectangularity of the Apéry Set of a numerical semigroup
##  Based on "Classes Of Complete Intersection Numerical Semigroups"
##  Marco D'Anna, Vincenzo Micale, Alessio Sammartano
##
##  Implemented by Alessio Sammartano
##
##############################################################################################################
DeclareGlobalFunction("IsAperySetBetaRectangular");
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
DeclareGlobalFunction("IsAperySetAlphaRectangular");
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

#############################################################################
##
#F  OmegaPrimalityOfElementListInNumericalSemigroup(l,s)
##
##  Computes the omega primality of a list of elmenents l in S, 
##  Implemented by Chris O'Neill.  
##
#############################################################################
DeclareGlobalFunction("OmegaPrimalityOfElementListInNumericalSemigroup");

#############################################################################
##
#F  FactorizationsElementListWRTNumericalSemigroup(l,s)
##
##  Computes the factorizations of a list of elmenents l in S, 
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
        
