#############################################################################
##
#W  contributions.gd          
##
##
#H  @(#)$Id: contributions.gd,v 0.971 $
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
