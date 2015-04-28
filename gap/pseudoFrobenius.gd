#############################################################################
##
#W  pseudoFrobenius.gd          Manuel Delgado <mdelgado@fc.up.pt>
#W                              Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright .........
#############################################################################
###################################################
#F NumericalSemigroupWithGivenElementsAndFrobenius(elts,frob)
##
###########################################################################
DeclareGlobalFunction("NumericalSemigroupWithGivenElementsAndFrobenius");

###########################################################################
## Some auxiliary functions to be called by the functions below
###########################################################################
##
#F StartingForcedGapsForPseudoFrobenius(PF)
##
###########################################################################
DeclareGlobalFunction("StartingForcedGapsForPseudoFrobenius");
###########################################################################
##
#F ElementsForPseudoFrobenius(f_gaps,f_elts,PF)
##
###########################################################################
DeclareGlobalFunction("ElementsForPseudoFrobenius");
###########################################################################
##
#F GapsForPseudoFrobenius(f_gaps,f_elts,PF)
##
###########################################################################
DeclareGlobalFunction("GapsForPseudoFrobenius");
###########################################################################
##
#F SimpleForcedIntegersForPseudoFrobenius
##
###########################################################################
DeclareGlobalFunction("SimpleForcedIntegersForPseudoFrobenius");
###########################################################################
##
#F NonAdmissibleForPseudoFrobenius(f_gaps,f_elts,PF)
##
###########################################################################
DeclareGlobalFunction("NonAdmissibleForPseudoFrobenius");
###########################################################################
##
#F ForcedIntegersForPseudoFrobenius
##
###########################################################################
DeclareGlobalFunction("ForcedIntegersForPseudoFrobenius");
DeclareGlobalFunction("ForcedIntegersForPseudoFrobenius_QV");

######################################
##
#F NumericalSemigroupsWithPseudoFrobeniusNumbers
##
###########################################################################
DeclareGlobalFunction("NumericalSemigroupsWithPseudoFrobeniusNumbers");
#DeclareGlobalFunction("NumericalSemigroupsWithPseudoFrobeniusNumbers_QV");
  
######################################
##
#F RandomNumericalSemigroupWithPseudoFrobeniusNumbers
##
###########################################################################
DeclareGlobalFunction("RandomNumericalSemigroupWithPseudoFrobeniusNumbers");
