#############################################################################
##
#W  pseudoFrobenius.gd          Manuel Delgado <mdelgado@fc.up.pt>
#W                              Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
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
#F FurtherForcedElementsForPseudoFrobenius(f_gaps,f_elts,PF)
##
###########################################################################
DeclareGlobalFunction("FurtherForcedElementsForPseudoFrobenius");
###########################################################################
##
#F FurtherForcedGapsForPseudoFrobenius(f_gaps,f_elts,PF)
##
###########################################################################
DeclareGlobalFunction("FurtherForcedGapsForPseudoFrobenius");
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
#F ANumericalSemigroupWithPseudoFrobeniusNumbers
##
###########################################################################
DeclareGlobalFunction("ANumericalSemigroupWithPseudoFrobeniusNumbers");
