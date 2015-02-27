#############################################################################
##
#W  pseudoFrobenius.gd          Manuel Delgado <mdelgado@fc.up.pt>
#W                              Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright .........
#############################################################################
###################################################
#F LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber(elts,frob)
##
###########################################################################
DeclareGlobalFunction("LeastNumericalSemigroupWithGivenElementsAndUpperBoundForFrobeniusNumber");

###################################################
#F GapsOfNumericalSemigroupForcedByGapsAndElements(f_gaps,elts)
##
###########################################################################
DeclareGlobalFunction("GapsOfNumericalSemigroupForcedByGapsAndElements");

###################################################
##
#F SomeConditionsForPseudoFrobenius(arg)
##
###########################################################################
DeclareGlobalFunction("SomeConditionsForPseudoFrobenius");

###################################################
##
#F ConditionsForPseudoFrobeniusBasedOnForcedIntegers(arg)
## 
###########################################################################
DeclareGlobalFunction("ConditionsForPseudoFrobeniusBasedOnForcedIntegers");   

###################################################
## Some auxiliary functions to be called by the functions below
###################################################
##
#F ObviouslyForcedGaps(PF)
##
###########################################################################
DeclareGlobalFunction("ObviouslyForcedGaps");
#######################################
##
#F NewBigElements(f_gaps,f_elts,PF)
##
###########################################################################
DeclareGlobalFunction("NewBigElements");
######################################
##
#F NewElementsByExclusion(f_gaps,f_elts,PF)
###########################################################################
DeclareGlobalFunction("NewElementsByExclusion");
######################################
##
#F SimpleForcedIntegers
##
###########################################################################
DeclareGlobalFunction("SimpleForcedIntegers");
  
######################################
##
#F ForcedGapsAndElementsForPseudoFrobenius
##
###########################################################################
DeclareGlobalFunction("ForcedGapsAndElementsForPseudoFrobenius");
  
######################################
##
#F NumericalSemigroupsWithPseudoFrobeniusNumbers
##
###########################################################################
DeclareGlobalFunction("NumericalSemigroupsWithPseudoFrobeniusNumbers");
  
######################################
##
#F RandomNumericalSemigroupWithPseudoFrobeniusNumbers
##
###########################################################################
DeclareGlobalFunction("RandomNumericalSemigroupWithPseudoFrobeniusNumbers");
