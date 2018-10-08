#############################################################################
# This file contains obsolet functions which are to be kept during a while for
# compatibility
# WARNING: the manual must be updated before removing the functions
#############################################################################
##
#F  GeneratorsOfNumericalSemigroupNC(S)
##
##  Returns a set of generators of the numerical
##  semigroup S.
##
#####From version 0.980 is just a synonym of the check version of the function
#############################################################################
DeclareSynonym( "GeneratorsOfNumericalSemigroupNC",GeneratorsOfNumericalSemigroup);

## the name "RandomNumericalSemigroupWithPseudoFrobeniusNumbers" should be removed in a further version... (it is not documented)
DeclareSynonym("RandomNumericalSemigroupWithPseudoFrobeniusNumbers",ANumericalSemigroupWithPseudoFrobeniusNumbers);
