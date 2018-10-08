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
#############################################################################
# InstallGlobalFunction( GeneratorsOfNumericalSemigroupNC, function(S)
#     if not IsNumericalSemigroup(S) then
#         Error("The argument must be a numerical semigroup");
#     fi;
#     if HasGenerators(S) then
#         return(Generators(S));
#     fi;
#     return(MinimalGeneratingSystemOfNumericalSemigroup(S));
# end);
