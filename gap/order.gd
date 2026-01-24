############################################################################
##
#F HasseDiagramOfNumericalSemigroup(s, A)
##  Returns a binary relation which is the Hasse diagram of A with 
##  respect to the ordering a <= b if b - a in S.
##
############################################################################
DeclareGlobalFunction("HasseDiagramOfNumericalSemigroup");

############################################################################
##
#F HasseDiagramOfBettiElementsOfNumericalSemigroup(s)
##  Returns a binary relation which is the Hasse diagram of the Betti
##  elements of s with respect to the ordering a <= b if b - a in S.
##
############################################################################
DeclareGlobalFunction("HasseDiagramOfBettiElementsOfNumericalSemigroup");

############################################################################
##
#F HasseDiagramOfAperyListOfNumericalSemigroup(s, n)
##  Returns a binary relation which is the Hasse diagram of the 
##  set Ap(s; n) with respect to the ordering a <= b if b - a in S.
##  The argument n is optional and its default value is the multiplicity 
##  of s.
##
############################################################################
DeclareGlobalFunction("HasseDiagramOfAperyListOfNumericalSemigroup");

############################################################################
##
#F AntichainsOfNumericalSemigroup(s, A)
##  Returns a the set of antichains (sets of non comparable elements) of A 
##  with respect to the ordering a <= b if b - a in S.
##  The implementation is based on the implementation of antichains_iterator
##  Peter Jipsen and Franco Saliola for the SAGE project
##
############################################################################
DeclareGlobalFunction("AntichainsOfNumericalSemigroup");
