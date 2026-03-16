#############################################################################
##
#W  order.gd           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2026 by Manuel Delgado and Pedro Garcia-Sanchez 
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
##
#R  IsPosetNSRep
##
##  The representation of a poset defined by a numerical semigroup and a list
##  of integers.
##
#############################################################################
DeclareRepresentation("IsPosetNSRep", IsAttributeStoringRep, []);

#############################################################################
##
#C  IsPosetNS
##
##  The category of posets defined by numerical semigroups and lists of integers.
##
#############################################################################
DeclareCategory( "IsPosetNS", IsPosetNSRep);

# Elements of posets defined by numerical semigroups are integers, so posets 
# defined by numerical semigroups are collections of integers.
BindGlobal( "PosetNSType", NewType( CollectionsFamily(CyclotomicsFamily), IsPosetNS));

#############################################################################
##
#F PosetNS(l,S)
##
## l is a list of integers and S a numerical semigroup
##
## returns the poset whose underlying set is l and the order is defined by
## a <= b if b - a in S
##
#############################################################################
DeclareOperation("PosetNS",[IsList, IsNumericalSemigroup]);
# we allow changing the order of the arguments
DeclareOperation("PosetNS",[IsNumericalSemigroup,IsList]);


DeclareAttribute("UnderlyingNSPoset", IsPosetNS);
DeclareAttribute("GroundSet", IsPosetNS);

#############################################################################
##
#A MaximalElements(p)
## Returns the list of maximal elements of the poset p
##
#############################################################################
DeclareAttribute("MaximalElements", IsPosetNS);

#############################################################################
##
#A MinimalElements(p)
## Returns the list of minimal elements of the poset p
##
#############################################################################
DeclareAttribute("MinimalElements", IsPosetNS);

#############################################################################
##
#O Upset(p,l)
##  Returns the upset of the list l in the poset p, that is, the set of 
##  elements greater than or equal to some element of l.
##
#############################################################################
DeclareOperation("Upset", [IsPosetNS, IsList]);

#############################################################################
##
#O Downset(p,l)
##  Returns the downset of the list l in the poset p, that is, the set of 
##  elements less than or equal to some element of l.
##
#############################################################################
DeclareOperation("Downset", [IsPosetNS, IsList]);

#############################################################################
##
#O Antichains(p)
##  Returns the set of antichains (sets of non comparable elements) of p.
##
#############################################################################
DeclareOperation("Antichains", [IsPosetNS]);

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


############################################################################
##
#O HasseDiagram(P)
##  Returns the Hasse diagram of the poset P.
##
############################################################################
DeclareOperation("HasseDiagram", [IsPosetNS]);