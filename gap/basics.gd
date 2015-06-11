#############################################################################
##
#W  basics.gd               Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#Y  Copyright 2005 by Manuel Delgado,
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#A  MultiplicityOfNumericalSemigroup(S)
##
##  Returns the multiplicity of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "MultiplicityOfNumericalSemigroup", IsNumericalSemigroup);


#############################################################################
##
#A  FrobeniusNumberOfNumericalSemigroup(S)
##
##  Returns the Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "FrobeniusNumberOfNumericalSemigroup", IsNumericalSemigroup);

DeclareSynonymAttr( "FrobeniusNumber", FrobeniusNumberOfNumericalSemigroup);


#############################################################################
##
#F  ConductorOfNumericalSemigroup(S)
##
##  Returns the conductor of the numerical semigroup S. 
##
#############################################################################
DeclareGlobalFunction("ConductorOfNumericalSemigroup");

#############################################################################
##
#F  TypeOfNumericalSemigroup(S)
##
##  Returns the type of the numerical semigroup S. 
##
#############################################################################
DeclareGlobalFunction("TypeOfNumericalSemigroup");

#############################################################################
##
#F  GeneratorsOfNumericalSemigroup(S)
##
##  Returns a set of generators of the numerical
##  semigroup S. If a minimal generating system has already been computed, this
##  is the set returned.
##
#############################################################################
DeclareGlobalFunction( "GeneratorsOfNumericalSemigroup");


#############################################################################
##
#F  GeneratorsOfNumericalSemigroupNC(S)
##
##  Returns a set of generators of the numerical
##  semigroup S.
##
#####From version 0.980 is just a synonym of the check version of the function
#############################################################################
DeclareSynonym( "GeneratorsOfNumericalSemigroupNC","GeneratorsOfNumericalSemigroup");


#############################################################################
##
#A  MinimalGeneratingSystemOfNumericalSemigroup(S)
##
##  Returns the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "MinimalGeneratingSystemOfNumericalSemigroup", IsNumericalSemigroup);
#############################################################################
#############################################################################
##
#F  MinimalGeneratingSystem(S)
##
##  If S is a numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfNumericalSemigroup
## If S is an ideal of numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfIdealOfNumericalSemigroup
##
#############################################################################
DeclareGlobalFunction("MinimalGeneratingSystem");

#############################################################################
##
#F  ReducedSetOfGeneratorsOfNumericalSemigroup(arg)
####From version 0.980 is a synonym of MinimalGenerating...
##
##  Returns a set with possibly fewer generators than those recorded in <C>S!.generators</C>. It changes <C>S!.generators</C> to the set returned.
##The function has 1 to 3 arguments. One of them a numerical semigroup. Then an argument is a boolean (<E>true</E> means that all the elements not belonging to the Apery set with respect to the multiplicity are removed; the default is "false") and another argument is a positive integer <M>n</M> (meaning that generators that can be written as the sum of <n> or less generators are removed; the default is "2"). The boolean or the integer may not be present. If a minimal generating set for <M>S</M> is known or no generating set is known, then the minimal generating system is returned.
##  
#DeclareGlobalFunction("ReducedSetOfGeneratorsOfNumericalSemigroup");
DeclareSynonym("ReducedSetOfGeneratorsOfNumericalSemigroup",MinimalGeneratingSystemOfNumericalSemigroup);
#############################################################################
##
#F  EmbeddingDimensionOfNumericalSemigroup(S)
##
##  Returns the cardinality of the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
DeclareGlobalFunction("EmbeddingDimensionOfNumericalSemigroup");

#############################################################################
##
#A  FundamentalGapsOfNumericalSemigroup(S)
##
##  Returns the fundamental gaps of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "FundamentalGapsOfNumericalSemigroup",IsNumericalSemigroup);


#############################################################################
##
#A  PseudoFrobeniusOfNumericalSemigroup(S)
##
##  Returns the pseudo Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "PseudoFrobeniusOfNumericalSemigroup",IsNumericalSemigroup);


#############################################################################
##
#A  SpecialGapsOfNumericalSemigroup(S)
##
##  Returns the special gaps of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "SpecialGapsOfNumericalSemigroup",IsNumericalSemigroup);


#############################################################################
##
#O  BelongsToNumericalSemigroup(n,S)
##
##  Tests if the integer n belongs to the numerical
##  semigroup S.
##
#############################################################################
DeclareOperation( "BelongsToNumericalSemigroup", [IsInt, IsNumericalSemigroup]);

#############################################################################
##
#O  AperyListOfNumericalSemigroupWRTElement(S,n)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to n.
##
#############################################################################
DeclareOperation( "AperyListOfNumericalSemigroupWRTElement",[IsNumericalSemigroup,IsInt]);

#############################################################################
##
#F  AperyListOfNumericalSemigroup(S)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to the multiplicity.
##
#############################################################################
DeclareGlobalFunction("AperyListOfNumericalSemigroup");

#############################################################################
##
#F  AperyListOfNumericalSemigroupWRTInteger(S,n)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to the positive integer n.
##
#############################################################################
DeclareGlobalFunction("AperyListOfNumericalSemigroupWRTInteger");


#############################################################################
##
#F  AperyListOfNumericalSemigroupAsGraph(ap)
##
##  <ap> is the Apery set of a numerical semigroup.
##  This function returns the adjacency list of the graph
##  whose vertices are
##  the elements of <ap> and the arrow u -> v exists
##  iff v - u is in <ap>.
##  The 0 is ignored.
##
#############################################################################
DeclareGlobalFunction("AperyListOfNumericalSemigroupAsGraph");

#############################################################################
##
#F  FirstElementsOfNumericalSemigroup(n,s)
##
##  Prints the list of the first <n> elements of <s>.
##
#############################################################################
DeclareGlobalFunction("FirstElementsOfNumericalSemigroup");

#############################################################################
##
#F KunzCoordinatesOfNumericalSemigroup(arg)
##
## If two argumets are given, the first is a semigroup s and the second an 
## element m in s. If one argument is given, then it is the semigroup, and 
## m is set to the multiplicity.
## Then the Apéry set of m in s has the form [0,k_1m+1,...,k_{m-1}m+m-1], and
## the output is the (m-1)-uple [k_1,k_2,...,k_{m-1}]
#############################################################################
DeclareGlobalFunction("KunzCoordinatesOfNumericalSemigroup");

#############################################################################
##
#F KunzPolytope(m)
## For a fixed multiplicity, the Kunz coordinates of the semigroups 
## with that multiplicity are solutions of a system of inequalities Ax\ge b 
## (see [R-GS-GG-B]). The output is the matrix (A|-b)
##
#############################################################################
DeclareGlobalFunction("KunzPolytope");
