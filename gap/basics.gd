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
DeclareAttribute( "Multiplicity", IsNumericalSemigroup);
DeclareSynonymAttr( "MultiplicityOfNumericalSemigroup", Multiplicity);


#############################################################################
##
#A  FrobeniusNumberOfNumericalSemigroup(S)
##
##  Returns the Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
# DeclareAttribute( "FrobeniusNumberOfNumericalSemigroup", IsNumericalSemigroup);

# DeclareSynonymAttr( "FrobeniusNumber", FrobeniusNumberOfNumericalSemigroup);
DeclareAttribute( "FrobeniusNumber", IsNumericalSemigroup);

DeclareSynonymAttr( "FrobeniusNumberOfNumericalSemigroup", FrobeniusNumber);


#############################################################################
##
#A  ConductorOfNumericalSemigroup(S)
##
##  Returns the conductor of the numerical semigroup S.
##
#############################################################################
DeclareAttribute("Conductor", IsNumericalSemigroup);
DeclareSynonymAttr("ConductorOfNumericalSemigroup", Conductor);
#DeclareGlobalFunction("ConductorOfNumericalSemigroup");

#############################################################################
##
#A  TypeOfNumericalSemigroup(S)
##
##  Returns the type of the numerical semigroup S.
##
#############################################################################
#class with FinInG
#DeclareAttribute("Type", IsNumericalSemigroup);
#DeclareSynonymAttr("TypeOfNumericalSemigroup", Type);
DeclareAttribute("TypeOfNumericalSemigroup", IsNumericalSemigroup);
DeclareOperation("Type",[IsNumericalSemigroup]);

#############################################################################
##
#A  Generators(S)
#A  GeneratorsOfNumericalSemigroup(S)
##
##  Returns a set of generators of the numerical
##  semigroup S. If a minimal generating system has already been computed, this
##  is the set returned.
##
#############################################################################
#DeclareAttribute( "Generators", IsNumericalSemigroup);
DeclareSynonymAttr( "GeneratorsOfNumericalSemigroup", Generators);
#DeclareGlobalFunction( "GeneratorsOfNumericalSemigroup");

#############################################################################
##
#A  MinimalGenerators(S)
#A  MinimalGeneratingSystem(S)
#A  MinimalGeneratingSystemOfNumericalSemigroup(S)
##
##  Returns the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "MinimalGenerators", IsNumericalSemigroup);
DeclareSynonymAttr( "MinimalGeneratingSystem", MinimalGenerators);
DeclareSynonymAttr( "MinimalGeneratingSystemOfNumericalSemigroup", MinimalGenerators);

#############################################################################
#############################################################################
##
#F  MinimalGeneratingSystem(S)
##
##  If S is a numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfNumericalSemigroup
## If S is an ideal of numerical semigroup, then this function just passes the task of computing the minimal generating system to MinimalGeneratingSystemOfIdealOfNumericalSemigroup
##
#############################################################################
#DeclareGlobalFunction("MinimalGeneratingSystem");

#############################################################################
##
#A  EmbeddingDimensionOfNumericalSemigroup(S)
##
##  Returns the cardinality of the minimal generating system of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "EmbeddingDimension",IsNumericalSemigroup);
DeclareSynonymAttr("EmbeddingDimensionOfNumericalSemigroup",EmbeddingDimension);

#############################################################################
##
#A  FundamentalGaps(S)
#A  FundamentalGapsOfNumericalSemigroup(S)
##
##  Returns the fundamental gaps of the numerical
##  semigroup S.
##
#############################################################################
DeclareSynonymAttr( "FundamentalGapsOfNumericalSemigroup",FundamentalGaps);


#############################################################################
##
#A  PseudoFrobeniusOfNumericalSemigroup(S)
##
##  Returns the pseudo Frobenius number of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "PseudoFrobeniusOfNumericalSemigroup",IsNumericalSemigroup);
DeclareSynonymAttr("PseudoFrobenius",PseudoFrobeniusOfNumericalSemigroup);

#############################################################################
##
#A  SpecialGapsOfNumericalSemigroup(S)
##
##  Returns the special gaps of the numerical
##  semigroup S.
##
#############################################################################
DeclareAttribute( "SpecialGaps",IsNumericalSemigroup);
DeclareSynonymAttr( "SpecialGapsOfNumericalSemigroup",SpecialGaps);


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
#A  AperyList(S)
#A  AperyListOfNumericalSemigroup(S)
##
##  Returns the Apery list of the numerical
##  semigroup S with respect to the multiplicity.
##
#############################################################################
#DeclareAttribute( "AperyList", IsNumericalSemigroup);
DeclareSynonymAttr( "AperyListOfNumericalSemigroup", AperyList);
#DeclareGlobalFunction("AperyListOfNumericalSemigroup");

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
DeclareOperation("KunzCoordinates",[IsNumericalSemigroup]);
DeclareOperation("KunzCoordinates",[IsNumericalSemigroup,IsInt]);

#############################################################################
##
#F KunzPolytope(m)
## For a fixed multiplicity, the Kunz coordinates of the semigroups
## with that multiplicity are solutions of a system of inequalities Ax\ge b
## (see [R-GS-GG-B]). The output is the matrix (A|-b)
##
#############################################################################
DeclareGlobalFunction("KunzPolytope");

#############################################################################
##
#A HolesOfNumericalSemigroup(s)
## For a numerical semigroup, finds the set of gaps x such that F(S)-x is
## is also a gap
##
#############################################################################
DeclareAttribute("Holes",IsNumericalSemigroup);
DeclareSynonymAttr("HolesOfNumericalSemigroup", Holes);

#############################################################################
##
#F  CocycleOfNumericalSemigroupWRTElement(S,n)
##
##  Returns the cocycle of the numerical semigroup S with respect to
##  the positive integer n (an element in S)
##
#############################################################################
DeclareGlobalFunction("CocycleOfNumericalSemigroupWRTElement");
#############################################################################
#############################################################################

#############################################################################
##
#O RthElementOfNumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer r, returns the r-th element of S
#############################################################################
DeclareOperation("RthElementOfNumericalSemigroup",[IsNumericalSemigroup,IsInt]);
DeclareOperation("RthElementOfNumericalSemigroup",[IsInt,IsNumericalSemigroup]);



#############################################################################
##
#O NextElementOfNumericalSemigroup(S,n)
## Given a numerical semigroup S and an integer r, returns the least integer
## greater than r belonging to S
#############################################################################
DeclareOperation("NextElementOfNumericalSemigroup",[IsNumericalSemigroup,IsInt]);
DeclareOperation("NextElementOfNumericalSemigroup",[IsInt,IsNumericalSemigroup]);


#############################################################################
##
#O DivisorsOfElementInNumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer n, returns a list L of integers such that
# x in L if and only if n - x belongs to S, that is, it returns S\cap(n-S)
# These elements are called divisors of n
##
#############################################################################
DeclareOperation("DivisorsOfElementInNumericalSemigroup",[IsNumericalSemigroup,IsInt]);
DeclareOperation("DivisorsOfElementInNumericalSemigroup",[IsInt,IsNumericalSemigroup]);

#########################################################################
##
#F NumericalSemigroupByNuSequence(NuSeq)
## Given a nu-sequence, compute the semigroup asociated to it.
## Based on the code by Jorge Angulo, inspired in 
## - Bras-Amorós, Maria Numerical semigroups and codes. 
## Algebraic geometry modeling in information theory, 167–218, 
## Ser. Coding Theory Cryptol., 8, World Sci. Publ., Hackensack, NJ, 2013
#########################################################################
DeclareGlobalFunction("NumericalSemigroupByNuSequence");

#########################################################################
#F NumericalSemigroupByTauSequence(TauSeq)
## Given a tau-sequence, compute the semigroup asociated to it.
## Based on the code by Jorge Angulo, inspired in 
## - Bras-Amorós, Maria Numerical semigroups and codes. 
## Algebraic geometry modeling in information theory, 167–218, 
## Ser. Coding Theory Cryptol., 8, World Sci. Publ., Hackensack, NJ, 2013
#########################################################################
DeclareGlobalFunction("NumericalSemigroupByTauSequence");

#############################################################################
##
#F ElementNumber_NumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer n, returns the nth element of S
#############################################################################
DeclareGlobalFunction("ElementNumber_NumericalSemigroup");

#############################################################################
##
#F NumberElement_NumericalSemigroup(S,n)
# Given a numerical semigroup S and an integer n, returns the position of 
# n in S
#############################################################################
DeclareGlobalFunction("NumberElement_NumericalSemigroup");
