#############################################################################
##
#W  catenary-tame.gd        Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: catenary-tame.gd,v 0.98 $
##
#Y  Copyright 2005 by Manuel Delgado,
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#F  NSGPfactorizationsNC(n,l)
##
##  <n> is a nonnegative integer and <l> is a list of positive integers.
##  Returns a list with the different factorizations of n as a linear
##  combination with elements in l.
##
#############################################################################
DeclareGlobalFunction( "NSGPfactorizationsNC" );


#############################################################################
##
#F  CatenaryDegreeOfNumericalSemigroup(s)
##
##  Computes the catenary degree of the numerical semigroup <s>.
##
##  The definition of catenary degree can be found in
##  the book:
##   -A. Geroldinger and F. Halter-Koch, Non-unique
##    Factorizations: Algebraic, Combinatorial and
##    Analytic Theory, Pure and AppliedMathematics,
##    vol. 278, Chapman & Hall/CRC, 2006.
##  The algorithm used appears in
##   -S. T. Chapman, P. A. García-Sánchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
DeclareGlobalFunction( "CatenaryDegreeOfNumericalSemigroup" );
#############################################################################
##
#F  IsConnectedGraphNCForNumericalSemigroups(l)
##
## This function returns true if the graph is connected an false otherwise
##
## It is part of the NumericalSGPS package just to avoid the need of using 
## other graph packages only to this effect. It is used in 
## CatenaryDegreeOfElementNS
##
#############################################################################
DeclareGlobalFunction("IsConnectedGraphNCForNumericalSemigroups");
#############################################################################
##
#F  CatenaryDegreeOfElementInNumericalSemigroup(s)
##
## This function returns the catenary cegree in a numerical semigroup S of 
## a positive integer n
##
## The NC version of CatenaryDegreeOfElementNS works well for numbers
## bigger than the Frobenius number
##
DeclareGlobalFunction( "CatenaryDegreeOfElementInNumericalSemigroup_NC" );
DeclareGlobalFunction( "CatenaryDegreeOfElementInNumericalSemigroup" );

#############################################################################
##
#F  TameDegreeOfElementInNumericalSemigroup(n,s)
##
##  Computes the tame degre of the element <n> of the numerical semigroup <s>.
##  Used for the computation of the tame degree of s, but can
##  be used separately.
##
##  The definition of tame degree appears in
##   -A. Geroldinger and F. Halter-Koch, Non-unique
##    Factorizations: Algebraic, Combinatorial and
##    Analytic Theory, Pure and AppliedMathematics,
##    vol. 278, Chapman & Hall/CRC, 2006.
##  The algorithm used appears in
##   -S. T. Chapman, P. A. García-Sánchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
DeclareGlobalFunction( "TameDegreeOfElementInNumericalSemigroup" );


#############################################################################
##
#F  TameDegreeOfNumericalSemigroup(s)
##
##  Computes the tame degree of a numerical semigroup <s>.
##
##  The definition of tame degree appears in
##   -A. Geroldinger and F. Halter-Koch, Non-unique
##    Factorizations: Algebraic, Combinatorial and
##    Analytic Theory, Pure and AppliedMathematics,
##    vol. 278, Chapman & Hall/CRC, 2006.
##  The algorithm used appears in
##   -S. T. Chapman, P. A. García-Sánchez,
##    D. Llena,  The catenary and tame degree of numerical
##    semigroups, Forum Math. 2007 1--13.
##
#############################################################################
DeclareGlobalFunction( "TameDegreeOfNumericalSemigroup" );


#############################################################################
##
#F  FactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the set of factorizations
##  of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
DeclareGlobalFunction( "FactorizationsElementWRTNumericalSemigroup" );


#############################################################################
##
#F  LengthsOfFactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the lengths of the set of
##  factorizations of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
DeclareGlobalFunction( "LengthsOfFactorizationsElementWRTNumericalSemigroup" );


#############################################################################
##
#F  ElasticityOfFactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the quotient (maximum length)/(minimum lenght) of the
##  factorizations of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
DeclareGlobalFunction( "ElasticityOfFactorizationsElementWRTNumericalSemigroup" );


#############################################################################
##
#F  ElasticityOfNumericalSemigroup(s)
##
##  Computes the supremum of the elasticities of the
##  factorizations of the elements of <s>.
##  From [CHM06, GHKb] this is precisely np/n1
##  with n1 the multiplicity of <s> and np the greatest
##  generator.
##
#############################################################################
DeclareGlobalFunction( "ElasticityOfNumericalSemigroup" );


#############################################################################
##
#F  DeltaSetOfFactorizationsElementWRTNumericalSemigroup(n,s)
##
##  Computes the set of differences between
##  two consecutive lengths of factorizations of
##  an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
DeclareGlobalFunction( "DeltaSetOfFactorizationsElementWRTNumericalSemigroup" );


#############################################################################
##
#F  MaximumDegreeOfElementWRTNumericalSemigroup(n,s)
##
##  Computes the maximum length of the
##  factorizations of an element <n> as linear combinations
##  with nonnegative coefficients of the minimal generators
##  of the semigroup <s>.
##
#############################################################################
DeclareGlobalFunction( "MaximumDegreeOfElementWRTNumericalSemigroup" );

#############################################################################
##
#F  OmegaPrimalityOfElementInNumericalSemigroup(n,s)
##
##  Computes the omega primality of an elmenent n in S, as explained in 
##  V. Blanco, P. A. Garc\'{\i}a-S\'anchez, A. Geroldinger, 
##  Semigroup-theoretical characterizations of arithmetical invariants with 
##  applications to numerical monoids and Krull monoids, {arXiv}:1006.4222v1.
##
#############################################################################
DeclareGlobalFunction("OmegaPrimalityOfElementInNumericalSemigroup");

#############################################################################
##
#F  OmegaPrimalityOfNumericalSemigroup(s)
##
##  Computes the maximum of omega primality of the minimal generators of S.
##
#############################################################################
DeclareGlobalFunction("OmegaPrimalityOfNumericalSemigroup");


#############################################################################
##
#F  FactorizationsIntegerWRTList(n,ls)
##
##  Computes the set of factorizations
##  of an integer n as linear combinations
##  with nonnegative coefficients of the elements in the list ls
##  Makes use of RestrictedPartitions
#############################################################################
DeclareGlobalFunction("FactorizationsIntegerWRTList");

#############################################################################
##
#F  LengthsOfFactorizationsIntegerWRTList(n,ls)
##
##  Computes the lengths of the set of
##  factorizations of an  integer <n> as linear combinations
##  with nonnegative coefficients of the elements in the list <ls>
##
#############################################################################
DeclareGlobalFunction("LengthsOfFactorizationsIntegerWRTList");

#############################################################################
##
#F  DeltaSetOfSetOfIntegers(n,s)
##
##  Computes the set of differences between
##  two consecutive lengths of factorizations of
##  an integer <n> as linear combinations
##  with nonnegative coefficients of the elements in the list <ls>
##
#############################################################################
DeclareGlobalFunction("DeltaSetOfSetOfIntegers");

#############################################################################
##
#F  CatenaryDegreeOfSetOfFactorizations(fact)
##
##  Computes the catenary degree of the set of factorizations 
##
#############################################################################
DeclareGlobalFunction("CatenaryDegreeOfSetOfFactorizations");

#############################################################################
##
#F  TameDegreeOfSetOfFactorizations(fact)
##
##  Computes the tame degree of the set of factorizations 
##
#############################################################################
DeclareGlobalFunction("TameDegreeOfSetOfFactorizations");

#############################################################################
##
#F  RClassesOfSetOfFactorizations(l)
##
##  Determine the set of R-classes (Chapter 7 [RGBook] of a set of factorizations
##
#############################################################################
DeclareGlobalFunction("RClassesOfSetOfFactorizations");

