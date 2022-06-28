#############################################################################
##
#W  catenary-tame.gd        Manuel Delgado <mdelgado@fc.up.pt>
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
##   -S. T. Chapman, P. A. Garcia-Sanchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
DeclareGlobalFunction( "CatenaryDegreeOfNumericalSemigroup" );
DeclareOperation("CatenaryDegree",[IsNumericalSemigroup]);
#############################################################################
##
#F  CatenaryDegreeOfElementInNumericalSemigroup(s)
##
## This function returns the catenary cegree in a numerical semigroup S of 
## a positive integer n
##
##
DeclareGlobalFunction( "CatenaryDegreeOfElementInNumericalSemigroup" );
DeclareOperation("CatenaryDegree",[IsNumericalSemigroup,IsInt]);
DeclareOperation("CatenaryDegree",[IsInt,IsNumericalSemigroup]);

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
##   -S. T. Chapman, P. A. Garcia-Sanchez,
##    D. Llena, V. Ponomarenko, and J. C. Rosales,
##    The catenary and tame degree in finitely generated
##    cancellative monoids, Manuscripta Mathematica 120 (2006) 253--264
##
#############################################################################
DeclareGlobalFunction( "TameDegreeOfElementInNumericalSemigroup" );
DeclareOperation("TameDegree",[IsInt,IsNumericalSemigroup]);
DeclareOperation("TameDegree",[IsNumericalSemigroup,IsInt]);

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
##   -S. T. Chapman, P. A. Garcia-Sanchez,
##    D. Llena,  The catenary and tame degree of numerical
##    semigroups, Forum Math. 2007 1--13.
##
#############################################################################
DeclareGlobalFunction( "TameDegreeOfNumericalSemigroup" );
DeclareOperation("TameDegree",[IsNumericalSemigroup]);

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
DeclareOperation("Factorizations",[IsInt,IsNumericalSemigroup]);
DeclareOperation("Factorizations",[IsNumericalSemigroup,IsInt]);

#############################################################################
##
#F  RFMatrices(f,s)
##
##  The integer f is a pseudo-Frobenius number of the numerical semigroup s
##  For each minimal generator n of s, it computes the factorizations of 
##  f+n in terms of the generators of s. These factorizations yield 
##  combinations of f in terms of the minimal generators of s (by substracting n).
##  The output is the cartesian product of these combinations for each of the 
##  minimal generator. This corresponds with the set of all Row Factorization 
##  matrices introduced by Moscariello (RF-Matrices)
##
#############################################################################
DeclareGlobalFunction( "RFMatrices" );


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
DeclareOperation("Elasticity",[IsPosInt,IsNumericalSemigroup]);
DeclareOperation("Elasticity",[IsNumericalSemigroup,IsPosInt]);

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
DeclareOperation("Elasticity",[IsNumericalSemigroup]);

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
DeclareOperation("DeltaSet",[IsInt,IsNumericalSemigroup]);
DeclareOperation("DeltaSet",[IsNumericalSemigroup,IsInt]);

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
DeclareOperation("MaximumDegree",[IsInt,IsNumericalSemigroup]);
DeclareOperation("MaximumDegree",[IsNumericalSemigroup,IsInt]);

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
DeclareOperation("OmegaPrimality",[IsInt,IsNumericalSemigroup]);
DeclareOperation("OmegaPrimality",[IsNumericalSemigroup,IsInt]);


#############################################################################
##
#F  OmegaPrimalityOfNumericalSemigroup(s)
##
##  Computes the maximum of omega primality of the minimal generators of S.
##
#############################################################################
DeclareGlobalFunction("OmegaPrimalityOfNumericalSemigroup");
DeclareOperation("OmegaPrimality",[IsNumericalSemigroup]);

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
DeclareOperation("DeltaSet",[IsHomogeneousList]);

#############################################################################
##
#F  CatenaryDegreeOfSetOfFactorizations(fact)
##
##  Computes the catenary degree of the set of factorizations 
##
#############################################################################
DeclareGlobalFunction("CatenaryDegreeOfSetOfFactorizations");
DeclareOperation("CatenaryDegree",[IsHomogeneousList]);

#############################################################################
##
#F  TameDegreeOfSetOfFactorizations(fact)
##
##  Computes the tame degree of the set of factorizations 
##
#############################################################################
DeclareGlobalFunction("TameDegreeOfSetOfFactorizations");
DeclareOperation("TameDegree",[IsHomogeneousList]);


#############################################################################
##
#F  RClassesOfSetOfFactorizations(l)
##
##  Determine the set of R-classes (Chapter 7 [RGBook] of a set of factorizations
##
#############################################################################
DeclareGlobalFunction("RClassesOfSetOfFactorizations");

########################################################
#  MaximalDenumerantOfElementInNumericalSemigroup(x,s)
#  returns the number of factorizations of maximal length of x in 
#  the numerical semigroup s
########################################################
DeclareGlobalFunction("MaximalDenumerantOfElementInNumericalSemigroup");
DeclareOperation("MaximalDenumerant",[IsInt,IsNumericalSemigroup]);
DeclareOperation("MaximalDenumerant",[IsNumericalSemigroup,IsInt]);

########################################################
#  MaximalDenumerantOfSetOfFactorizations(ls)
#  returns the number of factorizations of maximal length in ls
########################################################
DeclareGlobalFunction("MaximalDenumerantOfSetOfFactorizations");

########################################################
# MaximalDenumerantOfNumericalSemigroup(s)
# computes the maximal denumerant of a numerical semigroup
# by using de algorithm given by Bryant and Hamblin 
# Semigroup Forum 86 (2013), 571-582
########################################################
DeclareGlobalFunction("MaximalDenumerantOfNumericalSemigroup");
DeclareOperation("MaximalDenumerant",[IsNumericalSemigroup]);

########################################################
# AdjustmentOfNumericalSemigroup(s)
# computes the adjustment a numerical semigroup
# by using de algorithm given by Bryant and Hamblin 
# Semigroup Forum 86 (2013), 571-582
########################################################
DeclareGlobalFunction("AdjustmentOfNumericalSemigroup");
DeclareOperation("Adjustment",[IsNumericalSemigroup]);

##############################################################
# IsAdditiveNumericalSemigroup(s)
# Detects if s is an additive numerical semigroup, that is, 
# ord(m+x)=ord(x)+1 for all x in s. For these semigroups gr_m(K[[s]]) is 
# Cohen-Macaulay.	
# We use Proposition 4.7 in  Semigroup Forum 86 (2013), 571-582
##############################################################
DeclareGlobalFunction("IsAdditiveNumericalSemigroup");

##############################################################
# IsSuperSymmetricNumericalSemigroup(s)
# Detects if s is a numerical semigroup is supersymmetric, that is, 
# it is symmetric, additive and whenever w+w'=f+m 
# (with m the multiplicity and f the Frobenius number) we have 
# ord(w+w')=ord(w)+ord(w')
##############################################################
DeclareGlobalFunction("IsSuperSymmetricNumericalSemigroup");

#######################################################################
# BelongsToHomogenizationOfNumericalSemigroup(n,s)
# checks if the pair n belongs to the homogenization of s
#######################################################################
DeclareGlobalFunction("BelongsToHomogenizationOfNumericalSemigroup");

#######################################################################
# FactorizationsInHomogenizationOfNumericalSemigroup(n,s)
# computes the set of factorizations of  n with respect to generators of  
# the homogenization of s
#######################################################################
DeclareGlobalFunction("FactorizationsInHomogenizationOfNumericalSemigroup");

#######################################################################
# HomogeneousBettiElementsOfNumericalSemigroup(s) 
#  Computes the Betti elements of the Homogenization of s 
#  uses Cox-Little-O'Shea, Chapter 8, Theorem 4  for finding 
#  a system of generators of the ideal of S^h
#######################################################################
DeclareGlobalFunction("HomogeneousBettiElementsOfNumericalSemigroup");

####################################################################
#F HomogeneousCatenaryDegreeOfNumericalSemigroup(s) computes the 
##  homogeneous catenary degree of the numerical semigroup s ([GSOSN])
####################################################################
DeclareGlobalFunction("HomogeneousCatenaryDegreeOfNumericalSemigroup");

########################################
#F DenumerantOfElementInNumericalSemigroup(n,s)
## returns the denumerant
########################################
DeclareGlobalFunction("DenumerantOfElementInNumericalSemigroup");
### as function
DeclareOperation("DenumerantFunction",[IsNumericalSemigroup]);


#################################################################
## DenumerantIdeal(s,n)
## returns the ideal of s of all elements in s with denumerant 
## larger than n
#################################################################
DeclareOperation("DenumerantIdeal",[IsNumericalSemigroup,IsInt]);
DeclareOperation("DenumerantIdeal",[IsInt,IsNumericalSemigroup]);

####################################################################
#F MoebiusFunctionAssociatedToNumericalSemigroup(s,x)
## Computes the value in x of  Moebius function of the poset 
## associated to a numerial semigroup s 
## -Chappelon and Ramirez Alfonsin, Semigroup Forum 87 (2013), 313-330
####################################################################
DeclareGlobalFunction("MoebiusFunctionAssociatedToNumericalSemigroup");
DeclareOperation("MoebiusFunction",[IsNumericalSemigroup]);

###################################################################
#F  AdjacentCatenaryDegreeOfSetOfFactorizations(ls)
## computes the adjacent catenary degree of the set of factorizations ls
###################################################################
DeclareGlobalFunction("AdjacentCatenaryDegreeOfSetOfFactorizations");

###################################################################
#F EqualCatenaryDegreeOfSetOfFactorizations(ls) 
## computes the equal catenary degree of of the set of factorizations
###################################################################
DeclareGlobalFunction("EqualCatenaryDegreeOfSetOfFactorizations");

###################################################################
#F MonotoneCatenaryDegreeOfSetOfFactorizations(ls) 
## computes the equal catenary degree of of the set of factorizations
###################################################################
DeclareGlobalFunction("MonotoneCatenaryDegreeOfSetOfFactorizations");

############################################################
#F LShapesOfNumericalSemigroup(s)
## computes the set of LShapes associated to S (see [AG-GS])
##########################################################
DeclareGlobalFunction("LShapesOfNumericalSemigroup");
DeclareOperation("LShapes",[IsNumericalSemigroup]);

###########################################################################
#F  DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup(s)
##
## Computes the sets of elements in s, such that there exists a minimal 
## solution to msg*x-msg*y = 0, |x|<=|y| such that x,y are factorizations of s
## Used to compute the monotone catenary degree of the semigroup s
##
#############################################################################
DeclareGlobalFunction("DegreesOfMonotonePrimitiveElementsOfNumericalSemigroup");

###########################################################################
#F  DegreesOfEqualPrimitiveElementsOfNumericalSemigroup(s)
##
## Computes the sets of elements in s, such that there exists a minimal 
## solution to msg*x-msg*y = 0, |x|=|y| such that x,y are factorizations of s
## Used to compute the equal catenary degree of the semigroup
##
#############################################################################
DeclareGlobalFunction("DegreesOfEqualPrimitiveElementsOfNumericalSemigroup");

####################################################################
#F EqualCatenaryDegreeOfNumericalSemigroup(s) computes the 
##  adjacent catenary degree of the numerical semigroup s
##  the equal catenary degree is reached in the set of primitive
##  elements of s (see [PH])
####################################################################
DeclareGlobalFunction("EqualCatenaryDegreeOfNumericalSemigroup");

####################################################################
#F MonotoneCatenaryDegreeOfNumericalSemigroup(s) computes the 
##  adjacent catenary degree of the numerical semigroup s
##  the monotone catenary degree is reached in the set of primitive
##  elements of s (see [PH])
####################################################################
DeclareGlobalFunction("MonotoneCatenaryDegreeOfNumericalSemigroup");



###########################################################################
#####################################################################
##
#O FengRaoDistance(NS,r,m)
##
#Computes the r-th Feng-Rao distance of the element m in the numerical semigroup NS
#function originally implemented by Benjamin Heredia
#Based on the paper...
##
#####################################################################
DeclareOperation("FengRaoDistance",[IsNumericalSemigroup,IsPosInt,IsPosInt]);

###########################################################################
###########################################################################
##
#O FengRaoNumber(NS,r)
#O FengRaoNumber(r,NS)
# returns the r-Feng Rao number of a numerical semigroup NS
########
# based on [DelgadoFarranGarcia-SanchezLlena2013MC]
#################################################
#####################################################################
DeclareOperation("FengRaoNumber",[IsNumericalSemigroup,IsPosInt]);
DeclareOperation("FengRaoNumber",[IsPosInt,IsNumericalSemigroup]);

##############################################################################################################
##
#P  IsHomogeneousNumericalSemigroup(S)
##
##  Tests if S is homogeneous, that is, for every element a in the Apéry set of its multiplicity,
##  all the factorizations of a have the same length
##
##############################################################################################################
DeclareProperty("IsHomogeneousNumericalSemigroup",IsNumericalSemigroup);
