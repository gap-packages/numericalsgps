#############################################################################
##
#W  irreducibles.gd         Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: irreducibles.gd,v 0.971 $
##
#Y  Copyright 2005 by Manuel Delgado, 
#Y  Pedro Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the 
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#F  RemoveMinimalGeneratorFromNumericalSemigroup(n,s)
##
##  Computes the numerical semigroup obtained from s after removing from s 
##  its minimal generator n: s\{n}.
##
#############################################################################
DeclareGlobalFunction("RemoveMinimalGeneratorFromNumericalSemigroup");


#############################################################################
##
#F  AddSpecialGapOfNumericalSemigroup(g,s)
##
##  Adds the special gap g to the numerical semigroup s and
##  returns the resulting numerical semigroup: s\cup {g}.
##
#############################################################################
DeclareGlobalFunction("AddSpecialGapOfNumericalSemigroup");


#############################################################################
##
#F  AnIrreducibleNumericalSemigroupWithFrobeniusNumber(f)
##
##  Produces an irreducible numerical semigroup by using
##  "Every positive integer is the Frobenius number of an irreducible...".
##
#############################################################################
DeclareGlobalFunction("AnIrreducibleNumericalSemigroupWithFrobeniusNumber");


#############################################################################
##
#F  IrreducibleNumericalSemigroupsWithFrobeniusNumber(f)
##
##  Computes the set of irreducible numerical semigroups with given
##  Frobenius number f.
##
#############################################################################
DeclareGlobalFunction("IrreducibleNumericalSemigroupsWithFrobeniusNumber");


#############################################################################
##
#F  OverSemigroupsNumericalSemigroup(s)
##
##  Computes the set of numerical semigroups containing s.
##  The algorithm is based on 
##  "The oversemigroups of a numerical semigroup", Semigroup Forum.
##
#############################################################################
DeclareGlobalFunction("OverSemigroupsNumericalSemigroup");


#############################################################################
##
#F  DecomposeIntoIrreducibles(s)
##
##  Returns a list of irreducible numerical semigroups
##  such that its intersection is s.
##  This decomposition is minimal, and is inspired in 
##  Algorithm 26 of "The oversemigroups of a numerical semigroup".
##
#############################################################################
DeclareGlobalFunction("DecomposeIntoIrreducibles");


#############################################################################
##
#F  IsIrreducibleNumericalSemigroup(s)
##
##  Checks whether or not s is an irreducible numerical semigroup.
##
#############################################################################
DeclareGlobalFunction("IsIrreducibleNumericalSemigroup");


#############################################################################
##
#F  IsSymmetricNumericalSemigroup(s)
##
##  Checks whether or not s is a symmetric numerical semigroup.
##
#############################################################################
DeclareGlobalFunction("IsSymmetricNumericalSemigroup");


#############################################################################
##
#F  IsPseudoSymmetricNumericalSemigroup(s)
##
##  Checks whether or not s is a pseudosymmetric numerical semigroup.
##
#############################################################################
DeclareGlobalFunction("IsPseudoSymmetricNumericalSemigroup");



#############################################################################
##
#F AsGluingOfNumericalSemigroups
##
## returns all partitions {A1,A2} of the minimal generating set of s such
## that s is a gluing of <A1> and <A2> by gcd(A1)gcd(A2) 
##
#############################################################################
DeclareGlobalFunction("AsGluingOfNumericalSemigroups");

#############################################################################
##
#F IsACompleteIntersectionNumericalSemigroup
##
##returns true if the numerical semigroup is a complete intersection, 
## that is, the cardinality of a (any) minimal presentation equals 
## its embedding dimension minus one
##
#############################################################################
DeclareGlobalFunction("IsACompleteIntersectionNumericalSemigroup");
