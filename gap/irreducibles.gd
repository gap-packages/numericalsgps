#############################################################################
##
#W  irreducibles.gd         Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: irreducibles.gd,v 0.98 $
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
##  Frobenius number f, following Theorem 2.9 in [BR13]
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

#####################################################################
##                        Almost-symmetric numerical semigroups
## See [BF97] and [RGS13]
#  -J. C. Rosales, P. A. García-Sánchez, Constructing almost symmetric numerical 
#   semigroups from almost irreducible numerical semigroups, Comm. Algebra.
#####################################################################
##
#F IsAlmostSymmetricNumericalSemigroup(arg)
##
## The argument is a numerical semigroup. The output is True or False depending
## on if the semigroup is almost symmetric or not, see [BF97]
## 
#####################################################################
DeclareGlobalFunction("IsAlmostSymmetricNumericalSemigroup");


#####################################################################
##
#F AlmostSymmetricNumericalSemigrupsFromIrreducible(s)
##
## The argument is an irreducible numerical semigroup. The output is the set of
## almost-symmetric numerical semigroups obtained from s, as explained in 
## Theorem 3 in [RGS13]
## 
#####################################################################

DeclareGlobalFunction("AlmostSymmetricNumericalSemigroupsFromIrreducible");

#####################################################################
##
#F AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber(f)
##
## The argument is an integer. The output is the set of all almost-symmetric 
## numerical semigroups with Frobenius number f ([RGS13])
## 
#####################################################################
DeclareGlobalFunction("AlmostSymmetricNumericalSemigroupsWithFrobeniusNumber");


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

#############################################################################
##
#F IsFreeNumericalSemigroup
##
# # returns true if the numerical semigroup is a free semigroup, in the sense of 
# # Bertin and Carbonne [BC77]
##
#############################################################################
DeclareGlobalFunction("IsFreeNumericalSemigroup");

#############################################################################
##
#F IsTelescopicNumericalSemigroup
##
# # returns true if the numerical semigroup is telescopic [KP95], 
##  that is, free for the ordering n_1<...<n_e, with n_i the minimal generators
##
#############################################################################
DeclareGlobalFunction("IsTelescopicNumericalSemigroup");

#############################################################################
##
#F IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity
##
## returns true if the numerical semigroup is a telescopic numerical semigroup,
##  and in addition for all i, d_i n_i < d_{i+1}n_{i+1}, con d_i=gcd{n_j | j<i} [Z86]
##
#############################################################################
DeclareGlobalFunction("IsNumericalSemigroupAssociatedIrreduciblePlanarCurveSingularity");

#############################################################################
##
#F NumericalSemigroupsAssociatedIrreduciblePlanarCurveSingularityWithFrobeniusNumber
##
## returns the set of numerical semigroups associated to irreducible
## planar curves with Frobenius number given, as explained in [AGS13]
##
#############################################################################
DeclareGlobalFunction("NumericalSemigroupsAssociatedIrreduciblePlanarCurveSingularityWithFrobeniusNumber");

#############################################################################
##
#F TelescopicNumericalSemigroupsWithFrobeniusNumber
##
## returns the set of telescopic numerical semigroups with Frobenius number 
## given, as explained in [AGS13]
##
#############################################################################
DeclareGlobalFunction("TelescopicNumericalSemigroupsWithFrobeniusNumber");

#############################################################################
##
#F FreeNumericalSemigroupsWithFrobeniusNumber
##
## returns the set of free numerical semigroups with Frobenius number 
## given, as explained in [AGS13]
##
#############################################################################
DeclareGlobalFunction("FreeNumericalSemigroupsWithFrobeniusNumber");

#############################################################################
##
#F CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber
##
## returns the set of comple intersection numerical semigroups with Frobenius number 
## given, as explained in [AGS13]
##
#############################################################################
DeclareGlobalFunction("CompleteIntersectionNumericalSemigroupsWithFrobeniusNumber");
