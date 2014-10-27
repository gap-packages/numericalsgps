#############################################################################
##
#W  affine.gd               Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright .........
#############################################################################

#############################################################################
##
#F  HilbertBasisOfSystemOfHomogeneousEquations
#
# Computes the Hilbert basis of the system A X=0 mod md, where the rows
# of A are the elements of ls.
# md can be empty of have some modulus, if the length of md is smaller than 
# the lengths of the elements of ls, then the rest of equations are considered
# to be homogeneous linear Diophantine equations
##########################################################################
DeclareGlobalFunction("HilbertBasisOfSystemOfHomogeneousEquations");

#############################################################################
##
#F  HilbertBasisOfSystemOfHomogeneousInequalities
#
# Computes the Hilbert basis of the system ls*X>=0 over the nonnegative 
# integers
##########################################################################
DeclareGlobalFunction("HilbertBasisOfSystemOfHomogeneousInequalities");

###############################################################################
#F FactorizationsVectorWRTList
#
# Computes the set of factorizations of v in terms of the elements of ls 
# That is, a Hilbert basis for ls*X=v
# If ls contains vectors that generate a nonreduced monoid, then it 
# deprecates the infinite part of the solutions, or in other words, it
# returns only the minimal solutions of the above system of equations
########################################################################
DeclareGlobalFunction("FactorizationsVectorWRTList");

###############################################################################
#F OmegaPrimalityOfElementInAffineSemigroup
#
# Computes the omega-primality of v in the monoid a
###########################################################################
DeclareGlobalFunction("OmegaPrimalityOfElementInAffineSemigroup");

###############################################################################
#F OmegaPrimalityOfAffineSemigroup
#
# Computes the omega primality of the affine semigroup a
###########################################################################
DeclareGlobalFunction("OmegaPrimalityOfAffineSemigroup");

###############################################################################
#F PrimitiveElementsOfAffineSemigroup_Normaliz
# Computes the primitive elements of the affine semigroup a
#####################################################################

DeclareGlobalFunction("PrimitiveElementsOfAffineSemigroup");

###############################################################################
#F PrimitiveElementsOfAffineSemigroup_Normaliz
# An implementation of PrimitiveElementsOfAffineSemigroup using 
# Normaliz
#####################################################################

#labelled Normaliz, since this one is slower than with 4ti2
###########################################################################
#DeclareGlobalFunction("PrimitiveElementsOfAffineSemigroup_Normaliz");

###############################################################################
#F TameDegreeOfAffineSemigroup
# Computes the tame degree of the affine semigroup a
###########################################################################
DeclareGlobalFunction("TameDegreeOfAffineSemigroup");

###############################################################################
#F ElasticityOfAffineSemigroup_Normaliz
# Computes the elasticity of the affine semigroup a
###########################################################################
#DeclareGlobalFunction("ElasticityOfAffineSemigroup_Normaliz");

###############################################################################
#F ElasticityOfAffineSemigroup
# Computes the elasticity of the affine semigroup a
###########################################################################
DeclareGlobalFunction("ElasticityOfAffineSemigroup");

###############################################################################
#F BettiElementsOfAffineSemigroup
# Computes the Betti elements of the affine semigroup a
###########################################################################
DeclareGlobalFunction("BettiElementsOfAffineSemigroup");

###############################################################################
#F MinimalPresentationOfAffineSemigroup
# Computes a minimal presentation of the affine semigroup a
###########################################################################
DeclareGlobalFunction("MinimalPresentationOfAffineSemigroup");

###############################################################################
#F CatenaryDegreeOfAffineSemigroup
# Computes the catenary degree of the affine semigroup a
###########################################################################
DeclareGlobalFunction("CatenaryDegreeOfAffineSemigroup");

###############################################################################
#F 
#
###########################################################################
DeclareGlobalFunction("");
