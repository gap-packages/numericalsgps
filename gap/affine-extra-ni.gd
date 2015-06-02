#############################################################################
##
#W  affine-extra-ni.gd            
#W                          Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
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
#DeclareGlobalFunction("HilbertBasisOfSystemOfHomogeneousEquations");
#moved to affine.gd

#############################################################################
##
#F  HilbertBasisOfSystemOfHomogeneousInequalities
#
# Computes the Hilbert basis of the system ls*X>=0 over the nonnegative 
# integers
##########################################################################
#DeclareGlobalFunction("HilbertBasisOfSystemOfHomogeneousInequalities");
#moved to affine.df

###############################################################################
#F OmegaPrimalityOfElementInAffineSemigroup
#
# Computes the omega-primality of v in the monoid a
###########################################################################
#DeclareGlobalFunction("OmegaPrimalityOfElementInAffineSemigroup");
#
###############################################################################
#F OmegaPrimalityOfAffineSemigroup
#
# Computes the omega primality of the affine semigroup a
###########################################################################
#DeclareGlobalFunction("OmegaPrimalityOfAffineSemigroup");
# moved to affine.gd

###############################################################################
#F PrimitiveElementsOfAffineSemigroup_Normaliz
# Computes the primitive elements of the affine semigroup a
# #####################################################################
# DeclareGlobalFunction("PrimitiveElementsOfAffineSemigroup");
# moved to affine.gd

###############################################################################
#F PrimitiveElementsOfAffineSemigroup_Normaliz
# An implementation of PrimitiveElementsOfAffineSemigroup using 
# Normaliz
#####################################################################
#DeclareGlobalFunction("PrimitiveElementsOfAffineSemigroup_Normaliz");

###############################################################################
#F TameDegreeOfAffineSemigroup
# Computes the tame degree of the affine semigroup a
###########################################################################
#DeclareGlobalFunction("TameDegreeOfAffineSemigroup");
# moved to affine.gd

###############################################################################
#F ElasticityOfAffineSemigroup_Normaliz
# Computes the elasticity of the affine semigroup a
###########################################################################
#DeclareGlobalFunction("ElasticityOfAffineSemigroup_Normaliz");

###############################################################################
#F ElasticityOfAffineSemigroup
# Computes the elasticity of the affine semigroup a
###########################################################################
#DeclareGlobalFunction("ElasticityOfAffineSemigroup");
#moved to affine.gd
