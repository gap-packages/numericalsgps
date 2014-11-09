#############################################################################
##
#W  affine.gd               Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright .........
#############################################################################



###############################################################################
#O BelongsToAffineSemigroup
# Determines if the vector v is in the affine semigroup a
###########################################################################
DeclareOperation("BelongsToAffineSemigroup",[IsHomogeneousList,IsAffineSemigroup]);

###############################################################################
#F BasisOfGroupGivenByEquations
# Computes a basis of a subgroup of Z^n with defining equations 
# Ax =0 \in Z_m1\times\Z_mt \times Z^k, k is n-length(m),
# m=[m1,...,mt]
###########################################################################
DeclareGlobalFunction("BasisOfGroupGivenByEquations");

#############################################################
### 
#F EquationsOfGroupGeneratedBy
# Computes the defining equations of the group of Z^n 
# generated by M
# the output is [A,m] such that Ax=0 mod m are the equations
############################################################
DeclareGlobalFunction("EquationsOfGroupGeneratedBy");


##############################################################
###
#F GluingOfAffineSemigroups
# Determines if there is a gluing of the two affine semigroups
# if so, returns the gluin of them, and fail otherwise
##############################################################
DeclareGlobalFunction("GluingOfAffineSemigroups");

################################################################
###
#F IsFullAffineSemigroup
# Detects if the affine semigroup is full: the nonnegative 
# of the the group spanned by it coincides with the semigroup
# itself; or in other words, if a,b\in S and a-b\in \mathbb N^n,
# then a-b\in S
################################################################
## moved to affine-def
#DeclareGlobalFunction("IsFullAffineSemigroup");


###################### ContejeanDevieAlgorithm
DeclareGlobalFunction("ContejeanDevieAlgorithmForEquations");
DeclareGlobalFunction("ContejeanDevieAlgorithmForInequalities");


###############################################################################
#O FactorizationsVectorWRTList
#
# Computes the set of factorizations of v in terms of the elements of ls 
# That is, a Hilbert basis for ls*X=v
# While using normaliz:
# If ls contains vectors that generate a nonreduced monoid, then it 
# deprecates the infinite part of the solutions, or in other words, it
# returns only the minimal solutions of the above system of equations
# If not using nomaliz: then the above can cause an infinite loop
########################################################################
DeclareOperation("FactorizationsVectorWRTList",[IsHomogeneousList,IsMatrix]);
