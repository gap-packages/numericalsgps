#############################################################################
##
#W  good-semigroups.gd           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2016-- Centro de Matemática da Universidade do Porto, Portugal and IEMath-GR, Universidad de Granada, Spain
#############################################################################

#############################################################################
##
#R  IsGoodSemigroupRep
##
##  The representation of a good  semigroup.
##
#############################################################################
DeclareRepresentation( "IsGoodSemigroupRep", IsAttributeStoringRep, [] );

#############################################################################
##
#C  IsGoodSemigroup
##
##  The category of affine semigroups.
##
#############################################################################
DeclareCategory( "IsGoodSemigroup", IsAdditiveMagma and IsGoodSemigroupRep) ;

# Elements of affine semigroups are collections of integers, so affine
# semigroups are collections of collections of integers.
BindGlobal( "GoodSemigroupsType",
        NewType( CollectionsFamily(CollectionsFamily(CyclotomicsFamily)),
                IsGoodSemigroup));

###################################################
##
#F GoodSemigroup(X)
## define the good semigroup from the set of points G
###################################################
DeclareGlobalFunction("GoodSemigroup");

## ATTRIBUTES ##
##
DeclareAttribute( "DefinedByDuplication", IsGoodSemigroup);
DeclareSynonymAttr( "IsGoodSemigroupByDuplication", HasDefinedByDuplication);
##
DeclareAttribute( "DefinedByAmalgamation", IsGoodSemigroup);
DeclareSynonymAttr( "IsGoodSemigroupByAmalgamation", HasDefinedByAmalgamation);
##
DeclareAttribute( "DefinedByCartesianProduct", IsGoodSemigroup);
DeclareSynonymAttr( "IsGoodSemigroupByCartesianProduct", HasDefinedByCartesianProduct);
##
DeclareAttribute( "Generators", IsGoodSemigroup);
##
###################################################
##
#A MinimalGenerators(M)
#A MinimalGoodGeneratingSystemOfGoodSemigroup(M)
## returns the unique minimal good generating of the
## good semigroup M
###################################################
DeclareAttribute( "MinimalGenerators", IsGoodSemigroup);
DeclareSynonymAttr("MinimalGoodGeneratingSystemOfGoodSemigroup",MinimalGenerators);

##
DeclareAttribute( "Conductor", IsGoodSemigroup);
DeclareSynonymAttr("ConductorOfGoodSemigroup",Conductor);
###################################################
##
#A SmallElements(M)
#A SmallElementsOfGoodSemigroup(M)
## returns de small elements of M, that is,
## the elements below the conductor
##################################################
DeclareAttribute( "SmallElements", IsGoodSemigroup);
DeclareSynonymAttr("SmallElementsOfGoodSemigroup",SmallElements);

########???????????
DeclareAttribute( "NumericalSemigroupGS", IsGoodSemigroup);
DeclareAttribute( "NumericalSemigroupListGS", IsGoodSemigroup);
DeclareAttribute( "IdealGS", IsGoodSemigroup);
DeclareAttribute( "MorphismGS", IsGoodSemigroup);

## OPERATIONS ##
DeclareOperation("BelongsToGoodSemigroup",[IsHomogeneousList,IsGoodSemigroup]);

## FUNCTIONS ##

 ####################################################
 ##
 #F NumericalDublication(S,E,b)
 ## returns 2S\cup(2E+b)
 ####################################################
DeclareGlobalFunction("NumericalDuplication");

###################################################
##
#F NumericalSemigroupDuplication(S,E)
## returns S\bowtie E
###################################################
DeclareGlobalFunction("NumericalSemigroupDuplication");

###################################################
##
#F AmalgamationOfNumericalSemigroups(S,E,c)
## returns S\bowtie^f E, f multiplication by c
###################################################
DeclareGlobalFunction("AmalgamationOfNumericalSemigroups");

###################################################
##
#F CartesianProductOfNumericalSemigroups(S1,S2)
## Computes the cartesian product of S1 and S2, which
## is a good semigroup
###################################################
DeclareGlobalFunction("CartesianProductOfNumericalSemigroups");

###################################################
##
#F RepresentsSmallElementsOfGoodSemigroup(X)
## detects if X is a good semiring
###################################################
DeclareGlobalFunction("RepresentsSmallElementsOfGoodSemigroup");

###############################################################
##
#F IsSymmetricGoodSemigroup(M)
## Determines if M is symmetric
###############################################################
#DeclareGlobalFunction("IsSymmetricGoodSemigroup");
DeclareProperty("IsSymmetricGoodSemigroup",IsGoodSemigroup);
DeclareAttribute("IsSymmetric",IsGoodSemigroup);

###############################################################
##
#F GoodSemigroupBySmallElements(M)
## Constructs good semigroup from a set of small elements
###############################################################
DeclareGlobalFunction("GoodSemigroupBySmallElements");


###############################################################
##
#F ArfGoodSemigroupClosure(M)
## Constructs Arf good semigroup closure of M
###############################################################
DeclareGlobalFunction("ArfGoodSemigroupClosure");
DeclareOperation("ArfClosure", [IsGoodSemigroup]);

###############################################################
##
#F MaximalElementsOfGoodSemigroup(M)
## returns the set of maximal elements of M
###############################################################
DeclareGlobalFunction("MaximalElementsOfGoodSemigroup");

###############################################################
##
#F IrreducibleMaximalElementsOfGoodSemigroup(M)
## returns the set of irreducible maximal elements of M
###############################################################
DeclareGlobalFunction("IrreducibleMaximalElementsOfGoodSemigroup");

###############################################################
##
#F GoodSemigroupByMaximalElements(S1,S2,mx)
## returns the good semigroup determined by removing from
## S1 x S2 the set of points "above" a maximal element
###############################################################
DeclareGlobalFunction("GoodSemigroupByMaximalElements");


# ## FUNCTIONS ##

#  ####################################################
#  ##
#  #F NumericalDublication(S,E,b)
#  ## returns 2S\cup(2E+b)
#  ####################################################
# DeclareGlobalFunction("NumericalDuplication");

# ###################################################
# ##
# #F SemigroupDuplication(S,E)
# ## returns S\bowtie E
# ###################################################
# DeclareGlobalFunction("SemigroupDuplication");

# ###################################################
# ##
# #F Amalgamation(S,E,c)
# ## returns S\bowtie^f E, f multiplication by c
# ###################################################
# DeclareGlobalFunction("Amalgamation");

# ###################################################
# ##
# #F CartesianProduct(S1,S2)
# ## Computes the cartesian product of S1 and S2, which
# ## is a good semigroup
# ###################################################
# DeclareGlobalFunction("CartesianProduct");

# ###################################################
# ##
# #F RepresentsSmallElementsOfGoodSemigroup(X)
# ## detects if X is a good semiring
# ###################################################
# DeclareGlobalFunction("RepresentsSmallElementsOfGoodSemigroup");









# #############

# DeclareAttribute( "DefinedByDuplication", IsGoodSemigroup);
# DeclareAttribute( "DefinedByAmalgamation", IsGoodSemigroup);
# DeclareAttribute( "DefinedByCartesianProduct", IsGoodSemigroup);
# DeclareAttribute( "NumericalSemigroupGS", IsGoodSemigroup);
# DeclareAttribute( "NumericalSemigroupListGS", IsGoodSemigroup);
# DeclareAttribute( "IdealGS", IsGoodSemigroup);
# DeclareAttribute( "MorphismGS", IsGoodSemigroup);
# DeclareAttribute( "GeneratorsGS", IsGoodSemigroup);
# DeclareAttribute( "Conductor", IsGoodSemigroup);

# DeclareSynonymAttr( "IsGoodSemigroupByAmalgamation", HasDefinedByAmalgamation);
# DeclareSynonymAttr( "IsGoodSemigroupByDuplication", HasDefinedByDuplication);
# DeclareSynonymAttr( "IsGoodSemigroupByCartesianProduct", HasDefinedByCartesianProduct);

# DeclareOperation("BelongsToGoodSemigroup",[IsHomogeneousList,IsGoodSemigroup]);


#  ####################################################
#  ##
#  #F NumericalDublication(S,E,b)
#  ## returns 2S\cup(2E+b)
#  ####################################################
# DeclareGlobalFunction("NumericalDuplication");

# ###################################################
# ##
# #F SemigroupDuplication(S,E)
# ## returns S\bowtie E
# ###################################################
# DeclareGlobalFunction("SemigroupDuplication");

# ###################################################
# ##
# #F Amalgamation(S,E,c)
# ## returns S\bowtie^f E, f multiplication by c
# ###################################################
# DeclareGlobalFunction("Amalgamation");

# ###################################################
# ##
# #F CartesianProduct(S1,S2)
# ## Computes the cartesian product of S1 and S2, which
# ## is a good semigroup
# ###################################################
# DeclareGlobalFunction("CartesianProduct");

# ###################################################
# ##
# #F RepresentsSmallElementsOfGoodSemigroup(X)
# ## detects if X is a good semiring
# ###################################################
# DeclareGlobalFunction("RepresentsSmallElementsOfGoodSemigroup");

# ###################################################
# ##
# #F GoodSemigroup(X)
# ## define the good semigroup from the set of points G
# ###################################################
# DeclareGlobalFunction("GoodSemigroup");

# ###################################################
# ##
# #A SmallElements(M)
# #A SmallElementsOfGoodSemigroup(M)
# ## returns de small elements of M, that is,
# ## the elements below the conductor
# ##################################################
# DeclareAttribute( "SmallElements", IsGoodSemigroup);
# DeclareSynonymAttr("SmallElementsOfGoodSemigroup",SmallElements);









# #############

# DeclareAttribute( "DefinedByDuplication", IsGoodSemigroup);
# DeclareAttribute( "DefinedByAmalgamation", IsGoodSemigroup);
# DeclareAttribute( "DefinedByCartesianProduct", IsGoodSemigroup);
# DeclareAttribute( "NumericalSemigroupGS", IsGoodSemigroup);
# DeclareAttribute( "NumericalSemigroupListGS", IsGoodSemigroup);
# DeclareAttribute( "IdealGS", IsGoodSemigroup);
# DeclareAttribute( "MorphismGS", IsGoodSemigroup);
# DeclareAttribute( "GeneratorsGS", IsGoodSemigroup);
# DeclareAttribute( "Conductor", IsGoodSemigroup);

# DeclareSynonymAttr( "IsGoodSemigroupByAmalgamation", HasDefinedByAmalgamation);
# DeclareSynonymAttr( "IsGoodSemigroupByDuplication", HasDefinedByDuplication);
# DeclareSynonymAttr( "IsGoodSemigroupByCartesianProduct", HasDefinedByCartesianProduct);

# DeclareOperation("BelongsToGoodSemigroup",[IsHomogeneousList,IsGoodSemigroup]);


#  ####################################################
#  ##
#  #F NumericalDublication(S,E,b)
#  ## returns 2S\cup(2E+b)
#  ####################################################
# DeclareGlobalFunction("NumericalDuplication");

# ###################################################
# ##
# #F SemigroupDuplication(S,E)
# ## returns S\bowtie E
# ###################################################
# DeclareGlobalFunction("SemigroupDuplication");

# ###################################################
# ##
# #F Amalgamation(S,E,c)
# ## returns S\bowtie^f E, f multiplication by c
# ###################################################
# DeclareGlobalFunction("Amalgamation");

# ###################################################
# ##
# #F CartesianProduct(S1,S2)
# ## Computes the cartesian product of S1 and S2, which
# ## is a good semigroup
# ###################################################
# DeclareGlobalFunction("CartesianProduct");

# ###################################################
# ##
# #F RepresentsSmallElementsOfGoodSemigroup(X)
# ## detects if X is a good semiring
# ###################################################
# DeclareGlobalFunction("RepresentsSmallElementsOfGoodSemigroup");

# ###################################################
# ##
# #F GoodSemigroup(X)
# ## define the good semigroup from the set of points G
# ###################################################
# DeclareGlobalFunction("GoodSemigroup");

# ###################################################
# ##
# #A SmallElements(M)
# #A SmallElementsOfGoodSemigroup(M)
# ## returns de small elements of M, that is,
# ## the elements below the conductor
# ##################################################
# DeclareAttribute( "SmallElements", IsGoodSemigroup);
# DeclareSynonymAttr("SmallElementsOfGoodSemigroup",SmallElements);


# ###############################################################
# ##
# #F IsSymmetricGoodSemigroup(M)
# ## Determines if M is symmetric
# ###############################################################
# DeclareGlobalFunction("IsSymmetricGoodSemigroup");

# ###################################################
# ##
# #F MinimalGoodGeneratingSystemOfGoodSemigroup(M)
# ## returns the unique minimal good generating of the
# ## good semigroup M
# ###################################################
# DeclareGlobalFunction("MinimalGoodGeneratingSystemOfGoodSemigroup");
