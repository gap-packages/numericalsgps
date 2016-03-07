#############################################################################
##
#W  affine-def.gd           Manuel Delgado <mdelgado@fc.up.pt>
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



DeclareAttribute( "DefinedByDuplication", IsGoodSemigroup);
DeclareAttribute( "DefinedByAmalgamation", IsGoodSemigroup);
DeclareAttribute( "DefinedByCartesianProduct", IsGoodSemigroup);
DeclareAttribute( "NumericalSemigroupGS", IsGoodSemigroup);
DeclareAttribute( "NumericalSemigroupListGS", IsGoodSemigroup);
DeclareAttribute( "IdealGS", IsGoodSemigroup);
DeclareAttribute( "MorphismGS", IsGoodSemigroup);
DeclareAttribute( "GeneratorsGS", IsGoodSemigroup);
DeclareAttribute( "Conductor", IsGoodSemigroup);
DeclareAttribute( "SmallElementsGS", IsGoodSemigroup);

DeclareSynonymAttr( "IsGoodSemigroupByAmalgamation", HasDefinedByAmalgamation);
DeclareSynonymAttr( "IsGoodSemigroupByDuplication", HasDefinedByDuplication);
DeclareSynonymAttr( "IsGoodSemigroupByCartesianProduct", HasDefinedByCartesianProduct);

DeclareOperation("BelongsToGoodSemigroup",[IsHomogeneousList,IsGoodSemigroup]);


 ####################################################
 ##
 #F NumericalDublication(S,E,b)
 ## returns 2S\cup(2E+b)
 ####################################################
DeclareGlobalFunction("NumericalDuplication");

###################################################
##
#F SemigroupDuplication(S,E)
## returns S\bowtie E
###################################################
DeclareGlobalFunction("SemigroupDuplication");

###################################################
##
#F Amalgamation(S,E,c)
## returns S\bowtie^f E, f multiplication by c
###################################################
DeclareGlobalFunction("Amalgamation");

###################################################
##
#F CartesianProduct(S1,S2)
## Computes the cartesian product of S1 and S2, which
## is a good semigroup
###################################################
DeclareGlobalFunction("CartesianProduct");

###################################################
##
#F RepresentsSmallElementsOfGoodSemigroup(X)
## detects if X is a good semiring
###################################################
DeclareGlobalFunction("RepresentsSmallElementsOfGoodSemigroup");

###################################################
##
#F GoodSemigroup(X)
## define the good semigroup from the set of points G
###################################################
DeclareGlobalFunction("GoodSemigroup");

###################################################
##
#F SmallElementsOfGoodSemigroup(M)
## returns de small elements of M, that is,
## the elements below the conductor
##################################################
DeclareGlobalFunction("SmallElementsOfGoodSemigroup");


###############################################################
##
#F IsSymmetricGoodSemigroup(M)
## Determines if M is symmetric
###############################################################
DeclareGlobalFunction("IsSymmetricGoodSemigroup");

###################################################
##
#F MinimalGoodGeneratingSystemOfGoodSemigroup(M)
## returns the unique minimal good generating of the
## good semigroup M
###################################################
DeclareGlobalFunction("MinimalGoodGeneratingSystemOfGoodSemigroup");
