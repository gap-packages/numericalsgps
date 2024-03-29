#############################################################################
##
#W  affine-def.gd           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
#Y  Copyright 2015-- Centro de Matemática da Universidade do Porto, Portugal and Universidad de Granada, Spain
#############################################################################

#############################################################################
##
#R  IsAffineSemigroupRep
##
##  The representation of an affine semigroup.
##
#############################################################################
DeclareRepresentation( "IsAffineSemigroupRep", IsAttributeStoringRep, [] );

#############################################################################
##
#C  IsAffineSemigroup
##
##  The category of affine semigroups.
##
#############################################################################
DeclareCategory( "IsAffineSemigroup", IsAdditiveMagma and IsAffineSemigroupRep) ;

# Elements of affine semigroups are collections of integers, so affine
# semigroups are collections of collections of integers.
BindGlobal( "AffineSemigroupsType",
        NewType( CollectionsFamily(CollectionsFamily(CyclotomicsFamily)),
                 IsAffineSemigroup));


#############################################################################
##
#F  AffineSemigroupByGenerators(arg)
##
##  Returns the affine semigroup generated by arg.
##
#############################################################################
DeclareGlobalFunction( "AffineSemigroupByGenerators" );
#A
DeclareAttribute( "Generators", IsAffineSemigroup);
DeclareSynonymAttr( "GeneratorsOfAffineSemigroup", Generators);
DeclareSynonymAttr( "IsAffineSemigroupByGenerators", HasGenerators);
#A
DeclareAttribute( "Dimension", IsAffineSemigroup);
DeclareSynonymAttr( "DimensionOfAffineSemigroup", Dimension);

#############################################################################
##
#F  AffineSemigroupByMinimalGenerators(arg)
##
##  Returns the affine semigroup minimally generated by arg.
##  If the generators given are not minimal, the minimal ones
##  are computed and used.
##
#############################################################################
# DeclareGlobalFunction( "AffineSemigroupByMinimalGenerators" );
#A
DeclareAttribute( "MinimalGenerators", IsAffineSemigroup);
DeclareSynonymAttr( "IsAffineSemigroupByMinimalGenerators", HasMinimalGenerators);

###############################################################################
#A PseudoFrobenius
# The set of PseudoFrobeniusVectors
# Works only if the affine semigroup has finitely many gaps
###############################################################################
DeclareAttribute("PseudoFrobenius", IsAffineSemigroup);

###############################################################################
#O SpecialGaps
# The set of special gaps
# Works only if the affine semigroup has finitely many gaps
###############################################################################
DeclareAttribute("SpecialGaps", IsAffineSemigroup);

###############################################################################
#
# RemoveMinimalGeneratorFromAffineSemigroup(x,s)
#
# Compute the affine semigroup obtained by removing the minimal generator x from 
# the given affine semigroup s. If s has finite gaps, its set of gaps is setted 
# 
###############################################################################
DeclareGlobalFunction("RemoveMinimalGeneratorFromAffineSemigroup");

############################################################################## 
#
#  AddSpecialGapOfAffineSemigroup(x,s)
#
# Let a an affine semigroup with finite gaps and x be a special gap of a.
# We compute the unitary extension of a with x
################################################################################
DeclareGlobalFunction("AddSpecialGapOfAffineSemigroup");

#############################################################################
##
#F  AffineSemigroupByGaps(arg)
##
##  Returns the affine semigroup determined by the gaps arg.
##  If the given set is not a set of gaps, then an error is raised.
##
#############################################################################
DeclareGlobalFunction( "AffineSemigroupByGaps" );
#A
DeclareAttribute( "Gaps", IsAffineSemigroup);
#A
DeclareAttribute( "Genus", IsAffineSemigroup);


#############################################################################
##
#F  AffineSemigroupByMinimalGeneratorsNC(arg)
##
##  Returns the affine semigroup minimally generated by arg.
##  No test is made about args' minimality.
##
#############################################################################
# DeclareGlobalFunction( "AffineSemigroupByMinimalGeneratorsNC" );

#############################################################################
##
#O  Generators(S)
##
##  Computes a set of generators of the affine semigroup S.
##  If a set of generators has already been computed, this
##  is the set returned.
############################################################################
#DeclareOperation("Generators",[IsAffineSemigroup]);
# #A
# DeclareAttribute( "Generators", IsAffineSemigroup);
# DeclareSynonymAttr( "IsAffineSemigroupByGenerators", HasGenerators);

#############################################################################
## Full ffine semigroups
#############################################################################
##
#F  AffineSemigroupByEquations(ls,md)
##
##  Returns the (full) affine semigroup defined by the system A X=0 mod md, where the rows
## of A are the elements of ls.
##
#############################################################################
DeclareGlobalFunction( "AffineSemigroupByEquations" );
#A
DeclareAttribute( "Equations", IsAffineSemigroup);
DeclareSynonymAttr( "IsAffineSemigroupByEquations", HasEquations);
#############################################################################
##
#F  AffineSemigroupByInequalities(ls)
##
##  Returns the (full) affine semigroup defined by the system  ls*X>=0 over the nonnegative
## integers
##
#############################################################################
DeclareGlobalFunction( "AffineSemigroupByInequalities" );
#A
# collission with MatricesForHomalg
#DeclareAttribute( "Inequalities", IsAffineSemigroup);
#DeclareSynonymAttr( "IsAffineSemigroupByInequalities", HasInequalities);
DeclareAttribute( "AffineSemigroupInequalities", IsAffineSemigroup);
DeclareSynonymAttr( "HasInequalities", HasAffineSemigroupInequalities);
DeclareOperation("Inequalities", [IsAffineSemigroup and HasInequalities]);

#############################################################################
##
#F  AffineSemigroupByPMInequality(f, b, g)
##
##  Returns the proportionally modular affine semigroup defined by the 
##  inequality f*x mod b <= g*x
##
#############################################################################
DeclareGlobalFunction( "AffineSemigroupByPMInequality" );
DeclareAttribute( "PMInequality", IsAffineSemigroup);

#############################################################################
##
#F  AffineSemigroup(arg)
##
##  This function's first argument may be one of:
##  "generators", "minimalgenerators",
## : equations...
##
##  The following arguments must conform to the arguments of
##  the corresponding function defined above.
##  By default, the option "generators" is used, so,
##  gap> AffineSemigroup([1,3],[7,2],[1,5]);
##  <Affine semigroup in 3-dimensional space, with 3 generators>
##
##
#############################################################################
DeclareGlobalFunction( "AffineSemigroup" );


#############################################################################
##
#P  IsFullAffineSemigroup(S)
##
##  Tests if the affine semigroup S has the property of being full.
##
#############################################################################
DeclareProperty( "IsFull", IsAffineSemigroup);
DeclareSynonymAttr("IsFullAffineSemigroup",IsFull);

#############################################################################
##
#F AsAffineSemigroup(S)
##
## Takes a numerical semigroup as argument and returns it as affine semigroup
##
#############################################################################
DeclareGlobalFunction("AsAffineSemigroup");

#############################################################################
##
#F  FiniteComplementIdealExtension(l)
##
##  The argument is a list of lists of non-negative integers, which represent
##  elements in N^n. Returns affine semigroup {0} union (l+N^n) if this has 
##  finitely many gaps, and an error otherwise (in this setting there are 
##  infinitely many minimal generators and the monoid is not an affine 
##  semigroup) 
##
#############################################################################
DeclareGlobalFunction( "FiniteComplementIdealExtension" );
