#############################################################################
##
#W  numset.gd           Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
##
##
#Y  Copyright 2025 by Manuel Delgado and Pedro Garcia-Sanchez
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################


#############################################################################
##
#R  IsNumericalSetRep
##
##  The representation of a numerical set.
##
#############################################################################
DeclareRepresentation( "IsNumericalSetRep", IsAdditiveMagma and IsAttributeStoringRep, []);


#############################################################################
##
#C  IsNumericalSemigroup
##
##  The category of numerical semigroups.
##
#############################################################################
DeclareCategory( "IsNumericalSet", IsNumericalSetRep);

#InstallTrueMethod(IsNumericalSet, IsNumericalSemigroup);

# Elements of numerical semigroups are integers, so numerical semigroups are
# collections of integers.
BindGlobal( "NumericalSetsType",
        NewType( CollectionsFamily(CyclotomicsFamily),
                 IsNumericalSet));


#############################################################################
##
#F  NumericalSetBySmallElements(L)
##
##  Returns the numerical set specified by L,
##  the list of small elements of the numerical set.
##
#############################################################################
DeclareGlobalFunction( "NumericalSetBySmallElements" );
#A
DeclareAttribute( "SmallElements", IsNumericalSet);

#############################################################################
##
#F  NumericalSetByGaps(L)
##
##  Returns the numerical set specified by L,
##  the list of gaps of the numerical set.
##
#############################################################################
DeclareGlobalFunction( "NumericalSetByGaps" );
#A
DeclareAttribute( "Gaps", IsNumericalSet);
#A
DeclareAttribute( "Conductor", IsNumericalSet);
#A
DeclareAttribute( "FrobeniusNumber", IsNumericalSet);
#A
DeclareAttribute( "Genus", IsNumericalSet);
#A
DeclareAttribute( "Multiplicity", IsNumericalSet);

#############################################################################
##
#O AsNumericalSet( S )
##
## Returns S as a numerical set.
##
#############################################################################
DeclareOperation( "AsNumericalSet" , [IsNumericalSemigroup]);

DeclareOperation( "AsNumericalSet" , [IsIdealOfNumericalSemigroup] );


#############################################################################
##
#O AsNumericalSemigroup( S )
##
## Returns S as a numerical semigroup if S+S=S; raises an error otherwise.
##
#############################################################################
DeclareOperation("AsNumericalSemigroup",[IsNumericalSet]);

#############################################################################
##
#O AsIdealOfNumericalSemigroup( R, S )
##
## Returns R as an ideal of the numerical semigroup S if R+S=R; 
## raises an error otherwise.
##
#############################################################################
# DeclareOperation("AsIdealOfNumericalSemigroup",[IsNumericalSet,IsNumericalSemigroup]);

#############################################################################
##
#O IsAssociatedNumericalSetOfNumericalSemigroup( R, S )
##
## Checks if R is an associated set of S, that is,  R is an ideal and R-R=S
##
#############################################################################
DeclareOperation("IsAssociatedNumericalSetOfNumericalSemigroup",[IsNumericalSet,IsNumericalSemigroup]);


#TODO
# 1.
# we have different behavior between AsIdealOfNumericalSemigroup and AsNumericalSemigroup
# the first returns fail if it is not an ideal ("sometimes") and the second raises an error. 
# Also if we want to have the same for numerical sets, the first should no longer be
# a function and should become an operation.
# Notice that AsIdealOfNumericalSemigroup is declared on contributions and will only 
# work for integral ideals ...
# Thus, I am really in the mood of changing the existing function; we can probably use
# the new machinery of arithmetic of numerical sets.
# AsIdealOfNumericalSemigroup could simply be 
# i->IdealOfNumericalSemigroupBySmallElements(SmallElements(i),s)
#
# 2.
# We have n*I which is I+...+I (n times), but we do not have the same thing for 
# numerical semigroups; we would probably want to have the same for numerical sets
# For ideals, 0*I we took s; for semigroups or numerical sets, this is not so clear
# what should be the setting. So, probably, in that setting we could raise an error.
#
# 3. 
# Iterators: we have iterators for ideals and numerical semigroups. We do not have one for 
# numerical sets
#
