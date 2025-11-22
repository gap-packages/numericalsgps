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
DeclareRepresentation( "IsNumericalSetRep", IsAttributeStoringRep, []);


#############################################################################
##
#C  IsNumericalSemigroup
##
##  The category of numerical semigroups.
##
#############################################################################
DeclareCategory( "IsNumericalSet", IsNumericalSetRep);


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