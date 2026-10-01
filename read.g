#############################################################################
##
#W  read.g                  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#Y  Copyright 2005 by Manuel Delgado,
#Y  Pedro A. Garcia-Sanchez and Jose Joao Morais
#Y  We adopt the copyright regulations of GAP as detailed in the
#Y  copyright notice in the GAP manual.
##
#############################################################################

#############################################################################
##
#R  Read the installation files.
##
#############################################################################
ReadPackage( "numericalsgps", "gap/preliminaries.gi" );
ReadPackage( "numericalsgps", "gap/numsgp-def.gi" );
ReadPackage( "numericalsgps", "gap/elements.gi" );
ReadPackage( "numericalsgps", "gap/basics.gi" );
ReadPackage( "numericalsgps", "gap/basics2.gi" );
ReadPackage( "numericalsgps", "gap/operations.gi" );
ReadPackage( "numericalsgps", "gap/random.gi" );
ReadPackage( "numericalsgps", "gap/presentaciones.gi" );
ReadPackage( "numericalsgps", "gap/irreducibles.gi" );
ReadPackage( "numericalsgps", "gap/ideals-def.gi" );
ReadPackage( "numericalsgps", "gap/arf-med.gi" );
ReadPackage( "numericalsgps", "gap/catenary-tame.gi" );
ReadPackage( "numericalsgps", "gap/pseudoFrobenius.gi" );
ReadPackage( "numericalsgps", "gap/contributions.gi" );
ReadPackage( "numericalsgps", "gap/numsgps-utils.gi" );
ReadPackage( "numericalsgps", "gap/polynomials.gi" );
ReadPackage( "numericalsgps", "gap/other-families-ns.gi" );
ReadPackage( "numericalsgps", "gap/order.gi" );
##
ReadPackage( "numericalsgps", "gap/databases.gi" );
##
## Good semigroups N^2
##
ReadPackage( "numericalsgps", "gap/good-semigroups.gi");
ReadPackage( "numericalsgps", "gap/good-ideals.gi");
##
## Affine
##
SetInfoLevel(InfoNumSgps,1);
ReadPackage( "numericalsgps", "gap/affine-def.gi" );
ReadPackage( "numericalsgps", "gap/affine.gi" );
ReadPackage( "numericalsgps", "gap/ideals-affine.gi" );
##
## obsolet
##
#ReadPackage( "numericalsgps", "gap/obsolet.gi" );
##
## dot
##
ReadPackage( "numericalsgps", "gap/dot.gi" );
##
## Numerical sets
##
ReadPackage( "numericalsgps", "gap/numset.gi" );
##
## optional packages are handled by the extensions listed in PackageInfo.g
##


#E  read.g  . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
