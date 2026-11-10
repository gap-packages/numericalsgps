#############################################################################
##
#W  init.g                  Manuel Delgado <mdelgado@fc.up.pt>
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
#R  Read the declaration files.
##
#############################################################################
ReadPackage( "numericalsgps", "gap/preliminaries.gd" );
ReadPackage( "numericalsgps", "gap/numsgp-def.gd" );
ReadPackage( "numericalsgps", "gap/elements.gd" );
ReadPackage( "numericalsgps", "gap/basics.gd" );
ReadPackage( "numericalsgps", "gap/basics2.gd" );
ReadPackage( "numericalsgps", "gap/operations.gd" );
ReadPackage( "numericalsgps", "gap/random.gd" );
ReadPackage( "numericalsgps", "gap/presentaciones.gd" );
ReadPackage( "numericalsgps", "gap/irreducibles.gd" );
ReadPackage( "numericalsgps", "gap/ideals-def.gd" );
ReadPackage( "numericalsgps", "gap/arf-med.gd" );
ReadPackage( "numericalsgps", "gap/catenary-tame.gd" );
ReadPackage( "numericalsgps", "gap/pseudoFrobenius.gd" );
ReadPackage( "numericalsgps", "gap/contributions.gd" );
ReadPackage( "numericalsgps", "gap/numsgps-utils.gd" );
ReadPackage( "numericalsgps", "gap/polynomials.gd" );
ReadPackage( "numericalsgps", "gap/other-families-ns.gd" );
ReadPackage( "numericalsgps", "gap/order.gd" );
##
ReadPackage( "numericalsgps", "gap/databases.gd" );
##
## Good semigroups N^2
##
ReadPackage( "numericalsgps", "gap/good-semigroups.gd" );
ReadPackage( "numericalsgps", "gap/good-ideals.gd" );
##
## Affine
##
NumSgpsCanUseNI:=false;
NumSgpsCanUseSingular:=false;
NumSgpsCanUseSI:=false;
NumSgpsCanUse4ti2:=false;
NumSgpsCanUse4ti2gap:=false;
# NumSgpsCanUseGradedModules:=false;

###
# handling extensions with optional packages
###
NumSgpsSingularExtensionLoaded:=false;
NumSgpsNormalizExtensionLoaded:=false;
NumSgps4ti2ExtensionLoaded:=false;

ReadPackage( "numericalsgps", "gap/affine-def.gd" );
ReadPackage( "numericalsgps", "gap/affine.gd" );
ReadPackage( "numericalsgps", "gap/ideals-affine.gd" );

##
## obsolet
##
# ReadPackage( "numericalsgps", "gap/obsolet.gd" );
##
## dot
##
ReadPackage( "numericalsgps", "gap/dot.gd" );

##
## Numerical sets
##
ReadPackage( "numericalsgps", "gap/numset.gd" );


#E  init.g  . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
