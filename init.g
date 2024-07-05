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
## optiOnal packages
##

if IsPackageMarkedForLoading("NormalizInterface","0.0") then
	NumSgpsCanUseNI:=true;
fi;
if IsPackageMarkedForLoading("4ti2Interface","0.0") then
	NumSgpsCanUse4ti2:=true;
fi;
if IsPackageMarkedForLoading("4ti2gap","0.0") then
	NumSgpsCanUse4ti2gap:=true;
fi;
if IsPackageMarkedForLoading("SingularInterface","0.0") 	then
	NumSgpsCanUseSI:=true;
fi;
if IsPackageMarkedForLoading("singular","0.0") 	then
	NumSgpsCanUseSingular:=true;
fi;
# if IsPackageMarkedForLoading("GradedModules","0.0") 	then
# 	NumSgpsCanUseGradedModules:=true;
# fi;


#E  init.g  . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
