#############################################################################
##
#W  read.g                  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: read.g,v 0.98 $
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
ReadPackage( "numericalsgps", "gap/infolevelnumsgps" );
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
ReadPackage( "numericalsgps", "gap/contributions.gi" );
ReadPackage( "numericalsgps", "gap/numsgps-utils.gi" );
ReadPackage( "numericalsgps", "gap/polynomials.gi" );
##
## Affine
##
ReadPackage( "numericalsgps", "gap/affine-def.gi" );
ReadPackage( "numericalsgps", "gap/affine.gi" );
if NumSgpsCanUseNI then
    ReadPackage("numericalsgps", "gap/affine-extra-ni.gi");
else
    Info(InfoAffSgps,1,"Please load package NormalizInterface");
    Info(InfoAffSgps,1,"to have extended functionalities.");
fi;
if NumSgpsCanUse4ti2 then 
    ReadPackage("numericalsgps", "gap/affine-extra-4ti2.gi");
else
    Info(InfoAffSgps,1,"Please load package 4ti2Interface");
    Info(InfoAffSgps,1,"to have extended functionalities.");   
fi;
if NumSgpsCanUseSI then
    ReadPackage("numericalsgps", "gap/affine-extra-si.gi");
else
    Info(InfoAffSgps,1,"Please load package SingularInterface");
    Info(InfoAffSgps,1,"to have extended functionalities.");
	if NumSgpsCanUseSingular then
    ReadPackage("numericalsgps", "gap/affine-extra-s.gi");
	else
    	Info(InfoAffSgps,1,"Please load package singular");
    	Info(InfoAffSgps,1,"to have extended functionalities.");
	fi;
fi;
    



#E  read.g  . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
