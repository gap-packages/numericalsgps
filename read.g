#############################################################################
##
#W  read.g                  Manuel Delgado <mdelgado@fc.up.pt>
#W                          Pedro A. Garcia-Sanchez <pedro@ugr.es>
#W                          Jose Morais <josejoao@fc.up.pt>
##
##
#H  @(#)$Id: read.g,v 1.0 $
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
ReadPackage( "numericalsgps", "gap/pseudoFrobenius.gi" );
ReadPackage( "numericalsgps", "gap/contributions.gi" );
ReadPackage( "numericalsgps", "gap/numsgps-utils.gi" );
ReadPackage( "numericalsgps", "gap/polynomials.gi" );
##
## Affine
##
SetInfoLevel(InfoNumSgps,1);
ReadPackage( "numericalsgps", "gap/affine-def.gi" );
ReadPackage( "numericalsgps", "gap/affine.gi" );
if NumSgpsCanUseNI then
    ReadPackage("numericalsgps", "gap/affine-extra-ni.gi");
    Info(InfoNumSgps,1,"Loaded interface to Normaliz (NormalizInterface)");    
fi;
if NumSgpsCanUse4ti2 then
    ReadPackage("numericalsgps", "gap/affine-extra-4ti2.gi");
    Info(InfoNumSgps,1,"Loaded interface to 4ti2 (4ti2Interface)");

fi;
if NumSgpsCanUse4ti2gap then
    ReadPackage("numericalsgps", "gap/affine-extra-4ti2gap.gi");
    Info(InfoNumSgps,1,"Loaded interface to 4ti2 (4ti2gap)");

fi;
if not(NumSgpsCanUseNI or NumSgpsCanUse4ti2 or NumSgpsCanUse4ti2gap) then
    Info(InfoNumSgps,1,"Please load package NormalizInterface or 4ti2Interface");
    Info(InfoNumSgps,1,"to have extended functionalities.");
fi;

NumSgpsWarnUseSingular:=true;

if NumSgpsCanUseSI then
    ReadPackage("numericalsgps", "gap/affine-extra-si.gi");
    Info(InfoNumSgps,1,"Loaded interface to Singular (SingularInterface)");
    NumSgpsWarnUseSingular:=false;
else
    if NumSgpsCanUseSingular then
        ReadPackage("numericalsgps", "gap/affine-extra-s.gi");
        Info(InfoNumSgps,1,"Loaded interface to Singular (Singular)");
        NumSgpsWarnUseSingular:=false;    
    else
        if NumSgpsCanUseGradedModules then
            #NumSgpsRationals:=HomalgFieldOfRationalsInSingular();            
            ReadPackage("numericalsgps", "gap/affine-extra-gm.gi");
            Info(InfoNumSgps,1,"Loaded interface to Singular (through GradedModules)");
            NumSgpsWarnUseSingular:=false;
        fi;
    fi;
fi;

if NumSgpsWarnUseSingular then
    Info(InfoNumSgps,1,"Please load package SingularInterface or singular (not both)");
    Info(InfoNumSgps,1,"or GradedModules to have extended functionalities.");
fi;



#E  read.g  . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
