if not(NumSgps4ti2ExtensionLoaded) then
    ReadPackage("numericalsgps", "gap/affine-extra-4ti2.gi");
    Info(InfoNumSgps,1,"Loaded interface to 4ti2 (4ti2Interface)");
    NumSgps4ti2ExtensionLoaded:=true;
fi;
