if not(NumSgpsCanUse4ti2gap) then
    ReadPackage("numericalsgps", "gap/affine-extra-4ti2gap.gi");
    Info(InfoNumSgps,1,"Loaded interface to 4ti2 (4ti2gap)");
    NumSgpsCanUse4ti2gap:=true;
fi;
