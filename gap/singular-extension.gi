if not(NumSgpsSingularExtensionLoaded) then
	ReadPackage("numericalsgps", "gap/polynomials-extra-s.gd");
	ReadPackage("numericalsgps", "gap/affine-extra-s.gi");
	ReadPackage("numericalsgps", "gap/polynomials-extra-s.gi");
    Info(InfoNumSgps,1,"Loaded interface to Singular");
	NumSgpsCanUseSingular:=true;
	NumSgpsSingularExtensionLoaded:=true;
fi;