# singular and SingularInterface are incompatible; use whichever came first
if not(NumSgpsSingularExtensionLoaded or NumSgpsCanUseSI) then
	ReadPackage("numericalsgps", "gap/polynomials-extra-s.gd");
	ReadPackage("numericalsgps", "gap/affine-extra-s.gi");
	ReadPackage("numericalsgps", "gap/polynomials-extra-s.gi");
    Info(InfoNumSgps,1,"Loaded interface to Singular");
	NumSgpsCanUseSingular:=true;
	NumSgpsSingularExtensionLoaded:=true;
fi;