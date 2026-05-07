if not(NumSgpsNormalizExtensionLoaded) then
    ReadPackage("numericalsgps", "gap/affine-extra-ni.gi");
	ReadPackage("numericalsgps", "gap/ideals-extra-ni.gi");
	Info(InfoNumSgps,1,"Loaded interface to Normaliz (NormalizInterface)");
	NumSgpsNormalizExtensionLoaded:=true;
fi;
