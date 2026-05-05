LoadPackage( "numericalsgps" );

passed:=true;

passed:=passed and TestDirectory(DirectoriesPackageLibrary( "numericalsgps", "tst" ),
  rec(testOptions := rec(compareFunction := "uptowhitespace") ) );

if NumSgpsUseNormaliz()=true then
  Print("Testing with Normaliz\n");
  passed:=passed and TestDirectory(DirectoriesPackageLibrary( "numericalsgps", "tst-normaliz" ),
    rec(testOptions := rec(compareFunction := "uptowhitespace") ) );
else
  Print("Normaliz not available\n");
fi;

if NumSgpsUseSingular()=true then
  Print("Testing with Singular\n");
  passed:=passed and TestDirectory(DirectoriesPackageLibrary( "numericalsgps", "tst-normaliz" ),
    rec(testOptions := rec(compareFunction := "uptowhitespace") ) );
else
  Print("Singular not available\n");
fi;


if NumSgpsUse4ti2()=true then
  Print("Testing with 4ti2\n");
  passed:=passed and TestDirectory(DirectoriesPackageLibrary( "numericalsgps", "tst-normaliz" ),
    rec(testOptions := rec(compareFunction := "uptowhitespace") ) );
else
  Print("4ti2 not available\n");
fi;


FORCE_QUIT_GAP(passed); 

