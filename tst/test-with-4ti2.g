if LoadPackage( "NumericalSgps" ) = fail  then
    Print("Error, could not load NumericalSgps\n");
    QUIT_GAP(fail);
fi;

if not NumSgpsUse4ti2() then
    Print("Error, could not enable 4ti2 support in NumericalSgps\n");
    QUIT_GAP(fail);
fi;

TestDirectory(DirectoriesPackageLibrary( "numericalsgps", "tst/4ti2" ),
  rec(exitGAP     := true,
      # exclude     := [ "singular.tst" ],
      testOptions := rec(compareFunction := "uptowhitespace") ) );

FORCE_QUIT_GAP(1); # if we ever get here, there was an error
