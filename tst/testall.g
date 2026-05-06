LoadPackage( "numericalsgps" );

tstdir := DirectoriesPackageLibrary( "numericalsgps", "tst" );
tstdir2 := DirectoriesPackageLibrary( "numericalsgps", "tst-normaliz" );
passed:=true;

passed:=passed and TestDirectory(tstdir,
  rec(testOptions := rec(compareFunction := "uptowhitespace"), exclude := ["singular.tst"] ) );

if NumSgpsUseNormaliz()=true then
  Print("Testing with Normaliz\n");
  passed:=passed and TestDirectory(tstdir2, rec(testOptions := rec(compareFunction := "uptowhitespace") ) );
else
  Print("Normaliz not available\n");
fi;

if NumSgpsUse4ti2()=true then
    Print("Testing with 4ti2\n");
    passed:=passed and TestDirectory(tstdir2, rec(testOptions := rec(compareFunction := "uptowhitespace") ) );
else
  Print("4ti2 not available\n");
fi;

if NumSgpsUseSingular()=true and
  Filename(DirectoriesSystemPrograms(),"Singular")<>fail and
  IsExecutableFile(Filename(DirectoriesSystemPrograms(),"Singular")) then
    Print("Testing with Singular\n");
    passed:=passed and TestDirectory(tstdir2, rec(testOptions := rec(compareFunction := "uptowhitespace") ) );
    passed:=passed and Test(Filename(tstdir, "singular.tst"));
else
  Print("Singular not available\n");
fi;




FORCE_QUIT_GAP(passed); 

