#######################################################################
InstallGlobalFunction(NumSgpsInfo, 
        function(n)
  SetInfoLevel(InfoNumSgps, n);
  Info(InfoNumSgps,1, "Info Level for InfoNumSgps is set to ",n, "\n");
end);
#######################################################################
InstallGlobalFunction(NumSgpsTest,
        function()
  SetInfoLevel(InfoNumSgps, 0);
  Test(Concatenation(PackageInfo("numericalsgps")[1]!.
          InstallationPath, "/tst/testall.tst"),rec(compareFunction:="uptowhitespace"));
end);

