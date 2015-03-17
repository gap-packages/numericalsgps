#######################################################################
InstallGlobalFunction(NumSgpsMakeDoc,
        function()
  MakeGAPDocDoc(Concatenation(PackageInfo("numericalsgps")[1]!.
          InstallationPath, "/doc"), "NumericalSgpsMan.xml",
          [], "NumericalSgps",     "MathJax");;
end);
#######################################################################
InstallGlobalFunction(NumSgpsCopyHTMLStyleFiles,
        function()
  CopyHTMLStyleFiles(Concatenation(PackageInfo("numericalsgps")[1]!.
          InstallationPath, "/doc"));
end);
#######################################################################
InstallGlobalFunction(NumSgpsInfo, 
        function(n)
  SetInfoLevel(InfoNumSgps, n);
  Info(InfoNumSgps,1, "Info Level for InfoNumSgps is set to ",n, "\n");
end);
#######################################################################
InstallGlobalFunction(NumSgpsTest,
        function()
  Test(Concatenation(PackageInfo("numericalsgps")[1]!.
          InstallationPath, "/tst/testall.tst"),rec(compareFunction:="uptowhitespace"));
end);

