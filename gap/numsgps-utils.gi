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
  Info(InfoNumSgps,0, "Info Level for InfoNumSgps is set to ",n, "\n");
end);
#######################################################################
InstallGlobalFunction(NumSgpsTest,
        function()
  ReadTest(Concatenation(PackageInfo("numericalsgps")[1]!.
          InstallationPath, "/tst/testall.tst"));
end);

