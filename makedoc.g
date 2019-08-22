##  this creates the documentation, needs: GAPDoc and AutoDoc packages, pdflatex
##  
##  Call this with GAP from within the package directory.
##

if fail = LoadPackage("AutoDoc", ">= 2016.01.21") then
    Error("AutoDoc 2016.01.21 or newer is required");
fi;

AutoDoc(rec( gapdoc := rec(MainPage := false, LaTeXOptions := rec(EarlyExtraPreamble:="\\usepackage{graphicx}") )));
PrintTo("version", GAPInfo.PackageInfoCurrent.Version);
#
QUIT;
