##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##  
##  Call this with GAP.
##  We will use PackageInfo to find fining's path.

if fail = LoadPackage("AutoDoc", ">= 2022.07.10") then
    Error("AutoDoc 2022.07.10 or newer is required");
fi;
AutoDoc(rec(
    gapdoc := rec( main := "fining.xml" ),
));
QUIT;
