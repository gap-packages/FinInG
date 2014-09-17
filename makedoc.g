##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##  
##  Call this with GAP.
##  We will use PackageInfo to find fining's path.

finingpath := PackageInfo("fining")[1].InstallationPath;
path := Concatenation(finingpath,"/doc");
MakeGAPDocDoc(path, "fining", [], "FinInG");
GAPDocManualLab("fining");
quit;
