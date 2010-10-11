##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##  
##  Call this with GAP.
##  We will use PackageInfo to find fining's path.

finingpath := PackageInfo("fining")[1].InstallationPath;
path := Concatenation(finingpath,"/doc");

## It is possible that GAP is started up using multiple path. e.g. using
## gap4r4 -l "./Gap;./opt/gap4r4"
## Issuing GAP_ROOT_PATH; yields then [ "./Gap", "/opt/gap4r4" ] (the same order as startup command!
## I will ask the path as the first item in this list, so it is your responsability to have installed fining
## there!
## 

MakeGAPDocDoc(path, "fining", [], "FinInG");

#to create the tex file:
path := Directory("~/gap4r4/pkg/desargues/doc/");
main := "desargues.xml";
files := [];
bookname := "desargues";
doc := ComposedDocument("GAPDoc", path, main, files, true);;
r := ParseTreeXMLString(doc[1], doc[2]);
CheckAndCleanGapDocTree(r);
l := GAPDoc2LaTeX(r);;
FileString(Filename(path, Concatenation(bookname, ".tex")), l);

GAPDocManualLab("fining");

quit;
