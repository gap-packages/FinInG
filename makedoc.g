##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##  
##  Call this with GAP.
##  We will use PackageInfo to find fining's path.

finingpath := PackageInfo("fining")[1].InstallationPath;
path := Concatenation(finingpath,"/doc");

MakeGAPDocDoc(path, "fining", [], "FinInG");

#to create the tex file (was used by jdebeule, and not yet adapted to fining directory tree)
#november 9th, I changed this to "fining" now, but please check whether the paths apply
#to your installation... (jdb)
path := Directory("~/pkg/fining/doc/");
main := "fining.xml";
files := [];
bookname := "fining";
doc := ComposedDocument("GAPDoc", path, main, files, true);;
r := ParseTreeXMLString(doc[1], doc[2]);
CheckAndCleanGapDocTree(r);
l := GAPDoc2LaTeX(r);;
FileString(Filename(path, Concatenation(bookname, ".tex")), l);

GAPDocManualLab("fining");

quit;
