##  this creates the documentation, needs: GAPDoc package, latex, pdflatex,
##  mkindex, dvips
##  
##  Call this with GAP.
##

## start up GAP in parent directory of ./pkg/desargues
MakeGAPDocDoc("~/pkg/desargues/doc", "fining", [], "FinInG");

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
