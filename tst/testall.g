TestMyPackage := function( pkgname )
local pkgdir, testfiles, testresult, ff, fn;
LoadPackage( pkgname );
pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );

# Arrange chapters as required
testfiles := [
"tst_fining1.tst",
"tst_fining2.tst",
"tst_fining3.tst",
"tst_fining4.tst",
"tst_fining5.tst",
"tst_fining6.tst",
"tst_fining7.tst",
"tst_fining8.tst",
"tst_fining9.tst",
"tst_fining10.tst",
"tst_fining11.tst",
"tst_fining12.tst"
];

testresult:=true;
for ff in testfiles do
  fn := Filename( pkgdir, ff );
  Print("#I  Testing ", fn, "\n");
  if not Test( fn, rec(compareFunction := "uptowhitespace") ) then
    testresult:=false;
  fi;
od;
if testresult then
  Print("#I  No errors detected while testing package ", pkgname, "\n");
else
  Print("#I  Errors detected while testing package ", pkgname, "\n");
fi;
end;

# Set the name of the package here
TestMyPackage( "fining" );