TestMyPackage := function( pkgname )
local pkgdir, testfiles, testresult, ff, fn;
LoadPackage( pkgname );
pkgdir := DirectoriesPackageLibrary( pkgname, "tst" );

# Arrange chapters as required
testfiles := [
"tst_regular13system.tst",
"tst_segrevariety.tst",
"tst_hermitianspreads.tst",
"tst_andrebruckbose.tst",
"tst_titsovoid.tst",
"tst_finingorbits.tst",
"tst_enumerators.tst"
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