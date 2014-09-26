dir := DirectoriesPackageLibrary( pkgname, "tst" )[1];

testfiles := [
"tst_test1.tst",
"tst_test2.tst",
"tst_test3.tst",
"tst_test4.tst",
"tst_test5.tst"
];

for f in testfiles do
    file := Filename(dir,f);
    ReadTest(file);
od;
