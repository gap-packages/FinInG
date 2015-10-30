LoadPackage("fining");
dir := DirectoriesPackageLibrary( "fining", "tst" )[1];

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
"tst_fining11.tst"
];

for f in testfiles do
	file := Filename(dir,f);
	ReadTest(file);
od;