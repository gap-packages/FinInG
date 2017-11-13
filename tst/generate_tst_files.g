#create output and tst files from .g files in the tst/gap directory.
#Executing this file is NOT necessary for the installation of the package "fining" (including the documentation)

#Performing these steps, files will be written in the tst directory of the
#fining package tree. Under UNIX-like operating systems, you need sufficient
#permissions to do. Executing this is NOT necessary for the installation of the
#package "fining". This file is based on the generate_examples_files.g in the /fining/examples directory.

#to make sure there is no confusing with linebreaks, it seems better to have all input (in the .g files)
#in one (long) line.

#Messy things happen when you do it, so don't try this at home kids!

#create workspace with packages
LoadPackage("fining");
SaveWorkspace("fining.ws");
quit;

#restart gap now.

#initialize filenames

#initialisations to create .tst files from .g files in tst directory. Output will be in outputdir.

files := ["tst_fining1", "tst_fining2", "tst_fining3", "tst_fining4", "tst_fining5",
            "tst_fining6","tst_fining7", "tst_fining8", "tst_fining9", "tst_fining10",
            "tst_fining11", "tst_fining12" ];

files := ["tst_regular13system", "tst_segrevariety", "tst_hermitianspreads", "tst_andrebruckbose",
            "tst_titsovoid", "tst_finingorbits", "tst_enumerators"];

#initialize directorynames
#sourcedir = dir where .g files are located : ".../pkg/fining/tst/gap"
#preambledir = directory where 'preamble_sws.g is found' :  ".../pkg/fining/examples"
#outputdir = directory to write '.out' files: ".../pkg/fining/examples/output"

homedir := DirectoryCurrent();
sourcedir := DirectoriesPackageLibrary("fining","tst/gap")[1];
preambledir := DirectoriesPackageLibrary("fining","examples/")[1];
outputdir := DirectoriesPackageLibrary("fining","tst/output")[1];
paths := JoinStringsWithSeparator(GAPInfo.RootPaths{[3,4]},";");
args := JoinStringsWithSeparator(["-l",paths," -L fining.ws"," -o 4G"]," ");
args := ["-l",paths,"-L","fining.ws","-o","4G"];
extension := ".out\";";
cmddir := "dir \:\= DirectoriesPackageLibrary\(\"fining\"\,\"tst\/output\"\)\[1\]\;";

#name of script to start gap version, might be different on your computer
gapstart := "gap4r8";
Exec(Concatenation("which ",gapstart));
gap := Filename(Directory("/usr/bin/"),gapstart);
gap := Filename(Directory("/usr/local/bin/"),gapstart);

#create .out files using the saved workspace
#IMPORTANT: here we suppose that the script to start up our favorite version of
#GAP is called 'gap4r4', and is located in '/usr/bin'. Change the code if this is not true!
#you certainly now the name of the script, since you started gap. To find the
#dir, just issue in the gap session that is running:
#Exec("which gapstart); #for UNIX
#Note that test files that require a lot of time will keep running in background and produce their output
#also when the for loop is completely done.

for filename in files do
  Print("Now converting file: ", filename, "\n");
  stream := InputOutputLocalProcess( homedir, gap, args);
  #cmd := Concatenation("file := \"",filename,".out\";");
  cmd := Concatenation("file := \"",filename,extension);
  WriteLine(stream,cmd);
  #cmd := "dir \:\= DirectoriesPackageLibrary\(\"fining\"\,\"examples\/output\"\)\[1\]\;";
  WriteLine(stream,cmddir);
  preamble := Filename(preambledir,"preamble.g");
  preamble_stream := InputTextFile(preamble);
  cmds := ReadAll(preamble_stream);
  WriteLine(stream,cmds);
  repeat
    str := ReadLine(stream);
  until str = "true\n";
  inputfile := Filename(sourcedir,Concatenation(filename,".g"));
  input_stream := InputTextFile(inputfile);
  cmd := ReadLine(input_stream);
  while cmd <> fail do
    WriteAll(stream,cmd);
    cmd := ReadLine(input_stream);
    ReadAll(stream);
  od;
  repeat until ReadAll(stream)=fail; #new since oct 2015.
od;

#create .tst files
#the nested ifs together with the SizeScreen make sure that input lines (plural),
#are written back in the tst file as one (long) input line.
includedir := DirectoriesPackageLibrary("fining","tst")[1];
for filename in files do
  i := Filename(outputdir,Concatenation(filename,".out"));
  o := Filename(includedir,Concatenation(filename,".tst"));
  PrintTo(o,"");
  input_stream := InputTextFile(i);
  ReadLine(input_stream); #reads first line which is a line with a #
  #ReadLine(input_stream);
  AppendTo(o,Concatenation("gap> START_TEST(\"fining: ",filename,".tst\");\n"));
  line := ReadLine(input_stream);
  while line <> "gap> quit;\n" do
    if Length(line) > 3 then
        if line{[1..4]} = "gap>" then
            RemoveCharacters(line,"\n");
            line := Concatenation(line,"\n");
        fi;
    fi;
    SizeScreen([500,24]);
    AppendTo(o,line);
    SizeScreen([80,24]);
    line := ReadLine(input_stream);
  od;
  AppendTo(o,Concatenation("gap> STOP_TEST(\"",filename,".tst\", 10000 );\n"));
od;

#now write testall.g file.

o := Filename(includedir,"testall.g");
tstdir := DirectoriesPackageLibrary("fining","tst")[1];
part1 := Filename(tstdir,"template_part1.g");
input_stream := InputTextFile(part1);
PrintTo(o,ReadAll(input_stream));

AppendTo(o,"testfiles := [\n");
for i in [1..Length(files)-1] do
    filename := Concatenation(files[i],".tst");
    AppendTo(o,Concatenation("\"",filename,"\",\n"));
od;
filename := Concatenation(files[Length(files)],".tst");
AppendTo(o,Concatenation("\"",filename,"\"\n];\n\n"));

part2 := Filename(tstdir,"template_part2.g");
input_stream := InputTextFile(part2);
AppendTo(o,ReadAll(input_stream));


#old and unused stuff. 

#AppendTo(o,"TestMyPackage(\"fining\")");

#AppendTo(o,"LoadPackage(\"fining\");\n");

#AppendTo(o,"dir := DirectoriesPackageLibrary( \"fining\", \"tst\" )[1];\n\n");
#AppendTo(o,"testfiles := [\n");
#for i in [1..Length(files)-1] do
#    filename := Concatenation(files[i],".tst");
#    AppendTo(o,Concatenation("\"",filename,"\",\n"));
#od;
#filename := Concatenation(files[Length(files)],".tst");
#AppendTo(o,Concatenation("\"",filename,"\"\n];\n\n"));
#AppendTo(o,"for f in testfiles do\n\tfile := Filename(dir,f);\n\tTest(file, rec( compareFunction := \"uptowhitespace\" ));\nod;");

