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


homedir := DirectoryCurrent();
scriptfile := Filename(homedir,"generate_output_fining_testfiles.sh");
PrintTo(scriptfile,"");

#these will be global variables for the generate_script function :-)

#gapstart := "gap4r13.1"; #might be different on your computer
gapstart := "gap4r14"; #might be different on your computer

gap := Filename(Directory("/usr/local/bin/"),gapstart);
paths := JoinStringsWithSeparator(GAPInfo.RootPaths{[3,4]},";");
pathsstr := Concatenation("\"",paths,"\"");

generate_script := function(sub,files)
local homedir,scriptfile,sourcedir,str,filename,inputfile,outputfile,outputfilestr,outputdir,cmdlist,cmd;
homedir := DirectoryCurrent();
str := Concatenation("generate_output_fining_testfiles_",sub,".sh");
scriptfile := Filename(homedir,str);
PrintTo(scriptfile,"");
str := Concatenation("tst/gap/",sub);
sourcedir := DirectoriesPackageLibrary("fining",str)[1];
str := Concatenation("tst/output/",sub);
outputdir := DirectoriesPackageLibrary("fining",str)[1];
for filename in files do
inputfile := Filename(sourcedir,Concatenation(filename,".g"));
outputfile := Filename(outputdir,Concatenation(filename,".out"));
outputfilestr := Concatenation("\"LogTo(","\\\"",outputfile,"\\\"",");\"");
cmdlist := [gapstart,"-l",pathsstr,"-L","fining.ws","-o","4G","-c",outputfilestr,"<",inputfile,"\n"];
cmd := JoinStringsWithSeparator(cmdlist," ");
AppendTo(scriptfile,cmd);
od;
end;

#We have severral subdirectories with .g files to convert.

subs := ["geometry", "prsp", "prgrp", "polarities", "gpolygons", "advanced"];

filesgeometry := ["incstructure", "incstrucflag", "empty", "shortcuts", "subspaces", "elements"];

filesprsp := ["pg", "prdim", "basefield", "uderlyingvs", "ambientspace",
                "vspacetoel", "emptysub", "prdim_el", "elements_inc", "elshort",
                "inc", "frame", "coordinates", "dualcoordinates", "hyperplanedual",
                "equationhyp", "ambientspaceel", "basefieldel", "random",
                "randomsubspace", "span", "meet", "flagofinc", "shadowofelement",
                "shadowofflag", "elincel", "shortnames", "iterator", "aslist"];

filesprgrp := ["actionall", "collineation", "correlation", "duality", "elations", "identity",
                "onprojsubspaces", "onprojsubspacesext", "projectivity", "projsemilinear",
                "representatives" ];

filespolarities := ["polaritiesps", "poloperations", "polaritiesnice"];

filesgpolygons := ["gpbyblocks", "gpbyelements", "gpbyelements2"];

filesadvanced := ["regular13system", "segrevariety", "hermitianspreads", "andrebruckbose",
            "titsovoid", "finingorbits", "enumerators"];

generate_script("geometry",filesgeometry);
generate_script("prsp",filesprsp);
generate_script("prgrp",filesprgrp);
generate_script("polarities",filespolarities);
generate_script("gpolygons",filesgpolygons);
generate_script("advanced",filesadvanced);


#Exec("generate_output_fining_testfiles_prsp.sh");

#create tst files

preambledir := DirectoriesPackageLibrary("fining","examples/")[1];
outputdir := DirectoriesPackageLibrary("fining","tst/output")[1];
cmddir := "dir \:\= DirectoriesPackageLibrary\(\"fining\"\,\"tst\/output\"\)\[1\]\;";

#create .tst files
#the nested ifs together with the SizeScreen make sure that input lines (plural),
#are written back in the tst file as one (long) input line.
#subfolder. Make your choice:

#for easy "tst/easy"
#sub := "tst/easy";

#for examples
#sub := "tst/examples";

#for advanced:
#sub := "tst/adv";

create_tst_files := function(sub,files)
local includedir,str,filename,i,o,input_stream,line,outputdir;
str := Concatenation("tst/",sub);
includedir := DirectoriesPackageLibrary("fining",str)[1];
str := Concatenation("tst/output/",sub);
outputdir := DirectoriesPackageLibrary("fining",str)[1];
for filename in files do
  i := Filename(outputdir,Concatenation(filename,".out"));
  o := Filename(includedir,Concatenation(filename,".tst"));
  PrintTo(o,"");
  input_stream := InputTextFile(i);
  ReadLine(input_stream); #reads first line which is a line with a #
  #ReadLine(input_stream);
  AppendTo(o,Concatenation("gap> START_TEST(\"Forms: ",filename,".tst\");\n"));
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
end;

create_tst_files("geometry",filesgeometry);
create_tst_files("prsp",filesprsp);
create_tst_files("prgrp",filesprgrp);
create_tst_files("polarities",filespolarities);
create_tst_files("gpolygons",filesgpolygons);
create_tst_files("advanced",filesadvanced);
