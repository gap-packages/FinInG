LoadPackage("fining");
SaveWorkspace("fining.ws");

homedir := DirectoryCurrent();
gapstart := "gap4r13.1"; #might be different on your computer
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
cmd := Concatenation("chmod +x ",scriptfile);
Exec(cmd);
end;

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

filesprsp := ["pg", "prdim", "basefield", "uderlyingvs", "ambientspace",
                "vspacetoel", "emptysub", "prdim_el", "elements_inc", "elshort",
                "inc", "frame", "coordinates", "dualcoordinates", "hyperplanedual",
                "equationhyp", "ambientspaceel", "basefieldel", "random",
                "randomsubspace", "span", "meet", "flagofinc", "shadowofelement",
                "shadowofflag", "elincel", "shortnames", "iterator", "aslist"];

filesprgrp := ["projectivity", "collineation", "projsemilinear", "identity",
                "projectivity", "projsemilinear", "representatives", "projgroups",
                "representatives", "onprojsubspaces", "actionall", "onprojsubspacesext"];



sub := "prsp";
generate_script(sub,filesprsp);

sub := "prgrp";
generate_script(sub,filesprgrp);


sub := "prsp";
create_tst_files(sub,filesprsp);
sub := "prgrp";
create_tst_files(sub,filesprgrp);


