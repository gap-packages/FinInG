#initialize: 
affinefiles := ["affine_parallel", "affine_shadow1", "affine_shadow2", "affine_basic", 
               "affine_elements", "affine_iterator", "affine_enumerator", "affine_join", "affine_meet"];;

examplesfiles := ["examples_pg24", "examples_hermitian", "examples_embedW", "examples_spreads", "examples_qclan", "examples_KantorKnuth"];;

incgeomfiles := ["incgeom_elementsj","incgeom_points","incgeom_lines","incgeom_planes",
                 "incgeom_solids", "incgeom_isincident", "incgeom_shadowofelement",
		 "incgeom_shadowofflag" ]; 

morphismsfiles := ["morphisms_intertwiners","morphisms_embedding1","morphisms_embedding2","morphisms_typesubspace",
          "morphisms_fieldreduc1", "morphisms_fieldreduc2","morphisms_subfield1","morphisms_subfield2",
          "morphisms_isopolar","morphisms_projection","morphisms_completion","morphisms_klein","morphisms_duality"];;
projpolfiles := ["projpol_projectivespace", "projpol_polarspaceform",
                 "projpol_symplectic", "projpol_hermitian", "projpol_elliptic",
		 "projpol_parabolic", "projpol_hyperbolic", "projpol_element1", "projpol_in"];

projgroupsfiles := ["projgroups_basefield",
	  "projgroups_collineation", "projgroups_collineationgroup",
	  "projgroups_correlation", "projgroups_fieldautomorphism",
	  "projgroups_projectivity", "projgroups_psisomorphism",
	  "projgroups_stduality", "projgroups_underlyingmatrix", "projgroups_onprojsubspaces",
	  "projgroups_onprojsubspacesreversing"];;

polaritiespsfiles := ["polarities_construct1", "polarities_construct2",
          "polarities_fromform", "polarities_toform", "polarities_basefield", 
	  "polarities_automorphism", "polarities_grammatrix", "polarities_ishermitian", 
	  "polarities_issymplectic", "polarities_ispseudo", "polarities_isorthogonal", 
	  "polarities_geometryofabsolutepoints", "polarities_absolutepoints", 
	  "polarities_polarspace", "polarities_frompolarspace"];

gpolygonfiles := ["gpolygons_projplanes1", "gpolygons_projplanes2", "gpolygons_EGQByKantorFamily", 
                  "gpolygons_EGQByqClan", "gpolygons_EGQByBLTSet", "gpolygons_collineations", "gpolygons_SplitCayleyHexagon"];

files := [polaritiespsfiles[15]];

#create .out files
#  Jan, these commands almost do the trick. Need to locate binary for gap though.
homedir := DirectoryCurrent();
exampledir := DirectoriesPackageLibrary("desargues","examples/gap")[1]; 
preambledir := DirectoriesPackageLibrary("desargues","examples")[1];
outputdir := DirectoriesPackageLibrary("desargues", "examples/output")[1];
gap := Filename(Directory("/usr/bin/"),"gap");  
args := [ JoinStringsWithSeparator(["-l", "./;", GAP_ROOT_PATHS[1]], " ") ];

                   
# exampledir := Directory("./pkg/desargues/examples/gap/");
# preambledir := Directory("./pkg/desargues/examples/");
# outputdir := Directory("./pkg/desargues/examples/output");
# gap := GAPInfo.SystemCommandLine[1]; 
#gap := Filename(Directory("/usr/bin/"),"gap");  
#gap := Filename(Directory("/usr/bin/"),"gap4r4static"); #for jdbs old faithful iBook:-)
#gap := Filename(Directory("/usr/bin/"),"gap4r4"); #for jdbs shiny iMac and Mac Book Pro :-)
args := [ JoinStringsWithSeparator(["-l", "./;", GAP_ROOT_PATHS[1]], " ") ];
#args := [ JoinStringsWithSeparator(["-l", "./;", GAP_ROOT_PATHS[2]], " ") ];

for filename in files do
  Print("Now converting file: ", filename, "\n");
  stream := InputOutputLocalProcess( homedir, gap, args);
  cmd := Concatenation("file := \"",filename,".out\";");
  WriteLine(stream,cmd);
  cmd := Concatenation("dir := Directory(","\"./pkg/desargues/examples/output/\"",");");
  WriteLine(stream,cmd);
  preamble := Filename(preambledir,"preamble.g");
  preamble_stream := InputTextFile(preamble);
  cmds := ReadAll(preamble_stream);
  WriteLine(stream,cmds);
  repeat
    str := ReadLine(stream);
  until str = "true\n";
  inputfile := Filename(exampledir,Concatenation(filename,".g"));
  input_stream := InputTextFile(inputfile);
  cmd := ReadLine(input_stream);
  while cmd <> fail and not IsEndOfStream(input_stream) do            
    WriteAll(stream,cmd);
    cmd := ReadLine(input_stream);
    ReadAll(stream);
  od;
od;

#create .include files
#jdb 13/12 I've added the next line, because replacing '<' by '&lt;' adds some
#characters, causing a problem in .include files when .out files are just fine. 
SizeScreen([85,24]);
includedir := Directory("./pkg/desargues/examples/include/");
for filename in files do
  i := Filename(outputdir,Concatenation(filename,".out"));
  o := Filename(includedir,Concatenation(filename,".include"));
  PrintTo(o,"");
  input_stream := InputTextFile(i);
  ReadLine(input_stream);
  ReadLine(input_stream);
  line := ReadLine(input_stream);
  while line <> "gap> quit;\n" do
  # while line <> fail do
    if line <> "\n" then
      line := ReplacedString(line,"\\\n","\n");
      AppendTo(o,ReplacedString(line,"<","&lt;"));
    fi;
    line := ReadLine(input_stream);
  od;
od;
#back to default.
SizeScreen([80,24]);

