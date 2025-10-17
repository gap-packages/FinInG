#create output and include files from .g files in the examples/gap directory.
#Executing this file is NOT necessary for the installation of the package "fining", and the
#use of all documentation.
#
#Performing these steps, files will be written in the doc directory of the
#fining package tree. Under UNIX-like operating systems, you need sufficient
#permissions to do. Executing this is NOT necessary for the installation of the
#package "fining".

#Messy things happen when you do it, so don't try this at home kids!
#file adapted on april 10th, 2011.

#create workspace with packages
LoadPackage("fining");
SaveWorkspace("fining.ws");
quit;

#restart gap now.

#initialize filenames

examplesfiles := ["examples_pg24", "examples_hermitian", "examples_embedW", "examples_spreads",
                  "examples_qclan", "examples_KantorKnuth","examples_PSL211"];;

incgeomfiles := [ "incgeom_incstruct", "incgeom_categories1", "incgeom_typesofels", "incgeom_rank",
				"incgeom_categories2", "incgeom_elementsj", "incgeom_short", "incgeom_isincident",
				"incgeom_random", "incgeom_el_ambient", "incgeom_flag", "incgeom_flag_chamber",
				"incgeom_shadowofelement", "incgeom_shadowofelement2", "incgeom_shadowofflag",
				"incgeom_shadowshort", "incgeom_iterator", "incgeom_enum", "incgeom_lie_uvs",
				"incgeom_lie_projdimension", "incgeom_ambientspace", "incgeom_lie_vectorspacetoelement",
				"incgeom_lie_elementtovectorspace", "incgeom_lie_in", "incgeom_lie_hyperplanesof",
				"incgeom_lie_elementtoelement"];

projspfiles := ["projsp_projectivespace", "projsp_projdimension", "projsp_basefieldps", "projsp_underlyingvs", "projsp_element1",
				"projsp_emptysubspace", "projsp_projdimension_element", "projsp_elements", "projsp_short", "projsp_incident",
				"projsp_standardframe", "projsp_coordinates", "projsp_eqhyperplane", "projsp_ambientspaceelps", "projsp_basefieldelps",
				"projsp_randomeltps", "projsp_randomeltsps", "projsp_span", "projsp_meet", "projsp_flag", "projsp_chamber",
				"projsp_shadowofelement", "projsp_shadowofflag", "projsp_elsel", "projsp_enumerator", "projsp_short2",
				"projsp_iterator", "projsp_list" ];

projgroupsfiles := [ "projgroups_projectivity", "projgroups_collineation", "projgroups_stduality", "projgroups_correlation",
					 "projgroups_underlyingmatrix", "projgroups_basefield", "projgroups_fieldautomorphism", "projgroups_psisomorphism",
					 "projgroups_representative", "projgroups_order", "projgroups_collineationgroup", "projgroups_projectivitygroup",
					 "projgroups_specialprojectivitygroup", "projgroups_mult", "projgroups_embedding", "projgroups_onprojsubspaces",
					 "projgroups_onprojsubspacesreversing", "projgroups_elation1", "projgroups_elation2", "projgroups_homology1",
					 "projgroups_homology2", "projgroups_nicem", "projgroups_niceo", "projgroups_cancompute" ];


affinefiles := ["affine_affinespace", "affine_dimension", "affine_basefield", "affine_underlyingvs",
				"affine_ambientspace", "affine_subspaces", "affine_elements", "affine_short",
				"affine_incident", "affine_ambientspaceelas", "affine_basefieldelas", "affine_span",
				"affine_meet", "affine_isparallel", "affine_parallel", "affine_shadow1", "affine_shadow2",
				"affine_iterator", "affine_enumerator", "affine_affinegroupexample", "affine_affinegroupexample2",
				"affine_affinegroup", "affine_collineationgroup", "affine_actions"];;



morphismsfiles := ["morphisms_filters", "morphisms_intertwiners","morphisms_embedding1","morphisms_embedding2",
			"morphisms_fieldreducproj", "morphisms_intertwinerproj", "morphisms_fieldreducpolar1", "morphisms_fieldreducpolar2",
			"morphisms_typesubspace",
          "morphisms_fieldreduc1", "morphisms_fieldreduc2","morphisms_subfield1","morphisms_subfield2",
          "morphisms_isopolar","morphisms_projection","morphisms_completion","morphisms_klein","morphisms_duality"];;


projpolfiles1 := ["projpol_projectivespace", "projpol_projdimension",
                 "projpol_underlyingvs", "projpol_element1", "projpol_emptysubspace",
		 "projpol_projdimension_element", "projpol_standardframe", "projpol_coordinates",
		  "projpol_eqhyperplane", "projpol_basefieldps", "projpol_ambientspaceelps",
		  "projpol_basefieldelps", "projpol_randomeltsps", "projpol_randomelps",
		  "projpol_span", "projpol_meet", "projpol_shadowofelement", "projpol_flag",
		  "projpol_shadowofflag", "projpol_shadowofflag2", "projpol_elsel"];


		 "projpol_hermitian", "projpol_elliptic",
		 "projpol_parabolic", "projpol_hyperbolic", "projpol_element1", "projpol_in"];

projpolfiles1 := ["projpol_projectivespace", "projpol_polarspaceform",
                 "projpol_symplectic", "projpol_hermitian", "projpol_elliptic",
		 "projpol_parabolic", "projpol_hyperbolic", "projpol_element1", "projpol_in"];

projgroupsfiles := ["projgroups_basefield",
	  "projgroups_collineation", "projgroups_collineationgroup",
	  "projgroups_correlation", "projgroups_fieldautomorphism",
	  "projgroups_projectivity", "projgroups_psisomorphism",
	  "projgroups_stduality", "projgroups_underlyingmatrix", "projgroups_onprojsubspaces",
	  "projgroups_onprojsubspacesreversing", "projgroups_simgroup",
	  "projgroups_isomgroup", "projgroups_specialisomgroup",
	  "projgroups_mult", "projgroups_elation1" ,"projgroups_elation2", "projgroups_homology1", "projgroups_homology2"];;

polaritiespsfiles := ["polarities_construct1", "polarities_construct2",
          "polarities_fromform", "polarities_toform", "polarities_basefield",
	  "polarities_automorphism", "polarities_grammatrix", "polarities_ishermitian",
	  "polarities_issymplectic", "polarities_ispseudo", "polarities_isorthogonal",
	  "polarities_geometryofabsolutepoints", "polarities_absolutepoints",
	  "polarities_polarspace", "polarities_frompolarspace",
	  "polarities_commuting"];

gpolygonfiles := [ "gpolygons_EGQByBLTSet", "gpolygons_EGQByKantorFamily", "gpolygons_iskantorfamily", "gpolygons_EGQByqClan", "gpolygons_isqclan",
		"gpolygons_SplitCayleyHexagon", "gpolygons_kantorfamilybyqclan", "gpolygons_basepointofegq", "gpolygons_particularqclans",
		"gpolygons_bltsetbyqclan", "gpolygons_projplanes1", "gpolygons_collineations", "gpolygons_projplanes2", "gpolygons_elationgroup",
		"gpolygons_qclan"];

websitefiles := ["web_hyperoval24", "web_inumbersherm", "web_embedding", "web_spreads", "web_ovoidq63"];

diagramfiles := ["diagram_cosetgeom"];


files := [ "projgroups_mult" ];

#initialize directorynames
#exampledir = dir where .g files are located : ".../pkg/fining/examples/gap"
#preambledir = directory where 'preamble_sws.g is found' :  ".../pkg/fining/examples"
#outputdir = directory to write '.out' files: ".../pkg/fining/examples/output"
#name of script to start gap version. The user has to fill this in!

homedir := DirectoryCurrent();
exampledir := DirectoriesPackageLibrary("fining","examples/gap")[1];
preambledir := DirectoriesPackageLibrary("fining","examples/")[1];
outputdir := DirectoriesPackageLibrary("fining","examples/output")[1];
gap := Filename(Directory("/usr/bin/"),"gap4r4");
paths := JoinStringsWithSeparator(GAP_ROOT_PATHS,";");
args := JoinStringsWithSeparator(["-l ",paths," -L fining.ws","-o 1G"]," ");

#create .out files using the saved workspace
#IMPORTANT: here we suppose that the script to start up our favorite version of
#GAP is called 'gap4r4', and is located in '/usr/bin'. Change the code if this is not true!
#you certainly now the name of the script, since you started gap. To find the
#dir, just issue in the gap session that is running:

Exec("which gap4r4"); #for UNIX only

for filename in files do
  Print("Now converting file: ", filename, "\n");
  gap := Filename(Directory("/usr/bin/"),"gap4r4");
  stream := InputOutputLocalProcess( homedir, gap, [args]);
  cmd := Concatenation("file := \"",filename,".out\";");
  WriteLine(stream,cmd);
  cmd := "dir \:\= DirectoriesPackageLibrary\(\"fining\"\,\"examples\/output\"\)\[1\]\;";
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
  while cmd <> fail do
    WriteAll(stream,cmd);
    cmd := ReadLine(input_stream);
    #ReadAll(stream);
  od;
  repeat until ReadAll(stream)=fail;
od;

#create .include files
#for the include files, some characters will be translated to suitable xml
#codes, taking more then one character. Therefore we widen the screen a little bit.
#includir: directory containing the include files: ".../pkg/fining/examples/include"
SizeScreen([256,24]);
includedir := DirectoriesPackageLibrary("fining","examples/include")[1];
for filename in files do
  i := Filename(outputdir,Concatenation(filename,".out"));
  o := Filename(includedir,Concatenation(filename,".include"));
  PrintTo(o,"");
  input_stream := InputTextFile(i);
  ReadLine(input_stream);
  ReadLine(input_stream);
  line := ReadLine(input_stream);
  while line <> "gap> quit;\n" do
    if line <> "\n" then
      line := ReplacedString(line,"\\\n","\n");
      AppendTo(o,ReplacedString(ReplacedString(line,"<","&lt;"),">","&gt;"));
    fi;
    line := ReadLine(input_stream);
  od;
od;
SizeScreen([80,24]);

#create .include files suitable for include in html, e.g. on your homepage directory (for godsake,
#comment your code!).
SizeScreen([256,24]);
includedir := DirectoriesPackageLibrary("fining","examples/html")[1];
for filename in files do
  i := Filename(outputdir,Concatenation(filename,".out"));
  o := Filename(includedir,Concatenation(filename,".html"));
  PrintTo(o,"");
  input_stream := InputTextFile(i);
  ReadLine(input_stream);
  ReadLine(input_stream);
  line := ReadLine(input_stream);
  while line <> "gap> quit;\n" do
  # while line <> fail do
    if line <> "\n" then
      line := ReplacedString(line,"\\\n","\n");
      line := ReplacedString(ReplacedString(line,"<","&lt;"),">","&gt;");
      #ReplacedString(line,"<","&lt;");
      AppendTo(o,ReplacedString(line,"\n","<br>\n"));
    fi;
    line := ReadLine(input_stream);
  od;
od;
#back to default.
SizeScreen([80,24]);
