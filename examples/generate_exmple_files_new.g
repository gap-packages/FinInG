#create output and include files from .g files in the examples/gap directory.
#Executing this file is NOT necessary for the installation of the package "fining", and the
#use of all documentation.
#
#Performing these steps, files will be written in the doc directory of the
#fining package tree. Under UNIX-like operating systems, you need sufficient
#permissions to do. Executing this is NOT necessary for the installation of the
#package "fining".
#
#Messy things happen when you do it, so don't try this at home kids!
#file created feb 05 2013.
#file adapted oct 2015.
#new version created on April 8, 2025, during GAPDays Spring 2025 in Brussels

#create workspace with packages
LoadPackage("fining");
SaveWorkspace("fining.ws");
quit;

#restart gap now.

#initialize filenames

# examples.xml
# files for chapter "Examples"

examplesfiles := ["examples_hyperoval24", "examples_hermitian", "examples_embedW", "examples_patterson",  
				  "examples_spreads", "examples_qclan", "examples_KantorKnuth", "examples_PSL211",
                  "examples_sub1", "examples_sub2", "examples_underlyingobject", "examples_cps1", "examples_collgroup", 
				  "examples_splitcayley", "examples_e6", "examples_varieties", "examples_tits",
				  "examples_morphism1", "examples_morphism2", "examples_klein", "examples_embedsubfield",
                  "examples_embedfieldreduction", "examples_embedfieldreduction", "examples_octagon"];;

incgeomfiles := ["incgeom_incstruct", "incgeom_categories1", "incgeom_typesofels", "incgeom_rank", "incgeom_incgraph",
                 "incgeom_underlyingobject", "incgeom_type",
                 "incgeom_categories2", "incgeom_elementsj","incgeom_short", "incgeom_isincident", "incgeom_random",
				 "incgeom_el_ambient", "incgeom_flag", "incgeom_elementsofflag", "incgeom_rankofflag", "incgeom_sizeofflag",
                 "incgeom_ambientgeometry", "incgeom_typeofflag", "incgeom_isincidentwithflag", "incgeom_flag_chamber", 
                 "incgeom_shadowofelement", "incgeom_shadowofelement2", "incgeom_shadowofflag", "incgeom_residueofflag",
                 "incgeom_shadowshort", "incgeom_iterator", "incgeom_iteratorexample", "incgeom_enum", "incgeom_enumexample", 
                 "incgeom_list", "incgeom_aslist", "incgeom_lie_uvs",
				 "incgeom_lie_projdimension", "incgeom_ambientspace", "incgeom_lie_vectorspacetoelement", "incgeom_lie_elementtovectorspace", 
				 "incgeom_lie_in", "incgeom_lie_hyperplanesof", "incgeom_lie_elementtoelement", "incgeom_nrelements" ]; #"incgeom_lie_convert"

projspfiles := ["projsp_projectivespace", "projsp_projdimension", "projsp_basefieldps", "projsp_underlyingvs", "projsp_element1",
				"projsp_emptysubspace", "projsp_projdimension_element", "projsp_elements", "projsp_short", "projsp_incident", 
				"projsp_standardframe", "projsp_coordinates", "projsp_eqhyperplane", "projsp_ambientspaceelps", "projsp_basefieldelps",
				"projsp_randomeltps", "projsp_randomeltsps", "projsp_span", "projsp_meet", "projsp_flag", "projsp_chamber",
				"projsp_shadowofelement", "projsp_shadowofflag", "projsp_elsel", "projsp_short", "projsp_iterator", "projsp_enumerator", 
				"projsp_list", "projsp_short2" ];

projgroupsfiles := ["projgroups_isprojectivity", "projgroups_iscollineation", "projgroups_isstrictlysemilinear", "projgroups_iscorrelation",
					"projgroups_projectivity", "projgroups_collineation", "projgroups_stduality", "projgroups_correlation",
					"projgroups_representative", "projgroups_underlyingmatrix", "projgroups_underlyingmatrix2", "projgroups_basefield",
                    "projgroups_fieldautomorphism", "projgroups_psisomorphism",
					"projgroups_order", "projgroups_projectivitygroup", "projgroups_collineationgroup", "projgroups_specialprojectivitygroup", "projgroups_mult",
					"projgroups_embedding", "projgroups_onprojsubspaces", "projgroups_onprojsubspacesextended", "projgroups_elation1", "projgroups_elation2",
					"projgroups_homology1", "projgroups_homology2", "projgroups_nicem", "projgroups_niceo", "projgroups_cancompute", "projgroups_random",
                    "projgroups_correlationcollineationgroup"];;
	   
polaritiespsfiles := ["polarities_construct1", "polarities_construct2", "polarities_fromform", "polarities_frompolarspace", 
					"polarities_toform", "polarities_basefield", "polarities_grammatrix", "polarities_automorphism", "polarities_ishermitian",
					"polarities_issymplectic", "polarities_isorthogonal", "polarities_ispseudo", "polarities_geometryofabsolutepoints",
					"polarities_absolutepoints", "polarities_polarspace", "polarities_commuting"];

classicalpolfiles := ["cps_polarspaceform", "cps_example", "cps_symplectic", "cps_hermitian", "cps_parabolic", "cps_hyperbolic",
						"cps_elliptic", "cps_iscanonicalps", "cps_underlyingvs", "cps_ambientspace", "cps_projectivedimension", "cps_rank",
						"cps_basefield", "cps_ishyperbolicquadric", "cps_isellipticquadric", "cps_isparabolicquadric", 
						"cps_element1", "cps_emptysubspace", "cps_projdimension_element", "cps_elements", 
						"cps_ambientspaceelps", "cps_coordinates", "cps_incident", "cps_span", "cps_typesubspace", "cps_shadowofelement",
						"cps_elementsincidentwithelement", "cps_specialisometry", "cps_isometry", "cps_similarity", "cps_collineation",
						 "cps_enumerator", "cps_iterator", "cps_aslist", "cps_polarityofps", "cps_iscollinear",
						 "cps_tangentspace", "cps_pole", "cps_evaluateform" ];
	   
stab_orbsfiles := [ "stab_orbs_finingorbit1", "stab_orbs_finingorbit2", "stab_orbs_finingorbits1", "stab_orbs_example1", "stab_orbs_finingstabiliser", 
					"stab_orbs_finingstabiliserorb", "stab_orbs_timing1", "stab_orbs_setwisegeneric", "stab_orbs_finingsetwisestabiliser", 
					"stab_orbs_timing2", "stab_orbs_action1", "stab_orbs_behaviour1", "stab_orbs_behaviour2", "stab_orbs_stabofsubspace",
                    "stab_orbits_finingorbitsdomain"];

affinefiles := [ "affine_affinespace", "affine_dimension", "affine_basefield", "affine_underlyingvs", "affine_ambientspace", "affine_subspaces",
				 "affine_elements", "affine_short", "affine_incident", "affine_ambientspaceelas", "affine_basefieldelas", "affine_span", "affine_meet",
				 "affine_isparallel", "affine_parallel", "affine_shadow1", "affine_shadow2", "affine_iterator", "affine_enumerator", "affine_affinegroupexample",
				 "affine_affinegroupexample2", "affine_affinegroup", "affine_collineationgroup", "affine_actions"];

morphismsfiles := ["morphisms_filters", "morphisms_isopolar", "morphisms_embedding1", "morphisms_embedding2", "morphisms_fieldreducproj",
					"morphisms_intertwinerproj", "morphisms_fieldreducpolar1", "morphisms_fieldreducpolar2", "morphisms_subfield1",
					"morphisms_subfield2", "morphisms_projection", "morphisms_klein", "morphisms_klein2", "morphisms_klein3", "morphisms_duality",
                    "morphisms_selfduality",
                    "morphisms_completion","morphisms_plucker", "morphisms_blownupsubspace", "morphisms_fieldreducpolar3" ];

gpolygonfiles := [ "gpolygons_categories1", "gpolygons_categories2", "gpolygons_gpbyblocks", "gpolygons_gpbyincmat", "gpolygons_gpbyelements", "gpolygons_order",
                    "gpolygons_incgraph", "gpolygons_incmatrix", "gpolygons_collineationgroup1", "gpolygons_collineationgroup2", "gpolygons_collineationgroup3",
                    "gpolygons_collineations", "gpolygons_blockdesign", "gpolygons_objectselements", "gpolygons_span", "gpolygons_meet", "gpolygons_shadow",
                    "gpolygons_distance", "gpolygons_SplitCayleyHexagon", "gpolygons_TwistedTrialityHexagon", "gpolygons_cghvectorspacetoelement", 
                    "gpolygons_cghobjecttoelement", "gpolygons_cghin", "gpolygons_spanmeet", "gpolygons_collineationgroup4", "gpolygons_EGQByKantorFamily",
                    "gpolygons_objectselementsofegq", "gpolygons_elationgroup", "gpolygons_EGQByqClan", "gpolygons_bltsetbyqclan", 
                    "gpolygons_EGQByBLTSet", "gpolygons_definingplanes", "gpolygons_objectselementsblt", "gpolygons_collineationsubgroup"];

varieties_files := ["varieties_general", "varieties_polarspace", "varieties_veronese", "varieties_segre", 
					"varieties_hermitian", "varieties_quadratic", "varieties_grassmann", "varieties_points",
					"varieties_quadratic"  ];

diagramfiles := ["diagram_cosetgeom", "diagram_neumaier", "diagram_nearoctagon", "diagram_autiso", "diagram_autcor",
                    "diagram_random", "diagram_notFT", "diagram_firmthinthick", "diagram_connectedness", "diagram_flagmapping",
                    "diagram_residues"];
                    
subgeometriesfiles := ["subgeometries_canonical", "subgeometries_categories", "subgeometries_randomframe", "subgeometries_isframe", "subgeometries_underlyingstructures", "subgeometries_definingframe",
                        "subgeometries_collineationfixingsubgeometry", "subgeometries_vectorspacetoelement", "subgeometries_extendelement", "subgeometries_emptysubspace", "subgeometries_projdimension_element.g", "subgeometries_coordinates" ];

websitefiles := ["web_hyperoval24", "web_inumbersherm", "web_embedding", "web_spreads", "web_ovoidq63"];

groups_appfiles := ["groups_app_sodesargues", "groups_app_godesargues", "groups_app_sudesargues", "groups_app_gudesargues",
					"groups_app_spdesargues", "groups_app_gspdesargues", "groups_app_generalsymplectic", "groups_app_gammasp",
					"groups_app_deltaominus", "groups_app_deltaoplus", "groups_app_gammaoplus", "groups_app_gammaominus",
					"groups_app_gammao", "groups_app_gammau" ];

exampledir := DirectoriesPackageLibrary("fining","tst")[1];

files := [ "subgeometries_vectorspacetoelement" ];

files := ["projgroups_elation2"];

homedir := DirectoryCurrent();
scriptfile := Filename(homedir,"generate_output_fining.sh");
PrintTo(scriptfile,"");

#gapstart := "gap4r13.1"; #might be different on your computer
gapstart := "gap4r14"; #might be different on your computer
gap := Filename(Directory("/usr/local/bin/"),gapstart);
paths := JoinStringsWithSeparator(GAPInfo.RootPaths{[3,4]},";"); #note: this is typical for the installation on jdb's computer, gap is started using gap... - "./;/opt/..."
pathsstr := Concatenation("\"",paths,"\"");
exampledir := DirectoriesPackageLibrary("fining","examples/gap")[1];
outputdir := DirectoriesPackageLibrary("fining","examples/output")[1];

for filename in files do
inputfile := Filename(exampledir,Concatenation(filename,".g"));
outputfile := Filename(outputdir,Concatenation(filename,".out"));
outputfilestr := Concatenation("\"LogTo(","\\\"",outputfile,"\\\"",");\"");
cmdlist := [gapstart,"-l",pathsstr,"-L","fining.ws","-o","4G","-c",outputfilestr,"<",inputfile,"\n"];
cmd := JoinStringsWithSeparator(cmdlist," ");
AppendTo(scriptfile,cmd);
od;

#Now there is a .sh file ready called generate_output_fining.sh. Make it executable and execute it, then for each filename
#in files an .out file will be generated in ./pkg/fining/examples/output. Each of these .out files contains output almost suitable
#to include in the documentation, or to include in the test directory.


#create .include files
#for the include files, some characters will be translated to suitable xml
#codes, taking more then one character. Therefore we widen the screen a little bit.
#includir: directory containing the include files: ".../pkg/fining/examples/include"

SizeScreen([85,24]);
includedir := DirectoriesPackageLibrary("fining","examples/include")[1];
for filename in files do
  i := Filename(outputdir,Concatenation(filename,".out"));
  o := Filename(includedir,Concatenation(filename,".include"));
  PrintTo(o,"");
  input_stream := InputTextFile(i);
  #ReadLine(input_stream);
  ReadLine(input_stream);
  line := ReadLine(input_stream);
  while line <> "gap> quit;\n" do
    if line <> "\n" then
      line := ReplacedString(line,"\\\n","\n");
      AppendTo(o,ReplacedString(line,"<","&lt;"));
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


