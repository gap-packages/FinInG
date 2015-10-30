#create the inventory of all gi files.
#this script is not fool proof, but I guess fools don't program in GAP.

files := ["geometry.gi", "liegeometry.gi", "group.gi", "projectivespace.gi", "correlations.gi", 
		"polarspace.gi", "morphisms.gi", "enumerators.gi", "diagram.gi", "varieties.gi", "affinespace.gi",
		"affinegroup.gi", "gpolygons.gi", "orbits-stabilisers.gi" ];
		
#initialize directorynames
#exampledir = dir where .g files are located : ".../pkg/fining/examples/gap"
#preambledir = directory where 'preamble_sws.g is found' :  ".../pkg/fining/examples"
#outputdir = directory to write '.out' files: ".../pkg/fining/examples/output"
#name of script to start gap version. The user has to fill this in!

i_f := "InstallGlobalFunction(";
i_m := "InstallMethod(";

homedir := DirectoryCurrent();
workingdir := DirectoriesPackageLibrary("fining","lib")[1];
outputdir := DirectoriesPackageLibrary("fining","doc/output")[1]; 
SizeScreen([256,24]);

#different files for global functions and methods.
outputfile_functions := Filename(outputdir,"functions_inventory.txt");
outputfile_methods := Filename(outputdir,"methods_inventory.txt");

PrintTo(outputfile_methods,"Methods\n\n");
PrintTo(outputfile_functions,"Functions\n\n");

for file in files do
	AppendTo(outputfile_functions,file,": global functions\n\n");
	AppendTo(outputfile_methods,file,": methods\n\n");
	filen := Filename(workingdir,file);
	stream := InputTextFile(filen);
	r := ReadLine(stream);
	while r <> fail do	
		r := NormalizedWhitespace(r);
		if IsSubset(r,i_m) and r{[1..7]}="Install" then
			RemoveCharacters(r," \n");
			name := SplitString(r,"(,")[2];
			tmp := Concatenation("M: ",name,", ");
			while not IsSubset(r,"[") do
				r := ReadLine(stream);
			od;
			RemoveCharacters(r,"\t\r\n");
			tmp := Concatenation(tmp," ",r);
			while not IsSubset(r,"]") do
				r := ReadLine(stream);
				RemoveCharacters(r,"\t\r\n");
				tmp := Concatenation(tmp," ",r);
			od;
			tmp := NormalizedWhitespace(tmp);
			AppendTo(outputfile_methods,ReplacedString(ReplacedString(tmp,"<","&lt;"),">","&gt;"),"\n");
			#AppendTo(outputfile_methods,tmp,"\n");
		elif IsSubset(r,i_f) and r{[1..7]}="Install" then
			RemoveCharacters(r," \n");
			name := SplitString(r,"(,")[2];
			tmp := Concatenation("F: ",name);
			AppendTo(outputfile_functions,ReplacedString(ReplacedString(tmp,"<","&lt;"),">","&gt;"),"\n");
			#AppendTo(outputfile_functions,tmp,"\n");
		fi;
	r := ReadLine(stream);
	od;
	AppendTo(outputfile_functions,"\n");
	AppendTo(outputfile_methods,"\n");
od;	

files := ["geometry.gd", "liegeometry.gd", "group.gd", "projectivespace.gd", "correlations.gd", 
		"polarspace.gd", "morphisms.gd", "enumerators.gd", "diagram.gd", "varieties.gd", "affinespace.gd",
		"affinegroup.gd", "gpolygons.gd" ];

d_o := "DeclareOperation(";
d_a := "DeclareAttribute(";
d_p := "DeclareProperty(";

outputfile_operations := Filename(outputdir,"operations_inventory.txt");
outputfile_attributes := Filename(outputdir,"attributes_inventory.txt");
outputfile_properties := Filename(outputdir,"properties_inventory.txt");

PrintTo(outputfile_operations,"Operations\n\n");
PrintTo(outputfile_attributes,"Attributes\n\n");
PrintTo(outputfile_properties,"Properties\n\n");

for file in files do
	AppendTo(outputfile_operations,file,": operations\n\n");
	AppendTo(outputfile_attributes,file,": attributes\n\n");
	AppendTo(outputfile_properties,file,": properties\n\n");
	filen := Filename(workingdir,file);
	stream := InputTextFile(filen);
	r := ReadLine(stream);
	while r <> fail do	
		r := NormalizedWhitespace(r);
		if Length(r) >= 17 and r{[1..17]} = d_o then
			RemoveCharacters(r," \n");
			while not IsSubset(r,"[") do
				n := ReadLine(stream);
				RemoveCharacters(n,"\n");
				r := Concatenation(r,n);
			od;
			while not IsSubset(r,"]") do
				n := ReadLine(stream);
				RemoveCharacters(n,"\n");
				r := Concatenation(r,n);
			od;
			RemoveCharacters(r,"\t\r\n");
			split := SplitString(r,"(,)");
			oper := split[2];
			RemoveCharacters(oper,"\"");
			filters := JoinStringsWithSeparator(split{[3..Length(split)-1]},", ");
			tmp := Concatenation("O: ",oper,": ",filters);
			AppendTo(outputfile_operations,ReplacedString(ReplacedString(tmp,"<","&lt;"),">","&gt;"),"\n");
			#AppendTo(outputfile_operations,tmp,"\n");
		elif Length(r) >= 17 and r{[1..17]} = d_a then
			RemoveCharacters(r," \n");
			split := SplitString(r,"(,)");
			attr := split[2];
			RemoveCharacters(attr,"\"");
			filter := split[3];
			tmp := Concatenation("A: ",attr,": ",filter);
			AppendTo(outputfile_attributes,ReplacedString(ReplacedString(tmp,"<","&lt;"),">","&gt;"),"\n");
			#AppendTo(outputfile_attributes,tmp,"\n");
		elif Length(r) >= 16 and r{[1..16]} = d_p then
			RemoveCharacters(r," \n");
			split := SplitString(r,"(,)");
			attr := split[2];
			RemoveCharacters(attr,"\"");
			filter := split[3];
			tmp := Concatenation("P: ",attr,": ",filter);
			AppendTo(outputfile_properties,ReplacedString(ReplacedString(tmp,"<","&lt;"),">","&gt;"),"\n");
			#AppendTo(outputfile_properties,tmp,"\n");
		fi;
	r := ReadLine(stream);
	od;
	AppendTo(outputfile_operations,"\n");
	AppendTo(outputfile_attributes,"\n");
	AppendTo(outputfile_properties,"\n");
od;