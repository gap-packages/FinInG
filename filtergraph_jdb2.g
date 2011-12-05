files := ["geometry.gd", "liegeometry.gd", "group.gd", "projectivespace.gd", "correlations.gd", 
		"polarspace.gd", "morphisms.gd", "enumerators.gd", "diagram.gd", "varieties.gd", "affinespace.gd",
		"affinegroup.gd", "gpolygons.gd" ];

packagefilters:=[];
impliedfilters:=[];

basedir := DirectoriesPackageLibrary("fining","")[1];
codedir := DirectoriesPackageLibrary("fining","gap")[1];
docdir := DirectoriesPackageLibrary("fining","doc")[1];

d_c := "DeclareCategory(";
d_r := "DeclareRepresentation(";

for file in files do
	filen := Filename(codedir,file);
	stream := InputTextFile(filen);
	r := ReadLine(stream);
	while r <> fail do
		r := NormalizedWhitespace(r);
		if (IsSubset(r,d_c) and r{[1..16]}="DeclareCategory(") or (IsSubset(r,d_r) and r{[1..22]}="DeclareRepresentation(") then
			l := Length(r);
			while r[l] <> ';' do
				e := NormalizedWhitespace(ReadLine(stream));
				r := Concatenation(r," ",e);
				l := Length(r);
			od;
			r := NormalizedWhitespace(r);
			split := SplitString(r,",");
			part1 := split[1];
			part2 := split[2];
			packagefilt := SplitString(part1,"\"")[2];
			RemoveCharacters(part2,");");
			part2 := NormalizedWhitespace(part2);
			split2 := SplitString(part2," ");
			impliedfilt := Filtered(split2,x->x <> "and");
			Add(packagefilters,packagefilt);
			Add(impliedfilters,impliedfilt);
		fi;
		r := ReadLine(stream);
	od;
od;

Print("Done\nWriting 'filtergraph.dot'...\n");

output := OutputTextFile( "filtergraph2.dot", false );
writestring:="digraph filters {\n\t node [style=filled];\n";
AppendTo(output, writestring);

makevertex := function(str)
  local writestring;
    writestring := Concatenation("\"", str, "\" [color = lightblue2];\n");
    AppendTo(output, writestring);
end;
    
makeedge := function(src, dest)
  local writestring;
    writestring := Concatenation("\"", src, "\" -> \"", dest, "\";\n");
    AppendTo(output, writestring);
end;

vertices:=UnionSet(packagefilters, Union(impliedfilters));

#here a selection can be made.

item := "IsSubspacesOfClassicalPolarSpace";
n := Length(packagefilters);
Filtered([1..n],i->item = packagefilters[i] or item in impliedfilters[i]);




for vertex in vertices do
  makevertex(vertex);
od;

for i in [1..Length(packagefilters)] do
  dest := packagefilters[i];
  srcs:= impliedfilters[i];
  for src in srcs do
    makeedge(src, dest);
  od;
od;

AppendTo(output, "}\n");
CloseStream(output);






packagefilters:=[];
impliedfilters:=[];

splitfilters := function(str)
  local flist, pos;
    flist :=[];
    pos:=PositionSublist(str, " and ");
    while IsPosInt(pos) do
      Add(flist, str{[1..pos-1]});
      str:=str{[pos+5..Length(str)]};
      pos:=PositionSublist(str, " and ");
    od;
    Add(flist, str);
    return(flist);
end;
    
eclareCategory := function(filter, supfilters)
  local supfilterlist;
    Add(packagefilters, filter);
    supfilterlist:=splitfilters(supfilters);
    Add(impliedfilters, supfilterlist);
    Print(".\c");
end;

eclareCategoryCollections:= function(str)
return;
end;

eclareRepresentation := function(filter, supfilters, dummy)
  local supfilterlist;
    Add(packagefilters, filter);
    supfilterlist:=splitfilters(supfilters);
    Add(impliedfilters, supfilterlist);
    Print(".\c");
end;

Print("Categories ");

Read("eclairs3.tmp");

Exec("rm eclairs.tmp"); 
Exec("rm eclairs2.tmp"); 
Exec("rm eclairs3.tmp"); 

Print(" Done\nRepresentations");

Exec("grep -h DeclareRepresentation /Users/jdebeule/pkg/fining/gap/*gd | sed \"s/^D//\" > /Users/jdebeule/pkg/fining/eclairs.tmp");
Exec("sed \"s/  / /\" eclairs.tmp > eclairs2.tmp");
Exec("sed \"s/\\\" )/\\\")/\" eclairs2.tmp > eclairs3.tmp");
Exec("sed \"s/, /, \\\"/\" eclairs3.tmp > eclairs2.tmp");
Exec("sed \"s/, \\\[/\\\" , \\\[/\" eclairs2.tmp > eclairs3.tmp");

Read("eclairs3.tmp");

Exec("rm eclairs.tmp");
Exec("rm eclairs2.tmp");
Exec("rm eclairs3.tmp");

Print("Done\nWriting 'filtergraph.dot'...\n");

output := OutputTextFile( "filtergraph.dot", false );
writestring:="digraph filters {\n\t node [style=filled];\n";
AppendTo(output, writestring);

makevertex := function(str)
  local writestring;
    writestring := Concatenation("\"", str, "\" [color = lightblue2];\n");
    AppendTo(output, writestring);
end;
    
makeedge := function(src, dest)
  local writestring;
    writestring := Concatenation("\"", src, "\" -> \"", dest, "\";\n");
    AppendTo(output, writestring);
end;

vertices:=UnionSet(packagefilters, Union(impliedfilters));

for vertex in vertices do
  makevertex(vertex);
od;

for i in [1..Length(packagefilters)] do
  dest := packagefilters[i];
  srcs:= impliedfilters[i];
  for src in srcs do
    makeedge(src, dest);
  od;
od;

AppendTo(output, "}\n");
CloseStream(output);

Print("Making 'filtergraph.png'...\n");

Exec("dot -Tpng filtergraph.dot > filtergraph.png");
