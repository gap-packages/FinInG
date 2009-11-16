Print("Making an excursion into UNIX...\n");

Exec("grep -h DeclareCategory gap/*gd | sed \"s/^D//\" > eclairs.tmp");
Exec("sed \"s/  / /\" eclairs.tmp > eclairs2.tmp");
Exec("sed \"s/\\\" )/\\\")/\" eclairs2.tmp > eclairs3.tmp");
Exec("sed \"s/, /, \\\"/\" eclairs3.tmp > eclairs2.tmp");
Exec("sed \"s/ )/\\\" )/\" eclairs2.tmp > eclairs3.tmp");

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

Read("eclairs3.tmp");

Exec("rm eclairs.tmp"); 
Exec("rm eclairs2.tmp"); 
Exec("rm eclairs3.tmp"); 

Exec("grep -h DeclareRepresentation gap/*gd | sed \"s/^D//\" > eclairs.tmp");
Exec("sed \"s/  / /\" eclairs.tmp > eclairs2.tmp");
Exec("sed \"s/\\\" )/\\\")/\" eclairs2.tmp > eclairs3.tmp");
Exec("sed \"s/, /, \\\"/\" eclairs3.tmp > eclairs2.tmp");
Exec("sed \"s/, \\\[/\\\" , \\\[/\" eclairs2.tmp > eclairs3.tmp");

Read("eclairs3.tmp");

Exec("rm eclairs.tmp");
Exec("rm eclairs2.tmp");
Exec("rm eclairs3.tmp");

Print("\nWriting 'filtergraph.dot'...\n");

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
