scriptdir := DirectoriesPackageLibrary("fining","scripts")[1]; 
script := Filename(scriptdir,"generate_filtergraph.g");
Read(script);
Print("Making 'filtergraph.png'...\n");

Exec("dot -Tpng ./pkg/fining/doc/output/filtergraph.dot > ./pkg/fining/doc/filtergraph.png");
