#maximal partial spreads of H(5,4)
ps := HermitianPolarSpace(5,4);
planes := AsList(Planes(ps));;
coll := CollineationGroup(ps);
adj := function(x,y)
    return ProjectiveDimension(Meet(x,y))=-1;
end;
graph := Graph(coll,planes,OnProjSubspaces,adj,true);;
cliques := CompleteSubgraphs(graph);;
Length(cliques);
Collected(List(cliques,x->Length(x)));
cliques9 := CompleteSubgraphs(graph,9,2);
partial_spread := VertexNames(graph){cliques9[1]};;
partial_spreads := List(cliques9,x->VertexNames(graph){x});;
groups := List(partial_spreads,x->FiningSetwiseStabiliser(coll,x));;
List(groups,x->Order(x));
quit;
