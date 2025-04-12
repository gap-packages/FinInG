gap> START_TEST("Forms: hermitianspreads.tst");
gap> ps := HermitianPolarSpace(5,4);
H(5, 2^2)
gap> planes := AsList(Planes(ps));;
gap> coll := CollineationGroup(ps);
PGammaU(6,2^2)
gap> adj := function(x,y)
>     return ProjectiveDimension(Meet(x,y))=-1;
> end;
function( x, y ) ... end
gap> graph := Graph(coll,planes,OnProjSubspaces,adj,true);;
gap> cliques := CompleteSubgraphs(graph);;
gap> Length(cliques);
338
gap> Collected(List(cliques,x->Length(x)));
[ [ 7, 312 ], [ 9, 26 ] ]
gap> cliques9 := CompleteSubgraphs(graph,9,2);
[ [ 1, 2, 24, 37, 68, 172, 324, 455, 854 ], 
  [ 1, 2, 24, 37, 68, 172, 497, 508, 708 ], 
  [ 1, 2, 24, 37, 112, 172, 410, 455, 798 ] ]
gap> partial_spread := VertexNames(graph){cliques9[1]};;
gap> partial_spreads := List(cliques9,x->VertexNames(graph){x});;
gap> groups := List(partial_spreads,x->FiningSetwiseStabiliser(coll,x));;
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
#I  Computing adjusted stabilizer chain...
gap> List(groups,x->Order(x));
[ 162, 168, 9072 ]
gap> STOP_TEST("hermitianspreads.tst", 10000 );
