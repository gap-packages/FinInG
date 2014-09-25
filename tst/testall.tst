gap> START_TEST("testall.tst");
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
gap> STOP_TEST( "testall.tst", 2500000 );
