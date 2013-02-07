#NAtural projection by subspace example
ps := HyperbolicQuadric(5,3);
x := Random(Points(ps));;
planes_on_x := AsList( Planes(x) );
proj := NaturalProjectionBySubspace(ps, x);
image := ImagesSet(proj, planes_on_x);
quit;
