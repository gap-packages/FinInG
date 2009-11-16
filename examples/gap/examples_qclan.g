## examples_qclan.g
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t*id );;
IsqClan( clan, f );
egq := EGQByqClan( clan, f );
elations := ElationGroup( egq );
points := Points( egq );
p := Random(points);
x := Random(elations);
OnKantorFamily(p,x);
orbs := Orbits( elations, points, OnKantorFamily);;
Collected(List( orbs, Size ));
blt := BLTSetByqClan( clan, f );
q4q := AmbientGeometry( blt[1] );
span := Join( blt );
Print("Now we see if this BLT-set is a conic\n");
ProjectiveDimension( span ); 
quit;
