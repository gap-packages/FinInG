## examples_qclan.g
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t*id );;
IsqClan( clan, f );
clan := qClan(clan, f);
egq := EGQByqClan( clan );
elations := ElationGroup( egq );
points := Points( egq );
p := Random(points);
x := Random(elations);
OnKantorFamily(p,x);
orbs := Orbits( elations, points, OnKantorFamily);;
Collected(List( orbs, Size ));
blt := BLTSetByqClan( clan );
q4q := AmbientGeometry( blt[1] );
span := Span( blt );
ProjectiveDimension( span ); 
quit;
