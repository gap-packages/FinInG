# examples_hermitian.g
h:=HermitianPolarSpace(2, 7^2);
pg := AmbientSpace( h );
lines := Lines( pg );
curve := AsList( Points( h ) );;
Size(curve);
Collected( List(lines, t -> Number(curve, c-> c in t)));
time;
quit;
