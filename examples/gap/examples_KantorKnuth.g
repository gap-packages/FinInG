# examples_KantorKnuth.g
SetInfoLevel( InfoDesargues, 0 );
q := 9;
f := GF(q);
squares := AsList(Group(Z(q)^2));
n := First(GF(q), x -> not IsZero(x) and not x in squares);
sigma := FrobeniusAutomorphism( f );
zero := Zero(f);
qclan := List(GF(q), t -> [[t, zero], [zero,-n * t^sigma]] );
IsqClan( qclan, f );
egq1 := EGQByqClan( qclan, f );  
blt := BLTSetByqClan( qclan, f );
egq2 := EGQByBLTSet( blt );
quit;




