# examples_KantorKnuth.g
q := 9;
f := GF(q);
squares := AsList(Group(Z(q)^2));
n := First(GF(q), x -> not IsZero(x) and not x in squares);
sigma := FrobeniusAutomorphism( f );
zero := Zero(f);
qclan := List(GF(q), t -> [[t, zero], [zero,-n * t^sigma]] );
IsqClan( qclan, f );
qclan := qClan(qclan , f);
egq1 := EGQByqClan( qclan);  
blt := BLTSetByqClan( qclan );
egq2 := EGQByBLTSet( blt );
quit;




