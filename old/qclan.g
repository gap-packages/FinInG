# Two ways to construct a flock quadrangle from a Kantor-Knuth semifield q-clan 

A_t = [[t, 0], [0,-nt^sigma]]
q odd, not prime
n nonsquare of GF(q)
sigma nontrivial automorphism of GF(q)

q := 9;
f := GF(q);
squares := AsList(Group(Z(q)^2));
n := First(GF(q), x -> not IsZero(x) and not x in squares);
sigma := FrobeniusAutomorphism( f );
zero := Zero(f);
qclan := List(GF(q), t -> [[t, zero], [zero,-n * t^sigma]] );
IsqClan( qclan, f );




