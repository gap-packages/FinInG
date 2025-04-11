#Coordinates of a projective point
ps := PG(1,5);
p := VectorSpaceToElement(ps,[4,3]*Z(5)^0);
c := Coordinates(p);
quit;

