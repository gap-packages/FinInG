#underluing matrix of a collineation.
g:=CollineationGroup( ProjectiveSpace(3,3));
x:=Random(g);;
MatrixOfCollineation(x);
Unpack(last);
quit;
