#underluing matrix of a collineation.
g:=CorrelationCollineationGroup( ProjectiveSpace(4,9));
x:=Random(g);;
MatrixOfCorrelation(x);
Unpack(last);
quit;
