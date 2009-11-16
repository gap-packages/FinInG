#morphisms_completion.g
as := AffineSpace(3,5);
map := ProjectiveCompletion(as);
p := Random( Points(as) );
p^map;
quit;
