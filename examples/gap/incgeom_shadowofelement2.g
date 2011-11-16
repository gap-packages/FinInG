#one more exmple of general method
p := Random(Points(PG(3,3)));
lines1 := ShadowOfElement(SymplecticSpace(3,3),p,2);
Size(lines1);
lines2 := ShadowOfElement(PG(3,3),p,2); 
Size(lines2);
quit;
