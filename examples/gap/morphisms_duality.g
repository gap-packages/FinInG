#morphisms_duality.g
w := SymplecticSpace(3,5);
lines:=AsList(Lines(w));;
duality := NaturalDuality(w);
l:=lines[1];
l^duality;
PreImageElm(duality,last);
quit;
