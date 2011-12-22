#enumerator for collections of subspaces of polar spaces
Enumerator(Points(ParabolicQuadric(6,3)));
Enumerator(Lines(HermitianVariety(4,4)));
planes := List(Planes(HermitianVariety(5,4)));;
time;
Length(planes);
quit;

