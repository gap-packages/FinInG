#enumerator for collections of subspaces of polar spaces
Enumerator(Points(ParabolicQuadric(6,3)));
Enumerator(Lines(HermitianPolarSpace(4,4)));
planes := List(Planes(HermitianPolarSpace(5,4)));;
time;
Length(planes);
quit;

