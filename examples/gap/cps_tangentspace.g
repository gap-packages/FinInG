# tangent spaces.
ps := HermitianPolarSpace(3,4^2);
p := Random(Points(ps));
plane := TangentSpace(p);
TypeOfSubspace(ps,plane);
ps := ParabolicQuadric(6,4);
p := VectorSpaceToElement(PG(6,4),[0,1,0,0,0,0,0]*Z(4)^0);
hyp := TangentSpace(ps,p);
NucleusOfParabolicQuadric(ps) in hyp;
ps := EllipticQuadric(5,2);
line := Random(Lines(ps));
TangentSpace(line);
ps := HermitianPolarSpace(5,4);
plane := Random(Planes(ps));
tan := TangentSpace(plane);
tan in ps;
tan = plane;
quit;
