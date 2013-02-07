#base field of a projective space
ps := HermitianPolarSpace(3,7^2);
line := VectorSpaceToElement(ps,[[Z(7)^0,0*Z(7),Z(7^2)^34,Z(7^2)^44],
[0*Z(7),Z(7)^0,Z(7^2)^2,Z(7^2)^4]]);
AmbientSpace(line);
quit;
