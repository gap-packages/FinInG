#check whether a list of points is a frame
pg := PG(1,7^3);
p1 := VectorSpaceToElement(pg,[1,1]*Z(7)^0);
p2 := VectorSpaceToElement(pg,[1,2]*Z(7)^0);
p3 := VectorSpaceToElement(pg,[1,3]*Z(7)^0);
IsFrameOfProjectiveSpace([p1,p2,p3]);
quit;
