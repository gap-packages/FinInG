#subspaces of projective spaces (elementary example)
pg := PG(3,8);
vec := [0,1,0,1]*Z(8)^0;
point := VectorSpaceToElement(pg,vec);
mat := [[0,0,1,1],[0,1,0,0]]*Z(8)^0;
line := VectorSpaceToElement(pg,mat);
mat2 := [[1,0,0,1],[1,0,1,0],[1,1,0,0]]*Z(8)^0;
plane := VectorSpaceToElement(pg,mat2);
IsIncident(point,line);
IsIncident(line,point);
point * line;
line * point
point in line;
line in point;
IsIncident(point,plane);
IsIncident(line,plane);
line in plane;
plane2 := Span(point,line);
Meet(plane,plane2);
mat3 := [[1,0,0,0],[0,0,0,1]]*Z(8)^0;
line2 := VectorSpaceToElement(pg,mat3);
Meet(line,line2);
Span(plane,plane2);
quit;
