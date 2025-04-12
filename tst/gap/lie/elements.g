#Converting elements in Lie geometries
ps := ParabolicQuadric(2,59);
pg := PG(2,59);
p := VectorSpaceToElement(ps,[0,1,0]*Z(59)^0);
ElementToElement(pg,p);
obj := UnderlyingObject(p);
ObjectToElement(pg,1,obj);
quit;

