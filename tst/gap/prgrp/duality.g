#StandardDualityOfProjectiveSpace
pg := PG(2,2);
delta := StandardDualityOfProjectiveSpace(pg);
phi := delta^2;
p := VectorSpaceToElement(pg,[1,1,1]*Z(2));
line := p^delta;
Unpack(UnderlyingObject(line));
DualCoordinatesOfHyperplane(line);
quit;
