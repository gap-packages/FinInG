#constructing standardduality of projective space
ps := ProjectiveSpace(4,5);
delta := StandardDualityOfProjectiveSpace(ps);
delta^2;
p := VectorSpaceToElement(ps,[1,2,3,0,1]*Z(5)^0);
h := p^delta;
UnderlyingObject(h);
Unpack(last);
quit;
