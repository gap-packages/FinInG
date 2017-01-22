# underlying vector space/ ambient space of a subgeometry
pg := PG(2,5^6);
sub1 := CanonicalSubgeometryOfProjectiveSpace(pg,5);
UnderlyingVectorSpace(pg);
UnderlyingVectorSpace(sub1);
AmbientSpace(sub1);
sub2 := CanonicalSubgeometryOfProjectiveSpace(pg,5^3);
AmbientSpace(sub2);
UnderlyingVectorSpace(sub2);
frame := RandomFrameOfProjectiveSpace(pg);
sub3 := SubgeometryOfProjectiveSpaceByFrame(pg,frame,5^2);
AmbientSpace(sub3);
UnderlyingVectorSpace(sub3);
quit;

