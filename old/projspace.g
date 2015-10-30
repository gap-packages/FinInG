## The example below computes the image of a plane in
## projective space under a random element, and then
## computes all planes. 


pg := ProjectiveSpace(3, 3);
g := CollineationGroup(pg);
x := Random(g);
plane := [[1,0,0,0],[0,1,0,0],[0,0,1,0]]*Z(3)^0;
projplane := VectorSpaceToElement(pg, plane);
newplane := OnProjSubspaces(projplane, x);
ElementToVectorSpace(newplane);
