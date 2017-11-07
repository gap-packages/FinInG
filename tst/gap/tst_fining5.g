#Tits ovoid, taken from one of the examples wit less output
q := 8;
pg := PG(3,q);
f := GF(q);
vecs := Union(List(f,x->List(f,y->[One(f),x*y+x^6+y^4,x,y])));;
Add(vecs,[0,1,0,0]*Z(q)^0);
ovoid := List(vecs,x->VectorSpaceToElement(pg,x));;
numbers := List(Planes(pg),x->Number(ovoid,y->y in x));;
Collected(numbers);
quit;
quit;
