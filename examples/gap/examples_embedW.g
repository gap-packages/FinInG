# examples_embedW.g
pg2 := PG(2,5);
pg3 := PG(3,5);
hyp := VectorSpaceToElement(pg3,[[1,2,4,0],[0,3,2,0],[1,1,0,1]]*Z(5)^0);
em := NaturalEmbeddingBySubspace( pg2, pg3, hyp );
l := Random(Lines(pg2));
l^em;
p := Random(Points(hyp));
PreImageElm(em,p);
mat := [[0,0,0,1],[0,0,1,0],[0,-1,0,0],[-1,0,0,0]]*Z(3);
form := BilinearFormByMatrix(mat,GF(3));
w3 := PolarSpace(form);
w5 := SymplecticSpace(5, 3);
pg := AmbientSpace( w5 );
solid := VectorSpaceToElement(pg,[[1,0,0,0,0,0],[0,1,0,0,0,0],
[0,0,1,0,0,0],[0,0,0,1,0,0]]*Z(3)^0);
TypeOfSubspace(w5,solid);
em := NaturalEmbeddingBySubspace( w3, w5, solid );
points := Points( w3 );
points2 := ImagesSet(em, AsSet(points));;
ForAll(points2, x -> x in solid);
quit;
