#morphisms_isopolar.g
id := IdentityMat(6, GF(5));;
form := BilinearFormByMatrix( id, GF(5) );
ps := PolarSpace( form );
PolarSpaceType( ps );
quadric := HyperbolicQuadric( 5, 5 );
iso := IsomorphismPolarSpaces( ps, quadric );
HasCollineationGroup( ps );
hom := Intertwiner( iso );;
ImagesSet(hom, SpecialIsometryGroup( ps ));
quit;
