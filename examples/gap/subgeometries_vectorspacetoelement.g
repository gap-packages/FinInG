# VectorSpaceToElement for subgeometries
pg := PG(2,5^6);
vecs := [ [ Z(5)^0, Z(5^6)^13972, Z(5^6)^11653 ], 
[ Z(5)^0, Z(5^6)^9384, Z(5^6)^1372 ],
[ Z(5)^0, Z(5^6)^14447, Z(5^6)^15032 ], 
[ Z(5)^0, Z(5^6)^8784, Z(5^6)^10360 ] ];;
frame := List(vecs,x->VectorSpaceToElement(pg,x));
sub := SubgeometryOfProjectiveSpaceByFrame(pg,frame,5^3);
VectorSpaceToElement(sub,[0,0,0]*Z(5)^0);
vec := [ Z(5)^0, Z(5^6)^8584, Z(5^6)^13650 ];
VectorSpaceToElement(sub,vec);
vec := [ [ Z(5)^0, 0*Z(5), Z(5^6)^5740 ], [ 0*Z(5), Z(5)^0, Z(5^6)^15250 ] ];
VectorSpaceToElement(sub,vec);
quit;
vec := [ [ Z(5)^0, 0*Z(5), Z(5^6)^8268 ], [ 0*Z(5), Z(5)^0, Z(5^6)^1472 ] ];
VectorSpaceToElement(sub,vec);
VectorSpaceToElement(sub,vecs);
quit;


