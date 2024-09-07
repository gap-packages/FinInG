gap> START_TEST("Forms: iterator.tst");
gap> pg := PG(9,109^2);
ProjectiveSpace(9, 11881)
gap> planes := Planes(pg);
<planes of ProjectiveSpace(9, 11881)>
gap> iter := Iterator(planes);
<iterator>
gap> plane := NextIterator(iter);
<a plane in ProjectiveSpace(9, 11881)>
gap> Unpack(UnderlyingObject(plane));
[ [ Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ] ]
gap> plane := NextIterator(iter);
<a plane in ProjectiveSpace(9, 11881)>
gap> Unpack(UnderlyingObject(plane));
[ [ Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), Z(109)^0 ] ]
gap> plane := NextIterator(iter);
<a plane in ProjectiveSpace(9, 11881)>
gap> Unpack(UnderlyingObject(plane));
[ [ Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), Z(109) ] ]
gap> plane := NextIterator(iter);
<a plane in ProjectiveSpace(9, 11881)>
gap> Unpack(UnderlyingObject(plane));
[ [ Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), 0*Z(109) ], 
  [ 0*Z(109), 0*Z(109), Z(109)^0, 0*Z(109), 0*Z(109), 0*Z(109), 0*Z(109), 
      0*Z(109), 0*Z(109), Z(109)^2 ] ]
gap> STOP_TEST("iterator.tst", 10000 );
