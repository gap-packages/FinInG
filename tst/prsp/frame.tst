gap> START_TEST("Forms: frame.tst");
gap> frame := StandardFrame(PG(5,11));
[ <a point in ProjectiveSpace(5, 11)>, <a point in ProjectiveSpace(5, 11)>, 
  <a point in ProjectiveSpace(5, 11)>, <a point in ProjectiveSpace(5, 11)>, 
  <a point in ProjectiveSpace(5, 11)>, <a point in ProjectiveSpace(5, 11)>, 
  <a point in ProjectiveSpace(5, 11)> ]
gap> Length(frame);
7
gap> frame := StandardFrame(PG(2,19));
[ <a point in ProjectiveSpace(2, 19)>, <a point in ProjectiveSpace(2, 19)>, 
  <a point in ProjectiveSpace(2, 19)>, <a point in ProjectiveSpace(2, 19)> ]
gap> Length(frame);
4
gap> Display(frame);
[ NewVector(IsCVecRep,GF(19,1),[Z(19)^0,0*Z(19),0*Z(19),]), 
  NewVector(IsCVecRep,GF(19,1),[0*Z(19),Z(19)^0,0*Z(19),]), 
  NewVector(IsCVecRep,GF(19,1),[0*Z(19),0*Z(19),Z(19)^0,]), 
  NewVector(IsCVecRep,GF(19,1),[Z(19)^0,Z(19)^0,Z(19)^0,]) ]
gap> vects := List(frame,x->Unpack(UnderlyingObject(x)));
[ [ Z(19)^0, 0*Z(19), 0*Z(19) ], [ 0*Z(19), Z(19)^0, 0*Z(19) ], 
  [ 0*Z(19), 0*Z(19), Z(19)^0 ], [ Z(19)^0, Z(19)^0, Z(19)^0 ] ]
gap> Display(vects);
  1  .  .
  .  1  .
  .  .  1
  1  1  1
gap> STOP_TEST("frame.tst", 10000 );
