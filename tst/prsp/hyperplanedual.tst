gap> START_TEST("Forms: hyperplanedual.tst");
gap> ps := PG(8,13);
ProjectiveSpace(8, 13)
gap> hyp := HyperplaneByDualCoordinates(ps,[1,2,3,4,5,6,7,8,9]*Z(13)^0);
<a proj. 7-space in ProjectiveSpace(8, 13)>
gap> vect := UnderlyingObject(hyp);
<immutable cmat 8x9 over GF(13,1)>
gap> Display(Unpack(vect));
  1  .  .  .  .  .  .  . 10
  .  1  .  .  .  .  .  .  7
  .  .  1  .  .  .  .  .  4
  .  .  .  1  .  .  .  .  1
  .  .  .  .  1  .  .  . 11
  .  .  .  .  .  1  .  .  8
  .  .  .  .  .  .  1  .  5
  .  .  .  .  .  .  .  1  2
gap> STOP_TEST("hyperplanedual.tst", 10000 );
