gap> START_TEST("Forms: equationhyp.tst");
gap> pg := PG(4,23);
ProjectiveSpace(4, 23)
gap> hyp := HyperplaneByDualCoordinates(pg,[2,8,16,11,5]*Z(23)^0);
<a solid in ProjectiveSpace(4, 23)>
gap> vect := UnderlyingObject(hyp);
<immutable cmat 4x5 over GF(23,1)>
gap> Display(Unpack(vect));
  1  .  .  . 18
  .  1  .  .  3
  .  .  1  .  6
  .  .  .  1  7
gap> EquationOfHyperplane(hyp);
x_1+Z(23)^4*x_2+Z(23)^6*x_3+Z(23)^7*x_4+Z(23)^21*x_5
gap> STOP_TEST("equationhyp.tst", 10000 );
