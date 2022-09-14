#
gap> START_TEST("bugfix.tst");

# verify ElationOfProjectiveSpace does not run into a panic error
# see <https://github.com/gap-packages/FinInG/issues/14>
gap> ps := PG(2, 8);
ProjectiveSpace(2, 8)
gap> l := HyperplaneByDualCoordinates(ps, [1, 0, 0]*Z(8)^0);
<a line in ProjectiveSpace(2, 8)>
gap> p := VectorSpaceToElement(ps, [1, 0, 0]*Z(8)^0);
<a point in ProjectiveSpace(2, 8)>
gap> q := VectorSpaceToElement(ps, [1, 1, 0]*Z(8)^0);
<a point in ProjectiveSpace(2, 8)>
gap> ElationOfProjectiveSpace(l, p, q);
< a collineation: <cmat 3x3 over GF(2,3)>, F^0>

# verify SingerCycleCollineation does not run into an error,
# at least when using cvec >= 2.7.6;
# see <https://github.com/gap-packages/FinInG/issues/21>
gap> SingerCycleCollineation(2, 2^6);
< a collineation: <cmat 3x3 over GF(2,6)>, F^0>

#
gap> STOP_TEST("bugfix.tst", 1 );
