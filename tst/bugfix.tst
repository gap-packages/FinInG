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

# GAP >= 4.17 calls PreImagesSetNC internally
gap> d := NaturalDuality(SymplecticSpace(3, 3));;
gap> pts := AsList(Points(Range(d)!.geometry)){[1..3]};;
gap> PreImagesSetNC(d, pts) = List(pts, x -> PreImageElm(d, x));
true
gap> PreImagesSet(d, pts) = PreImagesSetNC(d, pts);
true

# verify IsFlagTransitiveGeometry checks every subset of types, not just
# suffixes: here flags of type {1,3,4} form two orbits;
# see <https://github.com/gap-packages/FinInG/issues/57>
gap> r0 := (1,2)(3,5)(4,6)(7,8);; r1 := (1,2)(3,7)(4,5)(6,8);;
gap> r2 := (1,3)(2,5)(4,7)(6,8);; r3 := (1,4)(2,6)(3,7)(5,8);;
gap> G := Group(r0, r1, r2, r3);;
gap> cg := CosetGeometry(G, [Subgroup(G, [r1, r2, r3]), Subgroup(G, [r0, r2, r3]),
>                            Subgroup(G, [r0, r1, r3]), Subgroup(G, [r0, r1, r2])]);;
gap> IsFlagTransitiveGeometry(cg);
false

#
gap> STOP_TEST("bugfix.tst", 1 );
