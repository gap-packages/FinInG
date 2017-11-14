gap> START_TEST("fining: tst_segrevariety.tst");
gap> # Segre variety. Notice ;; in line 9, since the number of generators can be different, and then ReadTest will print this difference. We do StructureDescription as a hard test here.
gap> q := 2;
2
gap> pg:=PG(1,q);
ProjectiveSpace(1, 2)
gap> sv:=SegreVariety([pg,pg,pg]);
Segre Variety in ProjectiveSpace(7, 2)
gap> svpts:=AsList(Points(sv));;
gap> Size(svpts);
27
gap> pgbig:=AmbientSpace(sv);
ProjectiveSpace(7, 2)
gap> g:=CollineationGroup(pgbig);
The FinInG collineation group PGL(8,2)
gap> stab1:=FiningSetwiseStabiliser(g,AsSet(svpts));;
#I  Computing adjusted stabilizer chain...
gap> Order(stab1);
1296
gap> orbs1:=FiningOrbits(stab1,Points(pgbig));
10%..31%..52%..95%..100%..[ <closed orbit, 27 points>, <closed orbit, 54 points>, 
  <closed orbit, 54 points>, <closed orbit, 108 points>, 
  <closed orbit, 12 points> ]
gap> o5:=orbs1[5];
<closed orbit, 12 points>
gap> pairs:=Combinations(o5,2);;
gap> secants:=AsSet(List(pairs,x->Span(x[1],x[2])));;
gap> 4lines:=Filtered(secants,l->Size(Filtered(Points(l),x->x in o5))=q+1);
[ <a line in ProjectiveSpace(7, 2)>, <a line in ProjectiveSpace(7, 2)>, 
  <a line in ProjectiveSpace(7, 2)>, <a line in ProjectiveSpace(7, 2)> ]
gap> STOP_TEST("tst_segrevariety.tst", 10000 );
