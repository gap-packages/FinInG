# Segre variety. Notice ;; in line 9, since the number of generators can be different, and then ReadTest will print this difference. We do StructureDescription as a hard test here.
q := 2;
pg:=PG(1,q);
sv:=SegreVariety([pg,pg,pg]);
svpts:=AsList(Points(sv));;
Size(svpts);
pgbig:=AmbientSpace(sv);
g:=CollineationGroup(pgbig);
stab1:=FiningSetwiseStabiliser(g,AsSet(svpts));;
Order(stab1);
orbs1:=FiningOrbits(stab1,Points(pgbig));
o5:=orbs1[5];
pairs:=Combinations(o5,2);;
secants:=AsSet(List(pairs,x->Span(x[1],x[2])));;
4lines:=Filtered(secants,l->Size(Filtered(Points(l),x->x in o5))=q+1);
quit;
