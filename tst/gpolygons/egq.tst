gap> START_TEST("Forms: egq.tst");
gap> f := GF(3);
GF(3)
gap> id := IdentityMat(2, f);;
gap> clan := List( f, t -> t * id );;
gap> IsqClan( clan, f );
true
gap> f := GF(7);
GF(7)
gap> id := IdentityMat(2, f);;
gap> list := List( f, t -> t * id );;
gap> clan := qClan(list,f);
<q-clan over GF(7)>
gap> fam := KantorFamilyByqClan(clan);
[ <matrix group with 8 generators>, 
  [ <matrix group with 2 generators>, <matrix group with 2 generators>, 
      <matrix group with 2 generators>, <matrix group with 2 generators>, 
      <matrix group with 2 generators>, <matrix group with 2 generators>, 
      <matrix group with 2 generators>, <matrix group with 4 generators> ], 
  [ <matrix group with 4 generators>, <matrix group with 4 generators>, 
      <matrix group with 4 generators>, <matrix group with 4 generators>, 
      <matrix group with 4 generators>, <matrix group with 4 generators>, 
      <matrix group with 4 generators>, <matrix group with 6 generators> ] ]
gap> egq := EGQByKantorFamily(fam[1],fam[2],fam[3]);
<EGQ of order [ 49, 7 ] and basepoint 0>
gap> clan := KantorKnuthqClan(9);
<q-clan over GF(3^2)>
gap> blt := BLTSetByqClan(clan);
[ <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 9): x_1*x_5+x_2*x_4-x_3^2=0> ]
gap> egq := EGQByBLTSet(blt);
#I  Now embedding dual BLT-set into W(5,q)...
#I  Computing elation group...
<EGQ of order [ 81, 9 ] and basepoint in W(5, 9 ) >
gap> DefiningPlanesOfEGQByBLTSet(egq);
[ <a plane in W(5, 9)>, <a plane in W(5, 9)>, <a plane in W(5, 9)>, 
  <a plane in W(5, 9)>, <a plane in W(5, 9)>, <a plane in W(5, 9)>, 
  <a plane in W(5, 9)>, <a plane in W(5, 9)>, <a plane in W(5, 9)>, 
  <a plane in W(5, 9)> ]
gap> clan := FisherThasWalkerKantorBettenqClan(11);
<q-clan over GF(11)>
gap> blt := BLTSetByqClan(clan);
[ <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 11): x_1*x_5+x_2*x_4-x_3^2=0> ]
gap> clan := LinearqClan(3);
<q-clan over GF(3)>
gap> bltset := BLTSetByqClan( clan);
[ <a point in Q(4, 3): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 3): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 3): x_1*x_5+x_2*x_4-x_3^2=0>, 
  <a point in Q(4, 3): x_1*x_5+x_2*x_4-x_3^2=0> ]
gap> egq := EGQByBLTSet( bltset );
#I  Now embedding dual BLT-set into W(5,q)...
#I  Computing elation group...
<EGQ of order [ 9, 3 ] and basepoint in W(5, 3 ) >
gap> p := BasePointOfEGQ(egq);
<a point in <EGQ of order [ 9, 3 ] and basepoint in W(5, 3 ) >>
gap> UnderlyingObject(p);
<a point in W(5, 3)>
gap> group := ElationGroup(egq);
<projective collineation group with 5 generators>
gap> Order(group);
243
gap> g := ElementaryAbelianGroup(27);
<pc group of size 27 with 3 generators>
gap> flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
gap> flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]), 
>             Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];; 
gap> IsKantorFamily( g, flist1, flist2 );
#I  Checking tangency condition...
#I  Checking triple condition...
true
gap> egq := EGQByKantorFamily(g, flist1, flist2);
<EGQ of order [ 3, 3 ] and basepoint 0>
gap> CategoriesOfObject(egq);
[ "IsIncidenceStructure", "IsIncidenceGeometry", "IsGeneralisedPolygon", 
  "IsGeneralisedQuadrangle", "IsElationGQ", "IsElationGQByKantorFamily" ]
gap> group := ElationGroup(egq);
<pc group of size 27 with 3 generators>
gap> CollineationAction(group) = OnKantorFamily;
true
gap> l := ObjectToElement(egq,RightCoset(flist1[1],One(g)));
<a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>
gap> stab := Stabilizer(group,l,OnKantorFamily);
Group([ f1 ])
gap> pts := List(Points(egq));
[ <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a point of class 3 of <EGQ of order [ 3, 3 ] and basepoint 0>> ]
gap> Orbits(group,pts,OnKantorFamily);
[ [ <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a point of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a point of class 3 of <EGQ of order [ 3, 3 ] and basepoint 0>> ] ]
gap> lines := List(Lines(egq));
[ <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
  <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ]
gap> Orbits(group,lines,OnKantorFamily);
[ [ <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>>, 
      <a line of class 1 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ], 
  [ <a line of class 2 of <EGQ of order [ 3, 3 ] and basepoint 0>> ] ]
gap> STOP_TEST("egq.tst", 10000 );
