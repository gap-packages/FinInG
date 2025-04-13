gap> START_TEST("Forms: gpbyblocks.tst");
gap> blocks := [ 
>   [ 1, 2, 3, 4, 5 ], [ 1, 6, 7, 8, 9 ], [ 1, 10, 11, 12, 13 ],
>   [ 1, 14, 15, 16, 17 ], [ 1, 18, 19, 20, 21 ], [ 2, 6, 10, 14, 18 ], 
>   [ 2, 7, 11, 15, 19 ], [ 2, 8, 12, 16, 20 ], [ 2, 9, 13, 17, 21 ], 
>   [ 3, 6, 11, 16, 21 ], [ 3, 7, 10, 17, 20 ], [ 3, 8, 13, 14, 19 ], 
>   [ 3, 9, 12, 15, 18 ], [ 4, 6, 12, 17, 19 ], [ 4, 7, 13, 16, 18 ], 
>   [ 4, 8, 10, 15, 21 ], [ 4, 9, 11, 14, 20 ], [ 5, 6, 13, 15, 20 ], 
>   [ 5, 7, 12, 14, 21 ], [ 5, 8, 11, 17, 18 ], [ 5, 9, 10, 16, 19 ] ];;
gap> gp := GeneralisedPolygonByBlocks( blocks );
<projective plane order 4>
gap> List(Points(gp));
[ <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>>, 
  <a point in <projective plane order 4>> ]
gap> List(Lines(gp));
[ <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>>, 
  <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>>, 
  <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>>, 
  <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>>, 
  <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>>, 
  <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>>, 
  <a line in <projective plane order 4>>, <a line in <projective plane order 
    4>>, <a line in <projective plane order 4>> ]
gap> blocks := [ 
>   [ 1, 4, 5 ], [ 1, 8, 9 ], [ 1, 12, 13 ], [ 2, 4, 6 ], [ 2, 8, 10 ], 
>   [ 2, 12, 14 ], [ 3, 4, 7 ], [ 3, 8, 11 ], [ 3, 12, 15 ], [ 5, 10, 15 ], 
>   [ 5, 11, 14 ], [ 6, 9, 15 ], [ 7, 9, 14 ], [ 6, 11, 13 ], [ 7, 10, 13 ] ];;
gap> gp := GeneralisedPolygonByBlocks( blocks );
<generalised quadrangle of order [ 2, 2 ]>
gap> blocks := [ 
>   [ 1, 16, 17 ], [ 1, 32, 33 ], [ 1, 48, 49 ], [ 2, 8, 10 ], [ 2, 32, 34 ], 
>   [ 2, 40, 42 ], [ 3, 24, 27 ], [ 3, 32, 35 ], [ 3, 56, 59 ], [ 4, 8, 12 ], 
>   [ 4, 16, 20 ], [ 4, 24, 28 ], [ 5, 16, 21 ], [ 5, 40, 45 ], [ 5, 56, 61 ], 
>   [ 6, 8, 14 ], [ 6, 48, 54 ], [ 6, 56, 62 ], [ 7, 24, 31 ], [ 7, 40, 47 ], 
>   [ 7, 48, 55 ], [ 9, 20, 29 ], [ 9, 34, 43 ], [ 9, 54, 63 ], [ 10, 36, 46 ], 
>   [ 10, 38, 44 ], [ 11, 23, 28 ], [ 13, 39, 42 ], [ 13, 51, 62 ], 
>   [ 17, 36, 53 ], [ 17, 37, 52 ], [ 12, 18, 30 ], [ 15, 19, 28 ], 
>   [ 13, 20, 25 ], [ 25, 46, 55 ], [ 25, 35, 58 ], [ 12, 22, 26 ], 
>   [ 27, 36, 63 ], [ 27, 39, 60 ], [ 29, 38, 59 ], [ 29, 47, 50 ], 
>   [ 18, 33, 51 ], [ 19, 33, 50 ], [ 30, 37, 59 ], [ 15, 37, 42 ], 
>   [ 30, 41, 55 ], [ 11, 34, 41 ], [ 21, 41, 60 ], [ 31, 43, 52 ], 
>   [ 18, 45, 63 ], [ 23, 45, 58 ], [ 22, 39, 49 ], [ 23, 38, 49 ], 
>   [ 14, 50, 60 ], [ 31, 44, 51 ], [ 26, 47, 53 ], [ 11, 53, 62 ], 
>   [ 26, 35, 57 ], [ 21, 44, 57 ], [ 15, 54, 57 ], [ 14, 52, 58 ], 
>   [ 22, 43, 61 ], [ 19, 46, 61 ] ];;
gap> gp := GeneralisedPolygonByBlocks( blocks );
<generalised hexagon of order [ 2, 2 ]>
gap> p := ObjectToElement(gp,1,1);
<a point in <generalised hexagon of order [ 2, 2 ]>>
gap> q := ObjectToElement(gp,1,16);
<a point in <generalised hexagon of order [ 2, 2 ]>>
gap> s := Span(p,q);
<a line in <generalised hexagon of order [ 2, 2 ]>>
gap> p * s;
true
gap> s * q;
true
gap> l := ObjectToElement(gp,2,[7,24,31]);
<a line in <generalised hexagon of order [ 2, 2 ]>>
gap> m := ObjectToElement(gp,2,[7,40,47]);
<a line in <generalised hexagon of order [ 2, 2 ]>>
gap> r := Meet(l,m);
<a point in <generalised hexagon of order [ 2, 2 ]>>
gap> r * l;
true
gap> m * r;
true
gap> List(Lines(p));
[ <a line in <generalised hexagon of order [ 2, 2 ]>>, 
  <a line in <generalised hexagon of order [ 2, 2 ]>>, 
  <a line in <generalised hexagon of order [ 2, 2 ]>> ]
gap> List(Points(l));
[ <a point in <generalised hexagon of order [ 2, 2 ]>>, 
  <a point in <generalised hexagon of order [ 2, 2 ]>>, 
  <a point in <generalised hexagon of order [ 2, 2 ]>> ]
gap> matrix := IncidenceMatrixOfGeneralisedPolygon(gp);;
gap> gp2 := GeneralisedPolygonByIncidenceMatrix(matrix);
<generalised hexagon of order [ 2, 2 ]>
gap> STOP_TEST("gpbyblocks.tst", 10000 );
