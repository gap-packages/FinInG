#examples for Segre varieties
sv:=SegreVariety(2,2,9);
sm:=SegreMap(sv);
cart1:=Cartesian(Points(PG(2,9)),Points(PG(2,9)));;
im1:=ImagesSet(sm,cart1);;
Span(im1);
l:=Random(Lines(PG(2,9)));
cart2:=Cartesian(Points(l),Points(PG(2,9)));;
im2:=ImagesSet(sm,cart2);;
Span(im2);
x:=Random(Points(PG(2,9)));
cart3:=Cartesian(Points(PG(2,9)),Points(x));;
im3:=ImagesSet(sm,cart3);;
pi:=Span(im3);
AsSet(List(Points(pi),y->y in sv));
quit;
