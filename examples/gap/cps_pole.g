# pole for elements in projective spaces wrt a polar space
conic := ParabolicQuadric(2,13);
p := VectorSpaceToElement(PG(2,13),[1,0,0]*Z(13)^0);
pole := Pole(conic,p);
TypeOfSubspace(conic,pole);
tangent_points := Filtered(Points(pole),x->x in conic);
tangent_lines_on_p := List(tangent_points,x->Span(x,p));
List(tangent_lines_on_p,x->Number(Points(x),y->y in conic));
quit;

