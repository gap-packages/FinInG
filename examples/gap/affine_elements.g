## affine_elements.g
ag := AffineSpace(3, 3);
x := [[1,1,0]]*Z(3)^0;
v := [0,-1,1] * Z(3)^0;
line := AffineSubspace(ag, v, x);
quit;
