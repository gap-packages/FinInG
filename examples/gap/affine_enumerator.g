#enumerator in affine spaces
ag := AffineSpace(3, 3);
lines := Lines( ag );
enum := Enumerator( lines );
l := enum[20];
Display(l);
quit;
