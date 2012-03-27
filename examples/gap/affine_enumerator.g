#enumerator in affine spaces
gap> ag := AffineSpace(3, 3);
gap> lines := Lines( ag );
gap> enum := Enumerator( lines );
gap> l := enum[20];
gap> Display(l);
quit;
