## affine_iterator.g
ag := AffineSpace(3, 3);
lines := Lines( ag );
iter := Iterator( lines );
l := NextIterator( iter );
quit;
