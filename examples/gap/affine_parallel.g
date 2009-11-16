# example: affine_parallel.g
as := AffineSpace(3, 3);
l := Random( Lines( as ) );
pclass := ParallelClass( l );
AsList(pclass);
quit;
