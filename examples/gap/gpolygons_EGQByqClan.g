## gpolygons_EGQByqClan.g
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t * id );;
IsqClan( clan, f );
egq := EGQByqClan( clan, f );
quit;
