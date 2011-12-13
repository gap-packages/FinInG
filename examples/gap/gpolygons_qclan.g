#an easy example of a q clan
f := GF(7);
id := IdentityMat(2, f);;
clan := List( f, t -> t * id );;
clan := qClan( clan, f );
quit;

