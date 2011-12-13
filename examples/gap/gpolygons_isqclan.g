#an easy example of a q clan check
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t * id );;
IsqClan( clan, f );
quit;

