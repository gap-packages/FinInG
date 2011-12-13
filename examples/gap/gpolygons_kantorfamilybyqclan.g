## kantor family by q clan
f := GF(7);
id := IdentityMat(2, f);;
list := List( f, t -> t * id );;
clan := qClan(list,f);
fam := KantorFamilyByqClan(clan);
egq := EGQByKantorFamily(fam[1],fam[2],fam[3]);
quit;
