#base point of egq
clan := LinearqClan(3);
egq := EGQByqClan(clan);
blt := BLTSetByqClan(clan);
egq2 := EGQByBLTSet(blt);
BasePointOfEGQ(egq);
Display(last);
BasePointOfEGQ(egq2);
Display(last);
quit;
