# Stabilisers of subspaces, superfast!
pg:=ProjectiveSpace(5,9);            
sub:=RandomSubspace(pg,2);     
coll:=StabiliserGroupOfSubspace(sub); time;                                
computed:=FiningStabiliserOrb(CollineationGroup(pg),sub); 
time;
coll = computed;
proj:=ProjectiveStabiliserGroupOfSubspace(sub); 
time;
FiningStabiliserOrb(ProjectivityGroup(pg),sub)=proj; 
time;      
specproj:=SpecialProjectiveStabiliserGroupOfSubspace(sub); 
time;
specproj = FiningStabiliserOrb(SpecialProjectivityGroup(pg),sub); 
time;
quit;
