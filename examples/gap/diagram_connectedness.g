#Connectedness
ps:=HyperbolicQuadric(7,2);
g:=IsometryGroup(ps);;
reps:=RepresentativesOfElements(ps);
solids:=Orbit(g,reps[4]);;
ps:=HyperbolicQuadric(7,2);
g:=IsometryGroup(ps);;
reps:=RepresentativesOfElements(ps);
h:=DerivedSubgroup(g);; # to get greek and latin solids
orbs:=FiningOrbits(h,Solids(ps));;
List(orbs, Size);
Filtered(orbs[2], s -> ProjectiveDimension(Meet(orbs[1][1],s))=2); # to
#find a latin incident with the greek which is orbs[1][1]
# Now we have a chamber
goodreps:=[reps[1],reps[2],orbs[1][1],last[1]];
pabs:=List(goodreps, r -> FiningStabiliser(h,r));
cos:=CosetGeometry(h,pabs);
IsConnected(cos);
IsResiduallyConnected(cos);
time;
quit;
