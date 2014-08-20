# in for classical generalised hexagons
ps := HyperbolicQuadric(7,5^3);
gh := TwistedTrialityHexagon(ps);
repeat
p := Random(Points(ps));
until p in gh;
time;
p in gh;
q := ElementToElement(gh,p);
l := Random(Lines(p));
l in gh;
List(Lines(q),x->x in gh);
quit;

