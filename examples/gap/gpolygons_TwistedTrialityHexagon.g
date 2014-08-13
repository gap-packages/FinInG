#T(q^3,q)
hexagon := TwistedTrialityHexagon(2^3);
AmbientPolarSpace(hexagon);
ps := HyperbolicQuadric(7,2^3);
hexagon := TwistedTrialityHexagon(ps);
AmbientPolarSpace(hexagon);
quit;
