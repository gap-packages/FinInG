#isometry groups of polar space
ps := SymplecticSpace(3,4);
IsometryGroup(ps);
ps := HyperbolicQuadric(5,8);
IsometryGroup(ps);
ps := EllipticQuadric(3,27);
IsometryGroup(ps);
ps := ParabolicQuadric(4,8);
IsometryGroup(ps);
ps := HermitianPolarSpace(4,9);
IsometryGroup(ps);
quit;
