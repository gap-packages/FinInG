# collineation group
gp := EllipticQuadric(5,4);
CollineationGroup(gp);
gp := TwistedTrialityHexagon(3^3);
CollineationGroup(gp);
time;
mat := [ [ 1, 1, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 1, 1, 0, 0 ], 
	 [ 1, 0, 1, 0, 0, 0, 1 ], [ 0, 1, 1, 1, 0, 0, 0 ], 
	 [ 0, 1, 0, 0, 1, 0, 1 ], [ 0, 0, 0, 1, 0, 1, 1 ], 
  	 [ 0, 0, 1, 0, 1, 1, 0 ] ];
gp := GeneralisedPolygonByIncidenceMatrix(mat);
group := CollineationGroup(gp);
gp := EGQByqClan(FisherqClan(3));
group := CollineationGroup(gp);
Order(group);
Random(group);
quit;
