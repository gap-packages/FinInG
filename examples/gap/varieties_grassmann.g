# Example for Grassmann varieties
pg:=PG(5,3);
planes:=Planes(pg);
gv:=GrassmannVariety(2,pg);
DefiningListOfPolynomials(gv);
Size(Points(gv));
gm:=GrassmannMap(gv);
quit;
