#meet stuff for cps's
ps := HyperbolicQuadric(5,3);
pi := Random( Planes(ps) );
tau := Random( Planes(ps) );
Meet(pi,tau);
quit;
