## In this example, we construct a 2-ovoid of W(3, q), q = 5.
## The construction is as follows:
## (1) We start with an elliptic quadric Q- lying in PG(3,q).
## (2) We define a null polarity sigma such that at each point 
## of Q, there is a unique self-polar tangent line (w.r.t sigma). 
## (3) We use the two polarities to construct the 2-ovoid; 
## the orthogonal polarity rho arising from Q- and the null 
## polarity sigma.

q := 5; 
pg := ProjectiveSpace(3, q);
eq := EllipticQuadric(3, q);
eqpoints := Points( eq );
rho := PolarityOfProjectiveSpace( eq );

## The following is a Gram matrix for a nice symplectic space

mat := [[0,0,1,0], [0,0,0,1], [-1,0,0,0], [0,-1,0,0]] * Z(q)^0;
form := BilinearFormByMatrix(mat, GF(q));

w := PolarSpace( form );
sigma := PolarityOfProjectiveSpace( w );

# Now we test whether this symplectic space is the one we want

tangentlines := p -> ShadowOfFlag(pg, [p, p^rho], 2);
ForAll( eqpoints, p -> Number( tangentlines(p), l -> l^sigma = l ) = 1 );

# If the last line returns true, then we proceed to make the 2-ovoid

twoovoid := Union(AsSet(eqpoints), Set(eqpoints, x-> (x^rho)^sigma ) );;

# Now we test to see if this object really is a 2-ovoid

lines := Lines( w );
ForAll( lines, l -> Number(twoovoid, x -> x in l) = 2 );

# Now we compute the stabiliser of the 2-ovoid

g := CollineationGroup(w);
stab := SetwiseStabilizer(g, OnProjSubspaces, twoovoid )!.setstab;
StructureDescription(stab);

# The group should be C2 x (C13 : C4) 
