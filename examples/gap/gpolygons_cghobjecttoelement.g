#object to element for classical generalised hexagons
mat := IdentityMat(8,GF(5^3));
form := BilinearFormByMatrix(mat,GF(5^3));
ps := PolarSpace(form);
gh := TwistedTrialityHexagon(ps);
vec := [ Z(5)^0, Z(5^3)^55, Z(5^3)^99, Z(5^3)^107, Z(5^3)^8, Z(5^3)^35, Z(5^3)^73, 
  Z(5^3)^115 ];
p := ObjectToElement(gh,vec);
vec := [ [ Z(5)^0, 0*Z(5), Z(5^3)^76, Z(5^3)^117, Z(5^3)^80, Z(5^3)^19, Z(5^3)^48, 
      Z(5^3)^100 ], 
  [ 0*Z(5), Z(5)^0, Z(5^3)^115, Z(5^3)^14, Z(5^3)^40, Z(5^3)^67, Z(5^3)^123, 
      Z(5^3)^3 ] ];
line := ObjectToElement(gh,vec);
quit;


