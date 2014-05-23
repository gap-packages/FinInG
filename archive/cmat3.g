x := [ [ Z(3^4)^3, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3^4)^27 ], 
  [ 0*Z(3), Z(3^4)^3, 0*Z(3), 0*Z(3), Z(3^4)^27, 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3^4)^3, Z(3^4)^27, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3^4)^27, Z(3^4)^3, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), Z(3^4)^27, 0*Z(3), 0*Z(3), Z(3^4)^3, 0*Z(3) ], 
  [ Z(3^4)^27, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3^4)^3 ] ];

xinv := x^-1;

frob := FrobeniusAutomorphism(GF(81))^3;

xfrob := List(x,v->List(v,u->u^frob));

mat := [ [ Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3) ] ];

newmat :=  xinv*mat*xfrob;

NewMatrix(IsCMatRep,GF(9),6,newmat);

newmat2 := [ [ Z(3^2)^3, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0 ], 
  [ 0*Z(3), Z(3^2)^3, 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3^2)^3, Z(3)^0, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0, Z(3^2)^3, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3), Z(3^2)^3, 0*Z(3) ], 
  [ Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3^2)^3 ] ];

newmat2 = newmat;

DefaultFieldOfMatrix(newmat2);

NewMatrix(IsCMatRep,GF(9),6,newmat2); #this will give an error

TypeObj(newmat)=TypeObj(newmat2);

ConvertToMatrixRep(newmat);

NewMatrix(IsCMatRep,GF(9),6,newmat); #now it will work



