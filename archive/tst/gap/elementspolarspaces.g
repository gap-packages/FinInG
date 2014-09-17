qlist := [2,3,4,5,7,8,9,11,13,16,17,19,23,25];

pslist := [];
n := 2;
for q in qlist do
  Add(pslist,ParabolicQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 3;
for q in qlist do
  Add(pslist,EllipticQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 3;
for q in qlist do
  Add(pslist,HyperbolicQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 4;
for q in qlist do
  Add(pslist,ParabolicQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 5;
for q in qlist do
  Add(pslist,EllipticQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 5;
for q in qlist do
  Add(pslist,HyperbolicQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 6;
for q in qlist do
  Add(pslist,ParabolicQuadric(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 3;
for q in qlist do
  Add(pslist,SymplecticSpace(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

pslist := [];
n := 5;
for q in qlist do
  Add(pslist,SymplecticSpace(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;

qlist := [4,9,16,25];
pslist := [];
n := 2;
for q in qlist do
  Add(pslist,HermitianVariety(n,q));
od;
testlist := [];
for ps in pslist do
  pts := List(Points(ps));;
  test := List(pts,x->IsIsotropicVector(SesquilinearForm(ps),Unwrap(x)));;
  Add(testlist,Set(test));
od;
#error in H(2,16)!! -> to be checked (jdb,16/01/09).
