ps := ParabolicQuadric(6,2);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
elements := List([1..s],x->enum!.ElementNumber(s,x));
List(elements,x->enum!.NumberElement(s,x));

tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := ParabolicQuadric(2,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := EllipticQuadric(3,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := HyperbolicQuadric(3,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := HyperbolicQuadric(3,q);
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := ParabolicQuadric(4,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := ParabolicQuadric(4,q);
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests1 := [];
tests2 := [];
for q in [2,3,4,5,7] do
ps := EllipticQuadric(5,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests2,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;
time; #15 s

tests1 := [];
tests2 := [];
for q in [2,3] do
ps := HyperbolicQuadric(5,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Planes(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests2,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;



elements := List([1..s],x->enum!.ElementNumber(s,x));;
List(elements,x->enum!.NumberElement(s,x));



tests1 := [];
tests2 := [];
for q in [2,3,4,5,7,8,9,11,13,16,17] do
ps := SymplecticSpace(3,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests2,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;
time; #15 s

tests1 := [];
tests2 := [];
tests3 := [];
#for q in [2,3,4,5,7,8,9,11,13,16,17] do
for q in [2,3,4,5] do
ps := SymplecticSpace(5,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests2,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Planes(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests3,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;
time; #1966 s

tests1 := [];
tests2 := [];
tests3 := [];
tests4 := [];
for q in [2,3] do
ps := SymplecticSpace(7,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests2,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Planes(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests3,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
els := Solids(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests4,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;
time;
tests1;
tests2;
tests3;
tests4;

tests := [];
for q in [2,3,4,5,7,8,9,11,13,17,19] do
ps := HermitianPolarSpace(2,q^2);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;


q := 3;
d := 6;
ps := ParabolicQuadric(d,q);
s := Size(Points(ps));
elements := List([1..s],n->VectorSpaceToElement(ps, QElementNumber(d, q, n)));
Set(last) = Set(AsList(Points(ps)));
