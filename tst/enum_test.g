ps := ParabolicQuadric(6,3);
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
elements := List([1..s],x->enum!.ElementNumber(s,x));
List(elements,x->enum!.NumberElement(s,x));

for i in [1..Length(elements)] do
	Print(i,"\n");
	enum!.NumberElement(s,elements[i]);
od;

	
	
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
for q in [2,3,4,5,7,8,9,11,13,16,17,19,23,25,27,31,37,41,43,47,49] do
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
for q in [2,3,4,5,7] do
ps := EllipticQuadric(5,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;
time;
tests1;
time; #15 s

tests1 := [];
for q in [2,3,4,5,7] do
ps := HyperbolicQuadric(5,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests1,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;
time;
tests1;



tests1 := [];
tests2 := [];
ps := EllipticQuadric(5,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
tests1 := Filtered([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x);
tests2 := Filtered([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))<>x);



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


tests := [];
for q in [2,3,4,5,7,8,9,11] do #16s
ps := ParabolicQuadric(6,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4,5,7] do #86
ps := ParabolicQuadric(8,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;

tests := [];
for q in [2,3,4] do #51 s
ps := ParabolicQuadric(10,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;



elements := List([1..s],x->enum!.ElementNumber(s,x));;
List(elements,x->enum!.NumberElement(s,x));


tests := [];
for q in [3] do
ps := ParabolicQuadric(8,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Add(tests,Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x)));
od;




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


tests := [];
for q in [2,3,4,5,7,8,9,11,13,16,17,19,23,25,27,29,31,37,41,49] do
ps := EllipticQuadric(9,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Print(q,", ",s,", ",Type(els)," \c");
c := [];
for x in [1..s] do
Add(c,enum!.NumberElement(s,enum!.ElementNumber(s,x))=x);
od;
#c := Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x));
c := Collected(c);
Print(c,"\n");
Add(tests,c);
od;



for q in [2,3,4,5,7,8,9,11,13,16,17,19,23,25,27,29,31,37,41,49] do


tests := [];
for q in [2,3,4,5,7] do
ps := HermitianPolarSpace(5,q^2);
els := Lines(ps);
enum := Enumerator(els);
s := Size(enum);
Print(q,", ",s,", ",Type(els)," \c");
c := [];
for x in [1..s] do
Add(c,enum!.NumberElement(s,enum!.ElementNumber(s,x))=x);
od;
#c := Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x));
c := Collected(c);
Print(c,"\n");
Add(tests,c);
od;

tests := [];
for q in [2,3,4,5] do
ps := SymplecticSpace(7,q);
els := Points(ps);
enum := Enumerator(els);
s := Size(enum);
Print(q,", ",s,", ",Type(els)," \c");
c := [];
for x in [1..s] do
Add(c,enum!.NumberElement(s,enum!.ElementNumber(s,x))=x);
od;
#c := Collected(List([1..s],x->enum!.NumberElement(s,enum!.ElementNumber(s,x))=x));
c := Collected(c);
Print(c,"\n");
Add(tests,c);
od;


