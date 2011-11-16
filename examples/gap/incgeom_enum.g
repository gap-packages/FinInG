#example of an enumerator
enum := Enumerator(Lines(ParabolicQuadric(6,2)));
s := Size(enum);
n := Random([1..s]);
l := enum!.ElementNumber(s,n);
enum!.NumberElement(s,l);
quit;
