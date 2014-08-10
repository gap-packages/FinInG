#example of an enumerator
lines := Lines( ParabolicQuadric(6,3) );
enum := Enumerator( lines );
s := Size(enum);
n := Random([1..s]);
l := enum[n];
Position(enum, l);
quit;
