#Test enumerators of parabolic quadrics in even char. Few cases not to exagerate. May take a few minutes.
ps := ParabolicQuadric(4,7);
els := Points(ps);
enum := Enumerator(Points(ps));
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Lines(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
ps := ParabolicQuadric(6,3);
els := Points(ps);
enum := Enumerator(Points(ps));
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Lines(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Planes(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
quit;
