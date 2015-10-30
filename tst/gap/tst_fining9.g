#Test enumerators of hyperbolic quadrics in odd/even char. Few cases not to exagerate. May take a few minutes.
ps := HyperbolicQuadric(3,9);
els := Points(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Lines(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
ps := HyperbolicQuadric(5,2);
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
