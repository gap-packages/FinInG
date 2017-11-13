#Test enumerators of elliptic quadrics in even char. Few cases not to exagerate. May take a few minutes.
ps := EllipticQuadric(3,7);
els := Points(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
ps := EllipticQuadric(5,3);
els := Points(ps);
enum := Enumerator(Points(ps));
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Lines(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
ps := EllipticQuadric(7,2);
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

