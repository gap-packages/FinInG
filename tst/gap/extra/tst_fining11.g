#Test enumerators of symplectic polar spaces in odd char. Few cases not to exagerate. May take a few minutes.
ps := SymplecticSpace(3,9);
els := Points(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Lines(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
ps := SymplecticSpace(5,3);
els := Points(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Lines(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
els := Planes(ps);
enum := Enumerator(els);
Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
quit;
