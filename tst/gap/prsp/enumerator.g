#Enumerator
pg := PG(3,4);
lines := Lines(pg);
enum := Enumerator(lines);;
Length(enum);
pg := PG(4,4);
lines := Lines(pg);
enum := Enumerator(lines);;
Length(enum);
planes := Planes(pg);
enum := Enumerator(lines);;
Length(enum);
quit;

