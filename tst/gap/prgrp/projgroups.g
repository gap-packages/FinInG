#PGammaL, PGL, PSL
pg := PG(5,179^2);
pgl := ProjectivityGroup(pg);
BaseField(pgl);
Dimension(pgl);
pgl := HomographyGroup(pg);
pgammal := CollineationGroup(pg);
BaseField(pgammal);
Dimension(pgammal);
psl := SpecialProjectivityGroup(pg);
psl := SpecialHomographyGroup(pg);
BaseField(psl);
Dimension(psl);
corr := CorrelationCollineationGroup(pg);
BaseField(corr);
Dimension(corr);
phi := Embedding(pgammal,corr);
gens := GeneratorsOfGroup(pgammal);
el := Product(gens);
Representative(el);
Representative(el^phi);
quit;



