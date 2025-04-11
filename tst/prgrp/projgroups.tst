gap> START_TEST("Forms: projgroups.tst");
gap> pg := PG(5,179^2);
ProjectiveSpace(5, 32041)
gap> pgl := ProjectivityGroup(pg);
The FinInG projectivity group PGL(6,32041)
gap> BaseField(pgl);
GF(179^2)
gap> Dimension(pgl);
6
gap> pgl := HomographyGroup(pg);
The FinInG projectivity group PGL(6,32041)
gap> pgammal := CollineationGroup(pg);
The FinInG collineation group PGammaL(6,32041)
gap> BaseField(pgammal);
GF(179^2)
gap> Dimension(pgammal);
6
gap> psl := SpecialProjectivityGroup(pg);
The FinInG PSL group PSL(6,32041)
gap> psl := SpecialHomographyGroup(pg);
The FinInG PSL group PSL(6,32041)
gap> BaseField(psl);
GF(179^2)
gap> Dimension(psl);
6
gap> corr := CorrelationCollineationGroup(pg);
The FinInG correlation-collineation group PGammaL(6,32041) : 2
gap> BaseField(corr);
GF(179^2)
gap> Dimension(corr);
6
gap> phi := Embedding(pgammal,corr);
MappingByFunction( The FinInG collineation group PGammaL(6,32041), The FinInG \
correlation-collineation group PGammaL(6,32041) : 2, function( y ) ... end )
gap> gens := GeneratorsOfGroup(pgammal);
[ < a collineation: <cmat 6x6 over GF(179,2)>, F^0>, 
  < a collineation: <cmat 6x6 over GF(179,2)>, F^0>, 
  < a collineation: <cmat 6x6 over GF(179,2)>, F^179> ]
gap> el := Product(gens);
< a collineation: <cmat 6x6 over GF(179,2)>, F^179>
gap> Representative(el);
[ <immutable cmat 6x6 over GF(179,2)>, FrobeniusAutomorphism( GF(179^2) ) ]
gap> Representative(el^phi);
[ <immutable cmat 6x6 over GF(179,2)>, FrobeniusAutomorphism( GF(179^2) ), 
  IdentityMapping( <All elements of ProjectiveSpace(5, 32041)> ) ]
gap> STOP_TEST("projgroups.tst", 10000 );
