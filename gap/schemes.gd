#############################################################################
##
##  schemes.gd              FinInG package
##                                                              John Bamberg
## 								Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
## 							      Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2006 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Declaration stuff for projective algebraic varieties
##
#############################################################################

#############################################################################
##
##  Categories and families
##
#############################################################################

DeclareCategory( "IsProjectiveAlgebraicVariety", IsLieGeometry );
DeclareRepresentation( "IsProjectiveAlgebraicVarietyRep", IsProjectiveAlgebraicVariety, [ "basefield", "vectorspace", "poly" ] );

DeclareCategory( "IsElementOfProjectiveAlgebraicVariety", IsSubspaceOfProjectiveSpace );
DeclareRepresentation( "IsElementOfProjectiveAlgebraicVarietyRep", IsElementOfProjectiveAlgebraicVariety, [ "geo", "type", "obj" ]);
DeclareCategory( "IsAllElementsOfProjectiveAlgebraicVariety", IsAllElementsOfLieGeometry );
DeclareRepresentation( "IsAllElementsOfProjectiveAlgebraicVarietyRep", IsAllElementsOfProjectiveAlgebraicVariety, [ "geometry", "type" ] );

DeclareCategory( "IsGrassmannVariety", IsProjectiveAlgebraicVariety );
DeclareRepresentation( "IsGrassmannVarietyRep", IsGrassmannVariety, [ "basefield", "ddim", "kdim" ] );

DeclareCategory( "IsElementOfGrassmannVariety", IsElementOfProjectiveAlgebraicVariety );
DeclareRepresentation( "IsElementOfGrassmannVarietyRep", IsElementOfIncidenceStructureRep and IsElementOfProjectiveAlgebraicVariety, [ "geo", "type", "obj" ]);
DeclareCategory( "IsAllElementsOfGrassmannVariety", IsAllElementsOfProjectiveAlgebraicVariety );
DeclareRepresentation( "IsAllElementsOfGrassmannVarietyRep", IsAllElementsOfProjectiveAlgebraicVarietyRep, [ "geometry", "type" ] );


DeclareCategory( "IsVeroneseVariety", IsProjectiveAlgebraicVariety  );
DeclareRepresentation( "IsVeroneseVarietyRep", IsVeroneseVariety, [ "basefield", "vectorspace" ] );
DeclareCategory( "IsElementOfVeroneseVariety", IsElementOfProjectiveAlgebraicVariety );
DeclareRepresentation( "IsElementOfVeroneseVarietyRep", IsElementOfIncidenceStructureRep and IsElementOfVeroneseVariety, [ "geo", "type", "obj" ]);
DeclareCategory( "IsAllElementsOfVeroneseVariety", IsAllElementsOfProjectiveAlgebraicVariety );
DeclareRepresentation( "IsAllElementsOfVeroneseVarietyRep", IsAllElementsOfProjectiveAlgebraicVarietyRep, [ "geometry", "type" ] );

DeclareCategory( "IsSegreVariety", IsProjectiveAlgebraicVariety  );
DeclareRepresentation( "IsSegreVarietyRep", IsSegreVariety, [ "basefield", "vectorspace", "dimlist" ] );
DeclareCategory( "IsElementOfSegreVariety", IsElementOfProjectiveAlgebraicVariety );
DeclareRepresentation( "IsElementOfSegreVarietyRep", IsElementOfIncidenceStructureRep and IsElementOfSegreVariety, [ "geo", "type", "obj" ]);
DeclareCategory( "IsAllElementsOfSegreVariety", IsAllElementsOfProjectiveAlgebraicVariety );
DeclareRepresentation( "IsAllElementsOfSegreVarietyRep", IsAllElementsOfProjectiveAlgebraicVarietyRep, [ "geometry", "type" ] );



#############################################################################
##
##  Operations and Attributes
##
##  Note: the attribute "RankAttr" is already defined in geometry.gd,
##  as well as "ElementTypes", "CollineationGroup", "CorrelationGroup".
##
#############################################################################

DeclareOperation( "ProjectiveAlgebraicVariety", [IsProjectiveSpace, IsPolynomial]);
DeclareOperation( "NrIndeterminatesPolynomial", [IsPolynomial]);

DeclareOperation( "GrassmannVariety", [IsPosInt, IsPosInt, IsField]);
DeclareOperation( "GrassmannVariety", [IsPosInt, IsPosInt, IsPosInt]);

DeclareOperation( "VeroneseVariety", [IsPosInt, IsField ]);
DeclareOperation( "VeroneseVariety", [IsPosInt, IsPosInt]);

## List of projective spaces
DeclareOperation( "SegreVariety", [ IsHomogeneousList ]);  

## List of dimensions and field
DeclareOperation( "SegreVariety", [ IsHomogeneousList, IsField ]);  

## Just two projective spaces
DeclareOperation( "SegreVariety", [ IsProjectiveSpace, IsProjectiveSpace ]);  
DeclareOperation( "SegreVariety", [ IsPosInt, IsPosInt, IsField ]);  

DeclareOperation( "ProductOfIterators", [IsList]);
DeclareOperation( "ConicOnFivePoints",  [ IsHomogeneousList and 
                              IsSubspaceOfProjectiveSpaceCollection ] );








