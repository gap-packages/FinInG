#############################################################################
##
##  varieties.gd              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2018	Colorado State University
##                  Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universiteit Gent
##					University of St. Andrews
##					University of Western Australia
##                  Vrije Universiteit Brussel
##
##
##  Declarations for algebraic varieties
##
#############################################################################


#############################################################################
##
##  Categories and families
##
#############################################################################

### 1. Algebraic Varieties ###

DeclareCategory( "IsAlgebraicVariety", IsAttributeStoringRep );
DeclareRepresentation("IsAlgebraicVarietyRep", IsAlgebraicVariety, ["geometry","polring", "listofpols"] );
DeclareCategory( "IsPointsOfAlgebraicVariety", IsDomain and IsCollection and IsComponentObjectRep );
DeclareRepresentation( "IsPointsOfAlgebraicVarietyRep", IsPointsOfAlgebraicVariety, ["variety"]);


### 2. Projective Varieties ###

DeclareCategory( "IsProjectiveVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsProjectiveVarietyRep", IsProjectiveVariety and IsAlgebraicVarietyRep, ["geometry","polring","listofpols"]);

DeclareCategory( "IsHermitianVariety", IsProjectiveVariety );
DeclareRepresentation( "IsHermitianVarietyRep", IsHermitianVariety and IsProjectiveVarietyRep, ["geometry","polring","listofpols"]);

DeclareCategory( "IsQuadraticVariety", IsProjectiveVariety );
DeclareRepresentation( "IsQuadraticVarietyRep", IsQuadraticVariety and IsProjectiveVarietyRep, ["geometry","polring","listofpols"]);

### 3. Affine Varieties ###

DeclareCategory( "IsAffineVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsAffineVarietyRep", IsAffineVariety and IsAlgebraicVarietyRep, ["geometry","polring","listofpols"]);


### 4. Segre Varieties ###

DeclareCategory("IsSegreVariety", IsProjectiveVariety );
DeclareRepresentation("IsSegreVarietyRep", IsSegreVariety and IsProjectiveVarietyRep, 
				["geometry","listofpols","inverseimage", "polring"]);
DeclareCategory( "IsPointsOfSegreVariety", IsPointsOfAlgebraicVariety );
DeclareRepresentation( "IsPointsOfSegreVarietyRep", IsPointsOfSegreVariety, ["variety"]);

DeclareCategory( "IsGeometryMap", IsAttributeStoringRep );
DeclareRepresentation ( "IsGeometryMapRep", IsGeometryMap, ["source","range","function"] );

DeclareCategory( "IsSegreMap", IsGeometryMap );
DeclareRepresentation( "IsSegreMapRep", IsSegreMap, ["source", "range", "map"] );

### 5. Veronese Varities ###
DeclareCategory("IsVeroneseVariety", IsProjectiveVariety );
DeclareRepresentation("IsVeroneseVarietyRep", IsVeroneseVariety and IsProjectiveVarietyRep, 
				["geometry","listofpols","inverseimage", "polring"]);
				
DeclareCategory( "IsPointsOfVeroneseVariety", IsPointsOfAlgebraicVariety );
DeclareRepresentation( "IsPointsOfVeroneseVarietyRep", IsPointsOfVeroneseVariety, ["variety"]);

DeclareCategory( "IsVeroneseMap", IsGeometryMap );
DeclareRepresentation( "IsVeroneseMapRep", IsVeroneseMap, ["source", "range", "map"] );

### 5. Grassmann Varieties ###
DeclareCategory("IsGrassmannVariety", IsProjectiveVariety );
DeclareRepresentation("IsGrassmannVarietyRep", IsGrassmannVariety and IsProjectiveVarietyRep, 
				["geometry","listofpols","inverseimage", "polring"]);

DeclareCategory( "IsPointsOfGrassmannVariety", IsPointsOfAlgebraicVariety );
DeclareRepresentation( "IsPointsOfGrassmannVarietyRep", IsPointsOfGrassmannVariety, ["variety"]);

DeclareCategory( "IsGrassmannMap", IsGeometryMap );
DeclareRepresentation( "IsGrassmannMapRep", IsGrassmannMap, ["source", "range", "map"] );

#############################################################################
##
##  Operations and Attributes
##
#############################################################################

### 1. Algebraic Varieties ###
DeclareOperation( "AlgebraicVariety", [IsProjectiveSpace, IsList] );
DeclareOperation( "AlgebraicVariety", [IsAffineSpace, IsList] );
DeclareOperation( "AlgebraicVariety", [IsProjectiveSpace, IsPolynomialRing, IsList] );
DeclareOperation( "AlgebraicVariety", [IsAffineSpace, IsPolynomialRing, IsList] );
DeclareAttribute( "DefiningListOfPolynomials", IsAlgebraicVariety );
DeclareOperation( "PointsOfAlgebraicVariety", [IsAlgebraicVariety] );
DeclareOperation( "Points", [IsAlgebraicVariety] );
DeclareAttribute( "AmbientSpace", IsAlgebraicVariety );

### 2. Projective Varieties ###

DeclareOperation( "ProjectiveVariety", [IsProjectiveSpace, IsPolynomialRing, IsList] );
DeclareOperation( "ProjectiveVariety", [IsProjectiveSpace, IsList] );
#DeclareOperation( "DualCoordinatesOfHyperplane", [IsSubspaceOfProjectiveSpace] ); #moved to projectivespace.gd
#DeclareOperation( "HyperplaneByDualCoordinates", [IsProjectiveSpace,IsList] );

DeclareOperation( "HermitianVariety", [IsPosInt,IsField] );
DeclareOperation( "HermitianVariety", [IsPosInt,IsPosInt] );
DeclareOperation( "HermitianVariety", [IsProjectiveSpace, IsPolynomialRing, IsPolynomial ] );
DeclareOperation( "HermitianVariety", [IsProjectiveSpace, IsPolynomial ] );

DeclareOperation( "QuadraticVariety", [IsPosInt,IsField] );
DeclareOperation( "QuadraticVariety", [IsPosInt,IsField,IsString] );
DeclareOperation( "QuadraticVariety", [IsPosInt,IsPosInt] );
DeclareOperation( "QuadraticVariety", [IsPosInt,IsPosInt,IsString] );
DeclareOperation( "QuadraticVariety", [IsProjectiveSpace, IsPolynomialRing, IsPolynomial ] );
DeclareOperation( "QuadraticVariety", [IsProjectiveSpace, IsPolynomial ] );

DeclareAttribute( "SesquilinearForm", IsHermitianVariety );
DeclareAttribute( "QuadraticForm", IsQuadraticVariety );

DeclareProperty( "IsStandardHermitianVariety", IsHermitianVariety );
DeclareProperty( "IsStandardQuadraticVariety", IsQuadraticVariety );

DeclareOperation( "PolarSpace", [IsProjectiveVariety] );


### 3. Affine Varieties ###

DeclareOperation( "AffineVariety", [IsAffineSpace, IsPolynomialRing, IsList] );
DeclareOperation( "AffineVariety", [IsAffineSpace, IsList] );

### 4. Segre Varieties ###
DeclareOperation( "SegreMap", [ IsHomogeneousList ] ); # list of projective spaces
DeclareOperation( "SegreMap", [ IsHomogeneousList, IsField ] ); # list of dimensions

DeclareOperation( "SegreVariety", [ IsHomogeneousList ]);  
DeclareOperation( "SegreVariety", [ IsHomogeneousList, IsField ]); 
DeclareOperation("PointsOfSegreVariety", [IsSegreVariety] );
DeclareOperation("SegreMap", [IsSegreVariety]);

# Some extra functionality for two factors 
DeclareOperation( "SegreMap", [ IsProjectiveSpace, IsProjectiveSpace ]);  
DeclareOperation( "SegreMap", [ IsPosInt, IsPosInt, IsField ]);
DeclareOperation( "SegreMap", [ IsPosInt, IsPosInt, IsPosInt ]);

DeclareOperation( "SegreVariety", [ IsProjectiveSpace, IsProjectiveSpace ]);  
DeclareOperation( "SegreVariety", [ IsPosInt, IsPosInt, IsField ]);
DeclareOperation( "SegreVariety", [ IsPosInt, IsPosInt, IsPosInt ]);



### 5. Veronese Varities ###
DeclareOperation( "VeroneseMap", [ IsProjectiveSpace ]);
DeclareOperation( "VeroneseMap", [ IsPosInt, IsField ]);
DeclareOperation( "VeroneseMap", [ IsPosInt, IsPosInt ]);

DeclareOperation( "VeroneseVariety", [IsProjectiveSpace ]);
DeclareOperation( "VeroneseVariety", [IsPosInt, IsField ]);
DeclareOperation( "VeroneseVariety", [IsPosInt, IsPosInt]);

DeclareOperation( "PointsOfVeroneseVariety", [IsVeroneseVariety]);

DeclareOperation( "VeroneseMap", [IsVeroneseVariety]);

### 6. GeometryMaps
DeclareAttribute("Source", IsGeometryMap);
DeclareAttribute("Range", IsGeometryMap);

### 7. Grassmann Varieties ###
DeclareOperation( "GrassmannCoordinates", [ IsSubspaceOfProjectiveSpace ]);

DeclareOperation( "GrassmannMap", [ IsPosInt,IsProjectiveSpace ] );
DeclareOperation( "GrassmannMap", [ IsPosInt, IsPosInt, IsPosInt ]);
DeclareOperation( "GrassmannMap", [ IsSubspacesOfProjectiveSpace ]);
DeclareOperation( "GrassmannMap", [ IsGrassmannVariety ] );


DeclareOperation( "GrassmannVariety", [ IsPosInt,IsProjectiveSpace ] );
DeclareOperation( "GrassmannVariety", [IsPosInt, IsPosInt, IsField]);
DeclareOperation( "GrassmannVariety", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation( "GrassmannVariety", [ IsSubspacesOfProjectiveSpace ]);

DeclareOperation( "PointsOfGrassmannVariety", [IsGrassmannVariety] );


### 8. Miscellaneous ###
DeclareOperation( "ConicOnFivePoints",  [ IsHomogeneousList and
                              IsSubspaceOfProjectiveSpaceCollection ] );


#############################################################################
#temporarily declarations, to be continued ...
#############################################################################




## List of dimensions and field

