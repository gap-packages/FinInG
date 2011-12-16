#############################################################################
##
##  varieties.gd              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2011	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
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

### 1. Algbraic Varieties ###

DeclareCategory( "IsAlgebraicVariety", IsAttributeStoringRep );
DeclareRepresentation("IsAlgebraicVarietyRep", IsAlgebraicVariety, ["geometry","polring", "listofpols"] );
DeclareCategory( "IsPointsOfAlgebraicVariety", IsDomain and IsCollection and IsComponentObjectRep );
DeclareRepresentation( "IsPointsOfAlgebraicVarietyRep", IsPointsOfAlgebraicVariety, ["variety"]);


### 2. Projective Varieties ###

DeclareCategory( "IsProjectiveVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsProjectiveVarietyRep", IsProjectiveVariety and IsAlgebraicVarietyRep, ["geometry","polring","listofpols"]);


### 3. Affine Varieties ###

DeclareCategory( "IsAffineVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsAffineVarietyRep", IsAffineVariety and IsAlgebraicVarietyRep, ["geometry","polring","listofpols"]);


### 4. Segre Varieties ###

DeclareCategory("IsSegreVariety", IsProjectiveVariety );
DeclareRepresentation("IsSegreVarietyRep", IsSegreVariety and IsProjectiveVarietyRep, 
				["geometry","listofpols","inverseimage", "polring"]);
DeclareCategory( "IsPointsOfSegreVariety", IsPointsOfAlgebraicVariety );
DeclareRepresentation( "IsPointsOfSegreVarietyRep", IsPointsOfSegreVariety, ["variety"]);

DeclareCategory( "IsGeometryMap", IsFunction );
DeclareRepresentation ( "IsGeometryMapRep", IsGeometryMap, ["source","range","function"] );

DeclareCategory( "IsSegreMap", IsGeometryMap );
DeclareRepresentation( "IsSegreMapRep", IsSegreMap, ["source", "range", "segremap"] );

### 5. Veronese Varities ###
DeclareCategory("IsVeroneseVariety", IsProjectiveVariety );
DeclareRepresentation("IsVeroneseVarietyRep", IsVeroneseVariety and IsProjectiveVarietyRep, 
				["geometry","listofpols","inverseimage","veronesemap", "polring"]);
DeclareCategory( "IsPointsOfVeroneseVariety", IsPointsOfAlgebraicVariety );
DeclareRepresentation( "IsPointsOfVeroneseVarietyRep", IsPointsOfVeroneseVariety, ["variety"]);


#############################################################################
##
##  Operations and Attributes
##
#############################################################################

### 1. Algebraic Varieties ###
DeclareOperation( "AlgebraicVariety", [IsProjectiveSpace, IsList] );
DeclareOperation( "AlgebraicVariety", [IsAffineSpace, IsList] );
DeclareAttribute( "DefiningListOfPolynomials", IsAlgebraicVariety );
DeclareOperation( "PointsOfAlgebraicVariety", [IsAlgebraicVariety] );
DeclareOperation( "Points", [IsAlgebraicVariety] );
DeclareAttribute( "AmbientSpace", IsAlgebraicVariety );

### 2. Projective Varieties ###

DeclareOperation( "ProjectiveVariety", [IsProjectiveSpace, IsPolynomialRing, IsList] );
DeclareOperation( "ProjectiveVariety", [IsProjectiveSpace, IsList] );
DeclareOperation( "DualCoordinatesOfHyperplane", [IsSubspaceOfProjectiveSpace] );
DeclareOperation( "HyperplaneByDualCoordinates", [IsProjectiveSpace,IsList] );

### 3. Affine Varieties ###

DeclareOperation( "AffineVariety", [IsAffineSpace, IsPolynomialRing, IsList] );
DeclareOperation( "AffineVariety", [IsAffineSpace, IsList] );

### 4. Segre Varieties ###
DeclareOperation( "SegreMap", [ IsHomogeneousList ] ); # list of projective spaces
DeclareOperation( "SegreMap", [ IsHomogeneousList, IsField ] ); # list of dimensions

DeclareOperation( "SegreVariety", [ IsHomogeneousList ]);  
DeclareOperation( "SegreVariety", [ IsHomogeneousList, IsField ]); 
DeclareOperation("PointsOfSegreVariety", [IsSegreVariety] );
#DeclareOperation("Points", [IsSegreVariety] );
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
#DeclareOperation( "Points", [IsVeroneseVariety]);

DeclareOperation( "VeroneseMap", [IsVeroneseVariety]);

### 6. Grassmann Varieties ###
DeclareOperation( "GrassmannCoordinates", [ IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "GrassmannMap", [ IsPosInt,IsProjectiveSpace ] );
DeclareOperation( "GrassmannMap", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation( "GrassmannVariety", [IsPosInt, IsPosInt, IsField]);
DeclareOperation( "GrassmannVariety", [IsPosInt, IsPosInt, IsPosInt]);

### 7. Miscellaneous ###
DeclareOperation( "ConicOnFivePoints",  [ IsHomogeneousList and
                              IsSubspaceOfProjectiveSpaceCollection ] );
DeclareOperation( "PolarSpace", [IsProjectiveVariety] );


#############################################################################
#temporarily declarations, to be continued ...
#############################################################################




## List of dimensions and field

