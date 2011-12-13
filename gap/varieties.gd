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
DeclareCategory( "IsAllPointsOfAlgebraicVariety", IsDomain and IsCollection and IsComponentObjectRep );
DeclareRepresentation( "IsAllPointsOfAlgebraicVarietyRep", IsAllPointsOfAlgebraicVariety, ["variety"]);


### 2. Projective Varieties ###

DeclareCategory( "IsProjectiveVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsProjectiveVarietyRep", IsProjectiveVariety, ["geometry","polring","listofpols"]);


### 3. Affine Varieties ###

DeclareCategory( "IsAffineVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsAffineVarietyRep", IsAffineVariety, ["geometry","polring","listofpols"]);


### 4. Segre Varieties ###

DeclareCategory("IsSegreVariety", IsProjectiveVariety );
DeclareRepresentation("IsSegreVarietyRep", IsSegreVariety, 
				["geometry","listofpols","inverseimage","segremap", "polring"]);


### 5. Veronese Varities ###
DeclareCategory("IsVeroneseVariety", IsProjectiveVariety );
DeclareRepresentation("IsVeroneseVarietyRep", IsVeroneseVariety, 
				["geometry","listofpols","inverseimage","veronesemap", "polring"]);


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

# Some extra functionality for two factors 
DeclareOperation( "SegreMap", [ IsProjectiveSpace, IsProjectiveSpace ]);  
DeclareOperation( "SegreMap", [ IsPosInt, IsPosInt, IsField ]);
DeclareOperation( "SegreMap", [ IsPosInt, IsPosInt, IsPosInt ]);

DeclareOperation( "SegreVariety", [ IsProjectiveSpace, IsProjectiveSpace ]);  
DeclareOperation( "SegreVariety", [ IsPosInt, IsPosInt, IsField ]);
DeclareOperation( "SegreVariety", [ IsPosInt, IsPosInt, IsPosInt ]);

DeclareOperation("PointsOfSegreVariety", [IsSegreVariety] );
DeclareOperation("Points", [IsSegreVariety] );

DeclareOperation("SegreMap", [IsSegreVariety]);


### 5. Veronese Varities ###
DeclareOperation( "VeroneseMap", [ IsProjectiveSpace ]);
DeclareOperation( "VeroneseMap", [ IsPosInt, IsField ]);
DeclareOperation( "VeroneseMap", [ IsPosInt, IsPosInt ]);

DeclareOperation( "VeroneseVariety", [IsProjectiveSpace ]);
DeclareOperation( "VeroneseVariety", [IsPosInt, IsField ]);
DeclareOperation( "VeroneseVariety", [IsPosInt, IsPosInt]);

DeclareOperation( "PointsOfVeroneseVariety", [IsVeroneseVariety]);
DeclareOperation( "Points", [IsVeroneseVariety]);

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


#############################################################################
#temporarily declarations, to be continued ...
#############################################################################




## List of dimensions and field

