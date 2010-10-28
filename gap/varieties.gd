#############################################################################
##
##  varieties.gd              FinInG package
##                                                              John Bamberg
##								Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##							      Michel Lavrauw
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


DeclareCategory( "IsAlgebraicVariety", IsAttributeStoringRep );
DeclareRepresentation("IsAlgebraicVarietyRep", IsAlgebraicVariety, ["geometry","polring", "listofpols"] );
DeclareCategory( "IsAllPointsOfAlgebraicVariety", IsDomain and IsCollection and IsComponentObjectRep );
DeclareRepresentation( "IsAllPointsOfAlgebraicVarietyRep", IsAllPointsOfAlgebraicVariety, ["variety"]);


### 1. Projective Varieties ###

DeclareCategory( "IsProjectiveVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsProjectiveVarietyRep", IsProjectiveVariety, ["geometry","polring","listofpols"]);


### 2. Affine Varieties ###

DeclareCategory( "IsAffineVariety", IsAlgebraicVariety );
DeclareRepresentation( "IsAffineVarietyRep", IsAffineVariety, ["geometry","polring","listofpols"]);

### 3. Algbraic Varieties ###

### 4. Segre Varieties ###

DeclareCategory("IsSegreVariety", IsProjectiveVariety );
DeclareRepresentation("IsSegreVarietyRep", IsSegreVariety, ["geometry","listofpols","inverseimage","segremap", "polring"]);


#############################################################################
##
##  Operations and Attributes
##
#############################################################################

### 1. Projective Varieties ###

DeclareOperation( "ProjectiveVariety", [IsProjectiveSpace, IsPolynomialRing, IsList] );
DeclareOperation( "ProjectiveVariety", [IsProjectiveSpace, IsList] );
#DeclareAttribute( "AmbientGeometry", [IsProjectiveVariety] );
#DeclareOperation( "PointsOfProjectiveVariety", [IsProjectiveVariety] );
#DeclareOperation( "Points", [IsProjectiveVariety] );

### 2. Affine Varieties ###

DeclareOperation( "AffineVariety", [IsAffineSpace, IsPolynomialRing, IsList] );
DeclareOperation( "AffineVariety", [IsAffineSpace, IsList] );
#DeclareAttribute( "AmbientGeometry", [IsAffineVariety] );
#DeclareAttribute( "DefiningListOfPolynomials", [IsAffineVariety] );
#DeclareOperation( "PointsOfAffineVariety", [IsAffineVariety] );
#DeclareOperation( "Points", [IsAffineVariety] );

### 3. Algebraic Varieties ###
#DeclareAttribute( "AmbientGeometry", [IsAlgebraicVariety] );
DeclareOperation( "AlgebraicVariety", [IsProjectiveSpace, IsList] );
DeclareOperation( "AlgebraicVariety", [IsAffineSpace, IsList] );
DeclareAttribute( "DefiningListOfPolynomials", IsAlgebraicVariety );
DeclareOperation( "PointsOfAlgebraicVariety", [IsAlgebraicVariety] );
DeclareOperation( "Points", [IsAlgebraicVariety] );

### 4. Segre Varieties ###
DeclareOperation( "SegreMap", [ IsHomogeneousList ] ); # list of projective spaces
DeclareOperation( "SegreVariety", [ IsHomogeneousList ]);  
DeclareOperation( "SegreVariety", [ IsHomogeneousList, IsField ]);  


### 5. Veronese Varities ###
DeclareOperation( "VeroneseMap", [ IsProjectiveSpace ]);
DeclareOperation( "VeroneseMap", [ IsPosInt, IsField ]);
DeclareOperation( "VeroneseMap", [ IsPosInt, IsPosInt ]);

### 6. Grassmann Varieties ###
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


DeclareOperation( "VeroneseVariety", [IsPosInt, IsField ]);
DeclareOperation( "VeroneseVariety", [IsPosInt, IsPosInt]);


## List of dimensions and field

## Just two projective spaces
DeclareOperation( "SegreVariety", [ IsProjectiveSpace, IsProjectiveSpace ]);  
DeclareOperation( "SegreVariety", [ IsPosInt, IsPosInt, IsField ]);  
