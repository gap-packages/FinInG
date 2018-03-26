#############################################################################
##
##  polaritiesps.gd              FinInG package
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
##  Declaration stuff for polarities of projective spaces
##
#############################################################################




#############################################################################
#
# operations to create incidence preserving and reversing maps (to be placed in geometry.gd afterwards).
#
#############################################################################

#DeclareOperation( "Collineation", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );
#DeclareOperation( "Correlation", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );
#DeclareOperation( "Triality", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );

#DeclareOperation( "Polarity", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );
#DeclareOperation( "PolarityOfProjectiveSpace", [IsProjectiveSpace, IsMultiplicativeElementWithInverse] );

DeclareCategory( "IsPolarityOfProjectiveSpace", IsProjGrpElWithFrobWithPSIsomRep );
DeclareRepresentation( "IsPolarityOfProjectiveSpaceRep", IsProjGrpElWithFrobWithPSIsomRep, ["mat","fld","frob","psisom", "form"] );

#############################################################################
# polarities are equivalent with sesquilinear forms. 
# This explains the basic constructor
#############################################################################

DeclareOperation( "PolarityOfProjectiveSpaceOp", [IsForm] );
DeclareOperation( "PolarityOfProjectiveSpace", [IsForm] );
DeclareOperation( "PolarityOfProjectiveSpace", [IsMatrix,IsField and IsFinite] );
DeclareOperation( "PolarityOfProjectiveSpace", [IsMatrix,IsFrobeniusAutomorphism,IsField and IsFinite] );
DeclareOperation( "HermitianPolarityOfProjectiveSpace", [IsMatrix, IsField and IsFinite ] );

DeclareOperation( "PolarityOfProjectiveSpace", [IsClassicalPolarSpace] );

#DeclareAttribute( "IsDegeneratePolarity", IsPolarityOfProjectiveSpace ); #obsolete.
DeclareOperation( "BaseField", [ IsPolarityOfProjectiveSpace ]);
DeclareAttribute( "GramMatrix", IsPolarityOfProjectiveSpace );
DeclareAttribute( "CompanionAutomorphism", IsPolarityOfProjectiveSpace );
DeclareAttribute( "SesquilinearForm", IsPolarityOfProjectiveSpace );


#############################################################################
# operations and attributes for polarities of projective space. 
#############################################################################

DeclareProperty( "IsHermitianPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );
DeclareProperty( "IsSymplecticPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );
DeclareProperty( "IsOrthogonalPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );
DeclareProperty( "IsPseudoPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );

DeclareOperation( "IsAbsoluteElement", [ IsElementOfIncidenceStructure, IsPolarityOfProjectiveSpace ] );

DeclareOperation( "GeometryOfAbsolutePoints", [ IsPolarityOfProjectiveSpace ] );
DeclareOperation( "AbsolutePoints", [ IsPolarityOfProjectiveSpace ] );
DeclareOperation( "PolarSpace", [ IsPolarityOfProjectiveSpace ] );
