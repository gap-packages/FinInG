#############################################################################
##
##  forms.gd              Desargues package
##                                                              John Bamberg
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                             Sven Reichard
##
##  Copyright 2006 University of Western Australia
##                 Lehrstuhl D fuer Mathematik
##
##  Declaration stuff for sesquilinear and quadratic forms in a polar space.
##
#############################################################################

#############################################################################
# Constructor operations:
#############################################################################

DeclareProperty( "IsSymplecticForm", IsSesquilinearForm );
DeclareProperty( "IsHyperbolicForm", IsSesquilinearForm );
DeclareProperty( "IsParabolicForm", IsSesquilinearForm );
DeclareProperty( "IsEllipticForm", IsSesquilinearForm );
DeclareProperty( "IsHermitianForm", IsSesquilinearForm);

DeclareOperation( "MatrixToSesquilinearForm", 
                    [ IsMatrix and IsFFECollColl, IsField, IsString ] );
DeclareOperation( "ChangeFormToCanonical", [IsSesquilinearForm] );
DeclareOperation( "ChangeFormToCanonicalModuloGerm", [IsSesquilinearForm] );
DeclareOperation( "MakeBlockHyperbolicPair", 
                    [IsMatrix and IsFFECollColl, IsField, IsPosInt] );
