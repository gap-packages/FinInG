#############################################################################
##
##  affinespace.gd              FinInG package
##                                                              John Bamberg
## 								                                Anton Betten
##                                                             Philippe Cara
##                                                              Jan De Beule
## 							                                  Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2008 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Declaration stuff for affine spaces
##
#############################################################################




#############################################################################
#
# Categories, families, and collections:
#
#############################################################################

DeclareCategory( "IsSubspaceOfAffineSpace", IsElementOfIncidenceGeometry );
DeclareCategory( "IsSubspacesOfAffineSpace", IsElementsOfIncidenceGeometry );
DeclareCategory( "IsAllSubspacesOfAffineSpace", IsAllElementsOfIncidenceGeometry );
DeclareCategory( "IsShadowSubspacesOfAffineSpace", IsShadowElementsOfIncidenceGeometry );
DeclareCategory( "IsParallelClassOfAffineSpace", IsElementsOfIncidenceGeometry );

DeclareRepresentation( "IsSubspacesOfAffineSpaceRep", IsElementsOfIncidenceGeometry, [ "geometry", "type" ] );
DeclareRepresentation( "IsAllSubspacesOfAffineSpaceRep", IsAllElementsOfIncidenceGeometry, [ "geometry", "type" ] );
DeclareRepresentation( "IsShadowSubspacesOfAffineSpaceRep", IsElementsOfIncidenceGeometry, [ "geometry", "type", "list" ]);
DeclareRepresentation( "IsParallelClassOfAffineSpaceRep", IsElementsOfIncidenceGeometry, [ "geometry", "element" ]);

DeclareCategoryCollections("IsSubspaceOfAffineSpace");
BindGlobal( "SoASFamily", 
  NewFamily( "SoASFamily", IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace));
BindGlobal( "SoASCollFamily", CollectionsFamily(SoASFamily) );

#############################################################################
#
# Transversals of vector subspaces (as GAP objects)
#
#############################################################################

DeclareCategory( "IsVectorSpaceTransversal", IsSubspacesVectorSpace );
DeclareRepresentation( "IsVectorSpaceTransversalRep", IsVectorSpaceTransversal, [ "vectorspace", "subspace" ]);

DeclareOperation( "VectorSpaceTransversal", [IsVectorSpace, IsFFECollColl]);
DeclareOperation( "VectorSpaceTransversalElement", [IsVectorSpace, IsFFECollColl, IsVector]);
DeclareOperation( "ComplementSpace", [IsVectorSpace, IsFFECollColl]);



#############################################################################
#
# Operations and Attribute(s):
#
#############################################################################

DeclareOperation( "AffineSpace", [IsPosInt, IsField] );
DeclareOperation( "AffineSpace", [IsPosInt, IsPosInt] );
DeclareSynonym( "AG", AffineSpace ); 

DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector, IsPlistRep ] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector, Is8BitMatrixRep ] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector, IsGF2MatrixRep ] );
DeclareOperation( "RandomSubspace", [ IsAffineSpace, IsInt]);
DeclareOperation( "IsParallel", [ IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace ] );

DeclareAttribute( "AmbientSpace", IsAffineSpace );

DeclareOperation( "ParallelClass", [IsAffineSpace, IsSubspaceOfAffineSpace] );
DeclareOperation( "ParallelClass", [IsSubspaceOfAffineSpace] );
DeclareOperation( "Points", [ IsSubspaceOfAffineSpace ] );
DeclareOperation( "Lines", [ IsSubspaceOfAffineSpace ] );
DeclareOperation( "Planes", [ IsSubspaceOfAffineSpace ] );
DeclareOperation( "Solids", [ IsSubspaceOfAffineSpace ] );

DeclareOperation( "Points", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
DeclareOperation( "Lines", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
DeclareOperation( "Planes", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
DeclareOperation( "Solids", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
