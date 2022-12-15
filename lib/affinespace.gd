#############################################################################
##
##  affinespace.gd              FinInG package
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
DeclareRepresentation( "IsVectorSpaceTransversalRep", IsVectorSpaceTransversal and IsComponentObjectRep, [ "vectorspace", "subspace" ]);

DeclareOperation( "VectorSpaceTransversal", [IsVectorSpace, IsFFECollColl]);
DeclareOperation( "VectorSpaceTransversalElement", [IsVectorSpace, IsFFECollColl, IsVector]);

#############################################################################
# Flags of affine spaces 
#############################################################################
DeclareCategory( "IsFlagOfAffineSpace", IsFlagOfIncidenceGeometry );

BindGlobal( "FlagsOfAS", NewFamily( "FlagsOfASFamily", IsObject ));  

BindGlobal( "IsFlagOfASType", NewType( FlagsOfAS,
                                    IsFlagOfAffineSpace and IsFlagOfIncidenceStructureRep) );

#############################################################################
#
# Operations and Attribute(s):
#
#############################################################################

DeclareOperation( "AffineSpace", [IsPosInt, IsField] );
DeclareOperation( "AffineSpace", [IsPosInt, IsPosInt] );
DeclareSynonym( "AG", AffineSpace ); 
DeclareOperation( "Hyperplanes", [IsAffineSpace] );


DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsCVecRep] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector, IsPlistRep ] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector, Is8BitMatrixRep ] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsRowVector, IsGF2MatrixRep ] );
DeclareOperation( "AffineSubspace", [ IsAffineSpace, IsCVecRep, IsCMatRep] );

DeclareOperation( "RandomSubspace", [ IsAffineSpace, IsInt]);
DeclareOperation( "IsParallel", [ IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace ] );

DeclareOperation( "UnderlyingVectorSpace", [IsAffineSpace] );
DeclareAttribute( "Dimension", IsAffineSpace );

DeclareAttribute( "AmbientSpace", IsAffineSpace );
DeclareAttribute( "AmbientSpace", IsSubspaceOfAffineSpace );

DeclareOperation( "ParallelClass", [IsAffineSpace, IsSubspaceOfAffineSpace] );
DeclareOperation( "ParallelClass", [IsSubspaceOfAffineSpace] );

#commented out next declarations, because I put them in geometry.gd for more general filters. jdb 19/4/2011.

#DeclareOperation( "Points", [ IsSubspaceOfAffineSpace ] );
#DeclareOperation( "Lines", [ IsSubspaceOfAffineSpace ] );
#DeclareOperation( "Planes", [ IsSubspaceOfAffineSpace ] );
#DeclareOperation( "Solids", [ IsSubspaceOfAffineSpace ] );

#DeclareOperation( "Points", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
#DeclareOperation( "Lines", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
#DeclareOperation( "Planes", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
#DeclareOperation( "Solids", [ IsAffineSpace, IsSubspaceOfAffineSpace ] );
