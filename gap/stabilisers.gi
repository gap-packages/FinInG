#############################################################################
##
##  stabilizers.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2013	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for placeholders of stabilizer functions
##
#############################################################################
##
## 11/02/13 ml
## These methods have been tested on all projective spaces and classical polar spaces
## The champion in ALL cases is FiningStabiliserEstimate ... BY FAR!!! using the ORB 
## package command ORB_EstimateOrbitSize.
## Next fastest method is the FiningStabiliser using the Stab method from the ORB package.
## There are small cases where the FiningStabiliserPerm (using permutation representation)
## seems to perform better than the FiningStabiliser (e.g. hyperbolic in 3 dimensions, parabolic in 2 dimensions,
## and elliptic in three dimensions). The FiningStabiliserPerm2 seems to be a bit faster
## in some small dimensional hermitian cases (e.g. H(3,5^2), H(2,7^2), H(2,9^2)), but
## in all other cases the FiningStabiliser is the faster.
#############################################################################
InstallMethod( FiningElementStabiliserOp,
	"for a collineation group, an element of a projective space and an action function",
	[ IsGroup, IsSubspaceOfProjectiveSpace, IsFunction],
	function(g,e,act)
		local t,size, stab;
		t := e!.type;
		size := Size(ElementsOfIncidenceStructure(e!.geo,e!.type)); #strongly using here that we know the representation well...
		stab := Stab(g,e,act,rec( Size:=Size(g), DoEstimate := size )).stab;
		return stab;
	end );
		
InstallMethod( FiningStabiliser,	[IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure],
# this uses the FiningElementStabiliserOp function
		function(fining_group,el)
		local stab;
		stab:=FiningElementStabiliserOp(fining_group,el,OnProjSubspaces);
		return stab;
end );

InstallMethod( FiningStabiliserEstimate, 
	[IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure],
	# This uses the ORB_EstimateOrbitSize command from the ORB package, and it
	# it is extremely fast. Much faster than the methods here, in ALL cases.
	function(fining_group,el);
	return Group(ORB_EstimateOrbitSize(ProductReplacer(fining_group),el,OnProjSubspaces,15,1000000,60000).Sgens);
end );

#ORB_EstimateOrbitSize(ProductReplacer(g),x,OnProjSubspaces,15,1000000,60000);
		
#############################
# Stabiliser methods using the permutation representation of a group action
################################


InstallMethod( FiningStabiliserPerm, [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure],
	# this uses the ActionHomomorphism and the Stabiliser method in standard gap
		function(fining_group,el)
		local type,geo,hom,enum,nr,stab,gens,x;
		type:=el!.type;
		geo:=el!.geo;
		hom:=ActionHomomorphism(fining_group,ElementsOfIncidenceStructure(geo,type),OnProjSubspaces);
		enum:=HomeEnumerator(UnderlyingExternalSet(hom));;
		nr:=Position(enum,el);
		stab:=Stabiliser(Image(hom),nr);
		gens:=GeneratorsOfGroup(stab);;
		gens:=List(gens,x->PreImagesRepresentative(hom,x));
		stab:=GroupWithGenerators(gens);
		return stab;
end );

InstallMethod( FiningStabiliserPerm2, [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure],
	# this uses the Stab method from the genss package AND the ActionHomomorphism
		function(fining_group,el)
		local type,geo,hom,enum,nr,size,stab,gens,x,im;
		type:=el!.type;
		geo:=el!.geo;
		hom:=ActionHomomorphism(fining_group,ElementsOfIncidenceStructure(geo,type),OnProjSubspaces);
		enum:=HomeEnumerator(UnderlyingExternalSet(hom));;
		nr:=Position(enum,el);
		size:=Size(ElementsOfIncidenceStructure(geo,type));
		im:=Image(hom);
		SetSize(im,Size(fining_group));
		stab:=Stab(im,nr,OnPoints,rec( DoEstimate := size )).stab;
		#stab:=Stabiliser(Image(hom),nr);
		gens:=GeneratorsOfGroup(stab);;
		gens:=List(gens,x->PreImagesRepresentative(hom,x));
		stab:=GroupWithGenerators(gens);
		return stab;
end );


