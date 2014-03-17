#############################################################################
##
##  orbits-stabilizers.gi             FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2014	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for placeholders of orbits/stabilizer functions
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
##
## 
#############################################################################

Print(", orbits+stabilisers\c");


# ADDED 17/03/14 jdb
#############################################################################
#O  FiningOrbit( <g>, <e>, <act> )
# helper operation, returns the stabiliser of e under g, using action function act.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space and an action function",
	[ IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure, IsFunction],
	function(g,e,act)
		return Enumerate(Orb(g,e,act));
	end );
	
# ADDED 17/03/14 jdb
#############################################################################
#O  FiningOrbit( <g>, <e>, <act> )
# helper operation, returns the stabiliser of e under g, using action function act.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space and an action function",
	[ IsProjectiveGroupWithFrob, CategoryCollections(IsElementOfIncidenceStructure), IsFunction],
	function(g,e,act)
		return Enumerate(Orb(g,e,act));
	end );

# ADDED 17/03/14 jdb, based on John's mail.
#############################################################################
#O  FiningOrbits( <g>, <e>, <act> )
# helper operation, returns the stabiliser of e under g, using action function act.
##
InstallMethod( FiningOrbits,
	"for a collineation group, an element of a projective space and an action function",
	[ IsProjectiveGroupWithFrob, IsElementsOfIncidenceStructure, IsFunction],
	function(g,set,action)
	local orbs, set2, x, o, upto, newupto;
	orbs := [];
	set2 := ShallowCopy(set);
	set2 := Set(set2);;
	upto := 0;
	repeat
		x := set2[1];
		o := Enumerate(Orb(g, x, action));
		Add(orbs, o);
		SubtractSet(set2, AsList(o));
		newupto := Int(100 * (Size(set)-Size(set2))/Size(set));
		if newupto <> upto then
			upto:=newupto;
			Print(upto, "%..\c");
		fi;
	until IsEmpty(set2);
	return orbs;
	end );

	
# ADDED 17/03/14 jdb, based on John's mail.
#############################################################################
#O  FiningOrbits( <g>, <e>, <act> )
# helper operation, returns the stabiliser of e under g, using action function act.
##
InstallMethod( FiningOrbits,
	"for a collineation group, an element of a projective space and an action function",
	[ IsProjectiveGroupWithFrob, CategoryCollections(IsElementOfIncidenceStructure), IsFunction],
	function(g,set,action)
	local orbs, set2, x, o, upto, newupto;
	orbs := [];
	set2 := ShallowCopy(set);
	set2 := Set(set2);;
	upto := 0;
	repeat
		x := set2[1];
		o := Enumerate(Orb(g, x, action));
		Add(orbs, o);
		SubtractSet(set2, AsList(o));
		newupto := Int(100 * (Size(set)-Size(set2))/Size(set));
		if newupto <> upto then
			upto:=newupto;
			Print(upto, "%..\c");
		fi;
	until IsEmpty(set2);
	return orbs;
	end );

# CHECKED 7/03/13 jdb
#############################################################################
#O  FiningElementStabiliserOp( <g>, <e>, <act> )
# helper operation, returns the stabiliser of e under g, using action function act.
##
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
		
# CHECKED 7/03/13 jdb
#############################################################################
#O  FiningStabiliser( <g>, <e> )
# returns the stabiliser of e under g. It is assumed that e is a subspace of a projective space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
# then the FiningElementStabiliserOp is called.
##
InstallMethod( FiningStabiliser,	
	"for a collineation group and a subspace of a projective space",
	[ IsProjectiveGroupWithFrob, IsSubspaceOfProjectiveSpace],
	function(fining_group,el)
	return FiningElementStabiliserOp(fining_group,el,OnProjSubspaces);
end );


# CHECKED 7/03/13 jdb
#############################################################################
#O  FiningStabiliserOrb( <g>, <e> )
# returns the stabiliser of e under g.
# This uses the ORB_EstimateOrbitSize command from the ORB package, and it
# it is extremely fast. Much faster than the methods here, in ALL cases.
# again the natural action OnProjSubspaces is assumed.
##
InstallMethod( FiningStabiliserOrb, 
	[IsProjectiveGroupWithFrob, IsSubspaceOfProjectiveSpace],
	function(fining_group,el);
	return Group(ORB_EstimateOrbitSize(ProductReplacer(fining_group),el,OnProjSubspaces,15,1000000,60000).Sgens);
end );

# CHECKED 7/03/13 jdb
#############################################################################
#O  FiningSetwiseStabiliser( <g>, <set> )
# returns the setwise stabiliser of set under g.
# This uses SetwiseStabilizer from the orb package. The natural action OnProjSubspaces is
# assumed.
##
InstallMethod( FiningSetwiseStabiliser,
	"for a set of elements of an projective space of a given type",
	[IsProjectiveGroupWithFrob,  IsSubspaceOfProjectiveSpaceCollection and IsHomogeneousList],
	function(g,set)
		return SetwiseStabilizer(g, OnProjSubspaces, set)!.setstab;
end );



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
		stab:=Stabilizer(Image(hom),nr); 

#here was an issue. Ml originally wrote "Stabiliser", but meant "Stabilizer", 
#according to his own documentation of this method. But "Stabiliser" is defined in the 
#package AutPGrp. So if this package is not present or loaded, this gives an error.
#However, if this package was loaded, then something else was happening than intended.
#We (pc+jdb) think that this issue is understood now.

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


