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




############################################################
# ORBITS
###################################################


# 26/03/14 CHECKED ml
#############################################################################
#O  FiningOrbit( <g>, <e>, <act> )
#  returns the orbit of e under g, using action function act.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space and an action function",
	[ IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure, IsFunction],
	function(g,e,act)
		return Enumerate(Orb(g,e,act));
	end );

# ADDED 26/03/14 ml
#############################################################################
#O  FiningOrbit( <g>, <e> )
#  returns the orbit of e under g. It is assumed that e is a subspace of a projective space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space",
	[ IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure ],
	function(g,e )
		return FiningOrbit(g,e,OnProjSubspaces);
	end );


# 26/03/14 CHECKED ml
#############################################################################
#O  FiningOrbit( <g>, <e>, <act> )
#  returns the orbit of e under g, using action function act.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space and an action function",
	[ IsProjectiveGroupWithFrob, CategoryCollections(IsElementOfIncidenceStructure), IsFunction],
	function(g,e,act)
		return Enumerate(Orb(g,e,act));
	end );


# ADDED 26/03/14 ml
#############################################################################
#O  FiningOrbit( <g>, <e> )
#  returns the orbit of e under g. It is assumed that e is a subspace of a projective space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space",
	[ IsProjectiveGroupWithFrob, CategoryCollections(IsElementOfIncidenceStructure) ],
	function(g,e )
		return FiningOrbit(g,e,OnProjSubspaces);
	end );


# CHECKED 26/03/14 ml
#############################################################################
#O  FiningOrbits( <g>, <e>, <act> )
#  returns the orbits of e under g, using action function act.
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

# ADDED 26/03/14 ml
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits of e under g. It is assumed that e is a subspace of a projective space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, an element of a projective space",
	[ IsProjectiveGroupWithFrob, IsElementsOfIncidenceStructure ],
	function(g,e )
		return FiningOrbits(g,e,OnProjSubspaces);
	end );





############################################################
# STABILISERS
###################################################

# # 26/03/14 CHECKED ml
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
		
# # 26/03/14 CHECKED ml
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


# # 26/03/14 CHECKED ml
#############################################################################
#O  FiningStabiliserOrb( <g>, <e> )
# returns the stabiliser of e under g.
# This uses the ORB_EstimateOrbitSize command from the ORB package, and it
# it is extremely fast. Much faster than the other methods here, in ALL cases.
# again the natural action OnProjSubspaces is assumed.
##
InstallMethod( FiningStabiliserOrb, 
	[IsProjectiveGroupWithFrob, IsSubspaceOfProjectiveSpace],
	function(fining_group,el)
		local stab;
		stab := Group(ORB_EstimateOrbitSize(ProductReplacer(fining_group),el,OnProjSubspaces,15,1000000,60000).Sgens);
	return stab;
end );

# # 26/03/14 CHECKED ml
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
		local stab;
		stab := SetwiseStabilizer(g, OnProjSubspaces, set)!.setstab;
		return stab;
end );



#############################
# Stabiliser methods using the permutation representation of a group action
################################


# 27/03/14 CHECKED ml
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
		gens:=GeneratorsOfGroup(stab);;
		gens:=List(gens,x->PreImagesRepresentative(hom,x));
		stab:=GroupWithGenerators(gens);
		return stab;
end );

# 27/03/14 CHECKED ml
InstallMethod( FiningStabiliserPerm2, 
	[IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure],
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


# 27/03/14 CHECKED ml
InstallMethod( FixedSubspaces,
	"for a projectivity and a projective space",
	[IsProjectiveGroupWithFrob, IsProjectiveSpace],
	# fixed subspaces by John.
	function(g,pg)
	local gens, md, fixed, flag;
	gens := GeneratorsOfGroup(g);
	flag := ForAll(gens, t -> IsOne(t!.frob));
	if not flag then
		Error("Group contains field automorphisms.");
	fi;
	md := GModuleByMats(List(gens,t->Unpack(t!.mat)), pg!.basefield);
	fixed := Filtered(MTX.BasesSubmodules(md),t->not Size(t) in [0,Rank(pg)+1]);
	return List(fixed,t->VectorSpaceToElement(pg,t));
end );

