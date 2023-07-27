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
##  Copyright 2018	Colorado State University
##                  Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universiteit Gent
##					University of St. Andrews
##					University of Western Australia
##                  Vrije Universiteit Brussel
##
##
##  Implementation stuff for placeholders of orbits/stabilizer functions
##
#############################################################################
##
## 11/02/13 ml
## These methods have been tested on all projective spaces and classical polar spaces
## The champion in ALL cases is FiningStabiliserOrb ... BY FAR!!! using the ORB 
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
# CHANGED 08/04/14 jb
#############################################################################
#O  FiningOrbit( <g>, <e> )
#  returns the orbit of e under g. It is assumed that e is a subspace of a projective space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a projective space",
	[ IsProjectiveGroupWithFrob,  IsSubspaceOfProjectiveSpace ],
	function(g,e )
		return FiningOrbit(g,e,OnProjSubspaces);
	end );

# ADDED 08/04/14 jb
#############################################################################
#O  FiningOrbit( <g>, <e> )
#  returns the orbit of e under g. It is assumed that e is a subspace of a affine space
# and g a collineation group, such that OnAffineSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbit,
	"for a collineation group, an element of a affine space",
	[ IsProjectiveGroupWithFrob,  IsSubspaceOfAffineSpace ],
	function(g,e )
		return FiningOrbit(g,e,OnAffineSubspaces);
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
# CHANGED 08/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e>, <act> )
#  returns the orbits of e under g, using action function act.
##
InstallMethod( FiningOrbits,
	"for a group, a homogeneous list, and an action function",
	[ IsGroup, IsHomogeneousList, IsFunction],
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
			if InfoLevel(InfoFinInG) > 0 then
				Print(upto, "%..\c");
			fi;
		fi;
	until IsEmpty(set2);
	return orbs;
	end );

# ADDED 21/06/16 jdb
#############################################################################
#O  FiningOrFiningOrbitsDomainbits( <g>, <e>, <act> )
#  returns the orbits of e under g, using action function act. We follow the
#  analogy of GAP: Domain when <e> is closed under the action of g. If you do
#  not know whether this is true, you should use FiningOrbits.
##
InstallMethod( FiningOrbitsDomain,
    "for a group, a collection and a function",
    true,
    [ IsGroup, IsElementsOfIncidenceGeometry, IsFunction ],
    0,
    function(G,col,act)
    local blist,D,x,pos,orbs,next,orb;
    D := AsSet(col);
    blist := BlistList( [ 1 .. Length( D ) ], [  ] );
    orbs := [  ];
    for next in [1..Length(D)] do
      if blist[next]=false then
        orb := Enumerate(Orb(G, D[next], act));
        for x in orb do
            pos := Position(D,x);
            blist[pos] := true;
        od;
        Add( orbs, orb );
      fi;
    od;
    return Immutable( orbs );
    end );

# ADDED 26/03/14 ml
# CHANGED 08/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits of e under g. It is assumed that e is a subspace of a projective space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, elements of a projective space",
	[ IsProjectiveGroupWithFrob,  IsSubspaceOfProjectiveSpaceCollection and IsHomogeneousList  ],
	function(g,e )
		return FiningOrbits(g,e,OnProjSubspaces);
	end );

# ADDED 08/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits of e under g. It is assumed that e is homogeneous list of affine spaces
# and g a collineation group, such that OnAffineSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, elements of an affine space",
	[ IsProjectiveGroupWithFrob, IsSubspaceOfAffineSpaceCollection and IsHomogeneousList ],
	function(g,e )
		return FiningOrbits(g,e,OnAffineSubspaces);
	end );

# ADDED 14/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits on e under g. It is assumed that e is a collection of subspaces of a projective space
#  and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, elements-collection of a projective space",
	[ IsProjectiveGroupWithFrob,  IsSubspacesOfProjectiveSpace ],
	function(g,e )
		return FiningOrbits(g,AsList(e),OnProjSubspaces);
	end );
	
# ADDED 14/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits on e under g. It is assumed that e is a collection of subspaces of an affine space
#  and g a collineation group, such that OnAffineSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, elements-collection of an affine space",
	[ IsProjectiveGroupWithFrob,  IsSubspacesOfAffineSpace ],
	function(g,e )
		return FiningOrbits(g,AsList(e),OnAffineSubspaces);
	end );

# ADDED 14/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits on e under g. It is assumed that e is a shadow space of projective spaces
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, shadow space of a projective space",
	[ IsProjectiveGroupWithFrob, IsShadowSubspacesOfProjectiveSpace	],
	function(g,e )
		return FiningOrbits(g,AsList(e),OnProjSubspaces);
	end );

# ADDED 14/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits on e under g. It is assumed that e is a shadow space of affine spaces
# and g a collineation group, such that OnAffineSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, shadow space of an affine space",
	[ IsProjectiveGroupWithFrob, IsShadowSubspacesOfAffineSpace	],
	function(g,e )
		return FiningOrbits(g,AsList(e),OnAffineSubspaces);
	end );
	
# ADDED 14/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits on e under g. It is assumed that e is a shadow space of a polar space
# and g a collineation group, such that OnProjSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, shadow space of a polar space",
	[ IsProjectiveGroupWithFrob, IsShadowSubspacesOfClassicalPolarSpace	],
	function(g,e )
		return FiningOrbits(g,AsList(e),OnProjSubspaces);
	end );

# ADDED 14/04/14 jb
#############################################################################
#O  FiningOrbits( <g>, <e> )
#  returns the orbits on e under g. It is assumed that e is a parallel class of affine spaces
# and g a collineation group, such that OnAffineSubspaces will be the natural action to use.
##
InstallMethod( FiningOrbits,
	"for a collineation group, parallel class of an affine space",
	[ IsProjectiveGroupWithFrob, IsParallelClassOfAffineSpace	],
	function(g,e )
		return FiningOrbits(g,AsList(e),OnAffineSubspaces);
	end );


############################################################
# STABILISERS
###################################################

# # 29/03/14 CHECKED jb
#############################################################################
#O  FiningElementStabiliserOp( <g>, <e>, <act> )
# helper operation, returns the stabiliser of e under g, using action function act.
##
InstallMethod( FiningElementStabiliserOp,
	"for a collineation group, an element of an incidence structure, and an action function",
	[ IsGroup, IsElementOfIncidenceStructure, IsFunction],
	
	# JB: 13/04/14
	# Not happy with this code. Some flaws:
	# (1) the record "size" is wrong. It should be the degree.
	# (2) it assumes that the size of g is known, so it computes it. I've asked Max what best-practice should be.
	
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

# # 29/03/14 CHECKED jb
#############################################################################
#O  FiningStabiliser( <g>, <e> )
# returns the stabiliser of e under g. It is assumed that e is a subspace of an affine space
# and g a collineation group, such that OnAffineSubspaces will be the natural action to use.
# then the FiningElementStabiliserOp is called.
##
InstallMethod( FiningStabiliser,	
	"for a collineation group and a subspace of a affine space",
	[ IsProjectiveGroupWithFrob, IsSubspaceOfAffineSpace],
	function(fining_group,el)
	return FiningElementStabiliserOp(fining_group,el,OnAffineSubspaces);
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
		stab := SubgroupNC(fining_group, ORB_EstimateOrbitSize(ProductReplacer(fining_group),el,OnProjSubspaces,15,1000000,60000).Sgens);
	return stab;
end );

# # 29/03/14 CHECKED jb
#############################################################################
#O  FiningStabiliserOrb( <g>, <e> )
# returns the stabiliser of e under g.
# This uses the ORB_EstimateOrbitSize command from the ORB package, and it
# it is extremely fast. Much faster than the other methods here, in ALL cases.
# again the natural action OnAffineSubspaces is assumed.
##
InstallMethod( FiningStabiliserOrb, 
	[IsProjectiveGroupWithFrob, IsSubspaceOfAffineSpace],
	function(fining_group,el)
		local stab;
		stab := Group(ORB_EstimateOrbitSize(ProductReplacer(fining_group),el,OnAffineSubspaces,15,1000000,60000).Sgens);
	return stab;
end );

# # 26/03/14 CHECKED jb
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
		SetParent(stab, g);    # since Max forgot to put this command in his code
		return stab;
end );

# # 29/03/14 CHECKED jb
#############################################################################
#O  FiningSetwiseStabiliser( <g>, <set> )
# returns the setwise stabiliser of set under g.
# This uses SetwiseStabilizer from the orb package. The natural action OnAffineSubspaces is
# assumed.
##
InstallMethod( FiningSetwiseStabiliser,
	"for a set of elements of an affine space of a given type",
	[IsProjectiveGroupWithFrob,  IsSubspaceOfAffineSpaceCollection and IsHomogeneousList],
	function(g,set)
		local stab;
		stab := SetwiseStabilizer(g, OnAffineSubspaces, set)!.setstab;		
		SetParent(stab, g);    # since Max forgot to put this command in his code
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


# ADDED 24/05/16 ml+pc
InstallMethod( ProjectiveStabiliserGroupOfSubspace,
 "for a subspace of a projective space",
[IsSubspaceOfProjectiveSpace],
function(sub)

local t,pg,n,F,b1,V,comp,b2,bas,basechangeproj,G1,G2,N1,N2,
genlist,A,B,mat,i,I1,I2,pgenlist,newgenlist,stab,q;

t:=ProjectiveDimension(sub)+1;
pg:=AmbientSpace(sub);
n:=ProjectiveDimension(pg)+1;
F:=pg!.basefield;
b1:=Unpack(sub!.obj);
# basis change!
V:=UnderlyingVectorSpace(pg);
comp:=ComplementSpace(V,sub!.obj);
b2:=Basis(comp);
bas:=Concatenation(b1,b2);
basechangeproj:=Projectivity(pg,bas);
G1:=GL(t,F); G2:=GL(n-t,F);
N1:=NullMat(t,n-t,F); N2:=NullMat(n-t,t,F);
genlist:=[];
for A in GeneratorsOfGroup(G1) do for B in GeneratorsOfGroup(G2) do
    mat:=[];
    for i in [1..t] do Add(mat,Concatenation(A[i],N1[i]));
    od;
    for i in [1..n-t] do Add(mat,Concatenation(N2[i],B[i]));
    od;
    Add(genlist,mat);
od;od;
N2[1,1]:=One(F); I1:=IdentityMat(t,F); I2:=IdentityMat(n-t,F);
mat:=[];
for i in [1..t] do Add(mat,Concatenation(I1[i],N1[i]));
od;
for i in [1..n-t] do Add(mat,Concatenation(N2[i],I2[i]));
od;
Add(genlist,mat);
pgenlist:=List(genlist,g->Projectivity(g,F));
newgenlist:=List(pgenlist,x->x^basechangeproj);
stab:=Group(newgenlist);
SetParent(stab,ProjectivityGroup(pg));
q:=Size(F);
SetSize(stab, Size(G1)*Size(G2)*q^(t*(n-t))/(q-1));
return stab;
#return Subgroup(ProjectivityGroup(pg),newgenlist);
end );


# ADDED 25/05/16 ml+pc
InstallMethod( StabiliserGroupOfSubspace,
 "for a subspace of a projective space",
[IsSubspaceOfProjectiveSpace],
function(sub)

local t,pg,n,F,b1,V,comp,b2,bas,basechangeproj,G1,G2,N1,N2,
genlist,A,B,mat,i,I1,I2,pgenlist,newgenlist,stab,q,frob,frobgens,pow;

t:=ProjectiveDimension(sub)+1;
pg:=AmbientSpace(sub);
n:=ProjectiveDimension(pg)+1;
F:=pg!.basefield;
b1:=Unpack(sub!.obj);
# basis change!
V:=UnderlyingVectorSpace(pg);
comp:=ComplementSpace(V,sub!.obj);
b2:=Basis(comp);
bas:=Concatenation(b1,b2);
basechangeproj:=Projectivity(pg,bas);
G1:=GL(t,F); G2:=GL(n-t,F);
N1:=NullMat(t,n-t,F); N2:=NullMat(n-t,t,F);
genlist:=[];
for A in GeneratorsOfGroup(G1) do for B in GeneratorsOfGroup(G2) do
    mat:=[];
    for i in [1..t] do Add(mat,Concatenation(A[i],N1[i]));
    od;
    for i in [1..n-t] do Add(mat,Concatenation(N2[i],B[i]));
    od;
    Add(genlist,mat);
od;od;
N2[1,1]:=One(F); I1:=IdentityMat(t,F); I2:=IdentityMat(n-t,F);
mat:=[];
for i in [1..t] do Add(mat,Concatenation(I1[i],N1[i]));
od;
for i in [1..n-t] do Add(mat,Concatenation(N2[i],I2[i]));
od;
Add(genlist,mat);
# We now make collineations and add Frobenius
frob := FrobeniusAutomorphism(F);
frobgens := List(genlist,x->[x,frob^0]);
if not IsOne(frob) then
	Add(frobgens,[IdentityMat(n,F),frob]);
fi; 
pgenlist := ProjElsWithFrob(frobgens);
newgenlist:=List(pgenlist,x->x^basechangeproj);
stab:=GroupWithGenerators(newgenlist);
SetParent(stab,CollineationGroup(pg));
q:=Size(F);
pow := LogInt(q, Characteristic(F));
SetSize(stab, pow*Size(G1)*Size(G2)*q^(t*(n-t))/(q-1));
return stab;
end );

# ADDED 25/05/16 ml+pc
InstallMethod( SpecialProjectiveStabiliserGroupOfSubspace,
 "for a subspace of a projective space",
[IsSubspaceOfProjectiveSpace],
function(sub)

local t,pg,n,F,b1,V,comp,b2,bas,basechangeproj,G1,G2,N1,N2,
genlist,A,B,mat,i,I1,I2,pgenlist,newgenlist,stab,q,det;

t:=ProjectiveDimension(sub)+1;
pg:=AmbientSpace(sub);
n:=ProjectiveDimension(pg)+1;
F:=pg!.basefield;
b1:=Unpack(sub!.obj);
# basis change!
V:=UnderlyingVectorSpace(pg);
comp:=ComplementSpace(V,sub!.obj);
b2:=Basis(comp);
bas:=Concatenation(b1,b2);
basechangeproj:=Projectivity(pg,bas);
G1:=GL(t,F); G2:=GL(n-t,F);
N1:=NullMat(t,n-t,F); N2:=NullMat(n-t,t,F);
genlist:=[];
for A in GeneratorsOfGroup(G1) do for B in GeneratorsOfGroup(G2) do
    mat:=[];
    for i in [1..t] do Add(mat,Concatenation(A[i],N1[i]));
    od;
    for i in [1..n-t] do Add(mat,Concatenation(N2[i],B[i]));
    od;
# make sure we are in SL
    det:=DeterminantMat(mat)^-1;
    mat[1]:=det*mat[1]; # rescale first row
    Add(genlist,mat);
od;od;
N2[1,1]:=One(F); I1:=IdentityMat(t,F); I2:=IdentityMat(n-t,F);
mat:=[];
for i in [1..t] do Add(mat,Concatenation(I1[i],N1[i]));
od;
for i in [1..n-t] do Add(mat,Concatenation(N2[i],I2[i]));
od;
Add(genlist,mat);
pgenlist:=List(genlist,g->Projectivity(g,F));
newgenlist:=List(pgenlist,x->x^basechangeproj);
stab:=Group(newgenlist);
SetParent(stab,SpecialProjectivityGroup(pg));
q:=Size(F);
SetSize(stab, Size(G1)*Size(G2)*q^(t*(n-t))/(q-1)/Gcd(n,q-1));
return stab;
end );
