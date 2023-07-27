#############################################################################
##
##  affinegroup.gi              FinInG package
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
##  Implementation stuff for affine groups
##
#############################################################################

# **** The way it works ****
#
# An affine transformation of the form x+A
# can be written as a matrix...
#   |     0|
#   |  A  0|
#   |     0|
#   |  x  1|
# where the matrix A occurs in all but the last row and column of the matrix,
# x sits in all but the last element of the last row, and the rest is filled
# with 0's and a 1 in the last spot. We can then just use ProjElWithFrob
# wrapping in order to incoorporate the Frobenius map.

# CHECKED 25/3/14 jdb
#############################################################################
#O  AffineGroup( <as> )
# returns AGL(d,q)  
##
InstallMethod( AffineGroup, 
	"for an affine space",
	[ IsAffineSpace ],
	function( as )
  
  ## This operation returns the group commonly known as AGL(d,F)
  
    local d, gf, vec, semi, gens, frob, newgens, agl, q;
    d := as!.dimension;
    gf := as!.basefield;
    q := Size(gf);
    vec := as!.vectorspace;
    semi := SemidirectProduct(GL(d, gf), vec);
    gens := GeneratorsOfGroup(semi);
    frob := FrobeniusAutomorphism( gf );
    newgens := List(gens, x -> [x, frob^0]);
    newgens := ProjElsWithFrob(newgens);
    agl := GroupWithGenerators(newgens);
    SetName(agl, Concatenation("AGL(",String(d),",",String(q),")") );
    SetSize(agl, Size(semi));
    return agl;
  end );

# CHECKED 27/3/2012 jdb
#############################################################################
#O  CollineationGroup( <as> )
# returns AGammaL(d,q)  
##
InstallMethod( CollineationGroup, 
	"for an affine space",
	[ IsAffineSpace ],
	function( as )
  
    ## This operation returns the group commonly known as AGammaL(d,F)
  
    local d, gf, vec, semi, gens, frob, newgens, coll, q;
    d := as!.dimension;
    gf := as!.basefield;
    q := Size(gf);
    vec := as!.vectorspace;
    semi := SemidirectProduct(GL(d, gf), vec);
    gens := GeneratorsOfGroup(semi);
    frob := FrobeniusAutomorphism( gf );
    newgens := List(gens, x -> [x, frob^0]);
    Add(newgens, [One(semi), frob]);
    newgens := ProjElsWithFrob(newgens);
    coll := GroupWithGenerators(newgens);
    if LogInt(q, Characteristic(gf)) > 1 then 
       SetName( coll, Concatenation("AGammaL(",String(d),",",String(q),")") );
    else
       SetName( coll, Concatenation("AGL(",String(d),",",String(q),")") );
    fi;
    SetSize(coll, Size(semi) * Order(frob));
    return coll;
  end );

# CHECKED 27/3/2012 jdb
# cvec changed 25/3/14.
#############################################################################
#F  OnAffinePoints( <y>, <el> )
# implements action of projective semilinear elements on affine points.
# <y> is subspace
# <el> is group element.
##
InstallGlobalFunction( OnAffinePoints, 
  function( y, el )
       # Note that 
       #        |     0|
       # [y, 1] |  A  0| = [yA + x, 1]
       #        |     0|
       #        |  x  1|
    local yobj, new, d, geo, bf;
	geo := y!.geo;
    bf := geo!.basefield;
    d := geo!.dimension;
    yobj := Unpack(y!.obj);
    yobj := CVec(Concatenation(yobj, [ One(bf) ]),bf); #Concatenation will make this a real lis, so CVec is applicable.
    new := yobj * el!.mat;
    new := new^el!.frob;
    new := new{[1..d]};
    return Wrap(geo, y!.type, new);
  end );

# CHECKED 27/3/2012 jdb
# cvec/cmat change 25/3/14.
# unpacking three times, and using AffineSubspace seems to be the easiest solution.
#############################################################################
#F  OnAffineNotPoints( <subspace>, <el> )
# implements action of projective semilinear elements on affine subspaces different
# from points.
##
# CHANGED 12/03/12 jdb
# new directions at infinity should be normalized, otherwise you get the orbits q-1 times.
##
InstallGlobalFunction( OnAffineNotPoints,
  function( subspace, el )
     local dir, v, vec, newv, ag, mat, newdir, d, frob, bf;
     ag := subspace!.geo;
     d := ag!.dimension;
     vec := ag!.vectorspace;
     v := Unpack(subspace!.obj[1]);
     dir := Unpack(subspace!.obj[2]);
     mat := Unpack(el!.mat);
     frob := el!.frob;
     newdir := dir * mat{[1..d]}{[1..d]};
     newdir := newdir^frob;
	 TriangulizeMat(newdir);   
     newv := Concatenation(v, [ One(ag!.basefield) ]); #will be a plain list.
     newv := newv * mat;
     newv := newv^frob;
     newv := newv{[1..d]};
     newv := VectorSpaceTransversalElement(vec, newdir, newv);
     return AffineSubspace(ag,newv,newdir);
	 #return Wrap(ag, subspace!.type, [newv, newdir]);
  end );

# CHECKED 27/3/2012 jdb
#############################################################################
#F  OnAffineSubspaces( <y>, <el> )
# This is the group action of an affine group on affine subspaces that 
# the user should use.
##
InstallGlobalFunction( OnAffineSubspaces,
  function( v, el )
    if v!.type = 1 then
        return OnAffinePoints(v, el);
    else
        return OnAffineNotPoints(v, el);
    fi;
  end );

# CHECKED 27/3/2012 jdb
#############################################################################
#F  \^( <x>, <em> )
# shorcut to OnAffineSubspaces.
##
InstallOtherMethod( \^,
	"for a subspace of an affine space and a projective element with frob",
	[IsSubspaceOfAffineSpace, IsProjGrpElWithFrob],
	function(x, em)
		return OnAffineSubspaces(x,em);
	end );

