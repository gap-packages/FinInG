#############################################################################
##
##  affinegroup.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
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
##  Implementation stuff for some new group representations
##
#############################################################################

########################################
#
# Things To Do:
#
# - testing
#
########################################


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




InstallMethod( AffineGroup, [ IsAffineSpace ],
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
    SetSize(agl, q^d * Size(GL(d,q)));
    return agl;
  end );


InstallMethod( CollineationGroup, [ IsAffineSpace ],
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
    SetSize(coll, q^d * Size(GL(d,q)) * Order(frob));
    return coll;
  end );


InstallGlobalFunction( OnAffinePoints, 
  function( y, el )
       # Note that 
       #        |     0|
       # [y, 1] |  A  0| = [yA + x, 1]
       #        |     0|
       #        |  x  1|
    local yobj, new, d, geo;
    geo := y!.geo;
    d := geo!.dimension;
    yobj := y!.obj;
    yobj := Concatenation(yobj, [ One(geo!.basefield) ]);
    new := yobj * el!.mat;
    new := new^el!.frob;
    new := new{[1..d]};
    return Wrap(geo, y!.type, new);
  end );

InstallGlobalFunction( OnAffineNotPoints,
  function( subspace, el )
     local dir, v, vec, newv, ag, mat, newdir, d, frob;
     ag := subspace!.geo;
     d := ag!.dimension;
     vec := ag!.vectorspace;
     v := subspace!.obj[1];
     dir := subspace!.obj[2];
     mat := el!.mat;
     frob := el!.frob;
     newdir := dir * mat{[1..d]}{[1..d]};
     newdir := newdir^frob;   
     newv := Concatenation(v, [ One(ag!.basefield) ]);
     newv := newv * mat;
     newv := newv^frob;
     newv := newv{[1..d]};
     newv := VectorSpaceTransversalElement(vec, newdir, newv);
     return Wrap(ag, subspace!.type, [newv, newdir]);
  end );

InstallGlobalFunction( OnAffineSubspaces,
  function( v, el )
  
  ## This is the group action of an affine group on affine subspaces that 
  ## the user should use.
  
    if v!.type = 1 then
        return OnAffinePoints(v, el);
    else
        return OnAffineNotPoints(v, el);
    fi;
  end );

InstallOtherMethod( \^, [IsSubspaceOfAffineSpace, IsProjGrpElWithFrob],
  function(x, em)
    return OnAffineSubspaces(x,em);
  end );

