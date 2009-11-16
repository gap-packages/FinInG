###################################################################
# Just an extension of projectivespace.gi...
###################################################################

DeclareOperation( "UnderlyingVectorSpace",
                   [IsProjectiveSpace] );

InstallMethod( UnderlyingVectorSpace,
   "for a projective space",
   [IsProjectiveSpace and IsProjectiveSpaceRep],
   function(ps)
   return ps!.vectorspace;
end);
   
###################################################################
# Code for the "standard duality" of a vectorspace. We want it to 
# be an involutory mapping. To be used later on in the objects 
# representing a correlation. 
# Difficulty: make it a mapping and overload the \* operator
# (because we know that delta^-1 = delta.
###################################################################

InstallGlobalFunction( StandardDualityOfVectorSpace, function(arg)
    local map,S,f,v;
    v := arg[1];
    S := Subspaces(v);
    f := x->Subspace(v,NullspaceMat(TransposedMat(GeneratorsOfVectorSpace(x))));
    map := Objectify( TypeOfDefaultGeneralMapping( S, S,
                                   IsStandardDualityOfVectorSpace
				 and IsSPMappingByFunctionWithInverseRep
				 and IsBijective ),
				 rec (fun := f,
				      invFun := f,
				      preFun := f) );
# We looked at the code to create a MappingByFunction. Restricted the filter
# was sufficient to be able to overload \* for my new category of mappings.
    Setter(Order)(map,2);
    Setter(IsOne)(map,false);
    return map;
end );

InstallMethod( StandardDualityOfProjectiveSpace, [IsProjectiveSpace],
  function( ps )
    ## In this method, we implement the standard duality acting
    ## on projective spaces.

    local map, f;

    f := function(v)
        local ty, b, newb, rk;
        ty := v!.type;
        b := v!.obj;
        if ty = 1 then
           b := [b];
        fi;       
        newb := NullspaceMat(TransposedMat(b));
        rk := Rank(newb);
        if rk = 1 then 
           newb := newb[1];
        fi;
        return Wrap(ps, rk, newb);
      end;
    map := MappingByFunction(Varieties(ps), Varieties(ps), f, f);

    Setter(Order)(map,2);
    Setter(IsOne)(map,false);
    return map;
end );

InstallMethod( ViewObj,
   "for such a nice looking standad duality of a vectorspace",
   [IsStandardDualityOfVectorSpace and IsSPMappingByFunctionWithInverseRep],
   function(delta)
   Print("StandardDuality( ",Source(delta)," )");
end);

InstallMethod( Display,
   "for such a nice looking standad duality of a vectorspace",
   [IsStandardDualityOfVectorSpace and IsSPMappingByFunctionWithInverseRep],
   function(delta)
   Print("StandardDuality( ",Source(delta)," )");
end);

InstallMethod( PrintObj,
   "for such a nice looking standad duality of a vectorspace",
   [IsStandardDualityOfVectorSpace and IsSPMappingByFunctionWithInverseRep],
   function(delta)
   Print("StandardDuality( ",Source(delta)," )");
end);

InstallMethod( ViewObj,
   "for such a nice looking standard duality of a projective space",
   [IsStandardDualityOfProjectiveSpace and IsSPMappingByFunctionWithInverseRep],
   function(delta)
   Print("StandardDuality( ",Source(delta)," )");
end);

InstallMethod( Display,
   "for such a nice looking standard duality of a projective space",
   [IsStandardDualityOfProjectiveSpace and IsSPMappingByFunctionWithInverseRep],
   function(delta)
   Print("StandardDuality( ",Source(delta)," )");
end);

InstallMethod( PrintObj,
   "for such a nice looking standard duality of a projective space",
   [IsStandardDualityOfProjectiveSpace and IsSPMappingByFunctionWithInverseRep],
   function(delta)
   Print("StandardDuality( ",Source(delta)," )");
end);


InstallMethod( \*,
   "for 2 times a standard-duality of a projective space",
   [IsStandardDualityOfVectorSpace, IsStandardDualityOfVectorSpace],
   function(delta1,delta2)
   return One(delta1);
end);

InstallMethod( \*,
   "for 2 times a standard-duality of a projective space",
   [IsStandardDualityOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
   function(delta1,delta2)
   return One(delta1);
end);

## John has only implemented "StandardDualityOfProjectiveSpace" stuff
## up to this point.


###################################################################
# Code for "projective elements with frobenius with vector space 
# isomorphism", 
# Such an object represent a collineation (delta = identity) OR 
# a correlation (delta = standard duality).
###################################################################

###################################################################
# Basic construction methods en viewing, displaying, printing...
###################################################################
InstallMethod( ProjElWithFrobWithVSIsom, 
  "for a ffe matrix and a Frobenius automorphism, a field and the st. duality",
  [IsMatrix and IsFFECollColl,
	  IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsStandardDualityOfVectorSpace],
  function( m, frob, f, delta )
    local el;
    el := rec( mat := m, fld := f, frob := frob, vsisom := delta );
    Objectify( ProjElsWithFrobWithVSIsomType, el );
    return el;
  end );
  
InstallMethod( ProjElWithFrobWithVSIsom, 
  "for a ffe matrix and a Frobenius automorphism and a field, no 4th argument, will be identity",
  [IsMatrix and IsFFECollColl,
	  IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField],
  function( m, frob, f )
    local el,isom,q,n;
    q := Size(f); 
    n := Length(m);
    isom := IdentityMapping(Subspaces(FullRowSpace(GF(q),n)));
    el := rec( mat := m, fld := f, frob := frob, vsisom := isom);
    Objectify( ProjElsWithFrobWithVSIsomType, el );
    return el;
  end );

InstallMethod( ProjElWithFrobWithVSIsom, 
  "for a ffe matrix and a Frobenius automorphism, a field and the identity mapping",
  [IsMatrix and IsFFECollColl,
	  IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsGeneralMapping and IsSPGeneralMapping and IsOne],
  function( m, frob, f, delta )
    local el;
    el := rec( mat := m, fld := f, frob := frob, vsisom := delta );
    Objectify( ProjElsWithFrobWithVSIsomType, el );
    return el;
  end );

InstallMethod( ViewObj, "for a projective group element with Frobenius with duality",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function(el)
    Print("<projective element with Frobenius with vectorspace isomorphism");
    ViewObj(el!.mat);
    if IsOne(el!.frob) then
        Print(", F^0, ");
    else
        Print(", F^",el!.frob!.power,", ");
    fi;
    ViewObj(el!.vsisom);
    Print(" >");
  end);

InstallMethod( Display, "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function(el)
    Print("<projective element with Frobenius, underlying matrix, \n");
    Display(el!.mat);
    if IsOne(el!.frob) then
        Print(", F^0");
    else
        Print(", F^",el!.frob!.power);
    fi;
    Print(", underlying vectorspace isomorphism:\n",el!.vsisom,">\n");
  end );

InstallMethod( PrintObj, "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function(el)
    Print("ProjElWithFrobWithVSIsom(");
    PrintObj(el!.mat);
    Print(",");
    PrintObj(el!.frob);
    Print(",");
    PrintObj(el!.vsisom);
    Print(")");
  end );

InstallOtherMethod( Representative, 
  "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    return [el!.mat,el!.frob,el!.vsisom];
  end );

InstallMethod( BaseField, "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    return el!.fld;
  end );


###################################################################
# code for multiplying elements with themselves and other elements...
###################################################################

InstallMethod( \=, 
  "for two projective group els with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep,
   IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( a, b )
    local aa,bb,p,s,i;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    if a!.frob <> b!.frob then return false; fi;
    aa := a!.mat;
    bb := b!.mat;
    p := PositionNonZero(aa[1]);
    s := bb[1][p] / aa[1][p];
    for i in [1..Length(aa)] do
        if s*aa[i] <> bb[i] then return false; fi;
    od;
    if a!.vsisom <> b!.vsisom then return false; fi;
    return true;
  end );

InstallMethod( IsOne, 
  "for a projective group elm with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    local s;
    if not(IsOne(el!.frob)) then return false; fi;
    if not(IsOne(el!.vsisom)) then return false; fi;
    s := el!.mat[1][1];
    if IsZero(s) then return false; fi;
    s := s^-1;
    return IsOne( s*el!.mat );
  end );

InstallOtherMethod( OneImmutable, 
  "for a projective group elm with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    local o;
    o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
              frob := el!.frob^0, vsisom := el!.vsisom^0);
    Objectify( ProjElsWithFrobWithVSIsomType, o);
    return o;
  end );

InstallOtherMethod( OneImmutable, 
  "for a projective group with Frobenius with vectorspace isomorphism",
  [IsGroup and IsProjGrpElWithFrobWithVSIsom],
  function( g )
    local gens, o;
    gens := GeneratorsOfGroup(g);
    if Length(gens) = 0 then
        if HasParent(g) then
            gens := GeneratorsOfGroup(Parent(g));
        else
            Error("sorry, no generators, no one");
        fi;
    fi;
    o := rec( mat := OneImmutable( gens[1]!.mat ), fld := BaseField(g),
              frob := gens[1]!.frob^0, vsisom := gens[1]!.vsisom^0 );
    Objectify( ProjElsWithFrobWithVSIsomType, o);
    return o;
  end );

InstallMethod( OneSameMutability, 
  "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    local o;
    o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
              frob := el!.frob^0, vsisom := el!.vsisom^0 );
    Objectify( ProjElsWithFrobWithVSIsomType, o);
    return o;
  end );

InstallMethod( \*, 
  "for two projective group elements with frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep,
   IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( a, b )
    local el;  
    el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1)))^a!.vsisom, fld := a!.fld, 
               frob := a!.frob * b!.frob, vsisom := a!.vsisom * b!.vsisom );
    Objectify( ProjElsWithFrobWithVSIsomType, el);
    return el;
  end );

InstallMethod(\<,  [IsProjGrpElWithFrobWithVSIsom, IsProjGrpElWithFrobWithVSIsom],
  function(a,b)
    local aa,bb,pa,pb,sa,sb,i,va,vb;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    if a!.vsisom < b!.vsisom then
        return true;
    elif b!.vsisom < a!.vsisom then
        return false;
    fi;
    if a!.frob < b!.frob then
        return true;
    elif b!.frob < a!.frob then
        return false;
    fi;
    aa := a!.mat;
    bb := b!.mat;
    pa := PositionNonZero(aa[1]);
    pb := PositionNonZero(bb[1]);
    if pa > pb then 
        return true;
    elif pa < pb then
        return false;
    fi;
    sa := aa[1][pa]^-1;
    sb := bb[1][pb]^-1;
    for i in [1..Length(aa)] do
        va := sa*aa[i];
        vb := sb*bb[i];
        if va < vb then return true; fi;
        if vb < va then return false; fi;
    od;
    return false;
  end);

## Let M be a matrix, f be a Frobenius aut, and t be a vsisom.
## Then the inverse of Mft is 
##        t^-1 f^-1 M^-1 = M^(ft) f^-1 t^-1

InstallMethod( InverseSameMutability, 
  "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    local m, f, t;
    f := el!.frob^-1;
    t := el!.vsisom;
    m := rec( mat := ((InverseSameMutability(el!.mat))^f)^t, fld := el!.fld,
              frob := f, vsisom := t);
    Objectify( ProjElsWithFrobWithVSIsomType, m );
    return m;
  end );

InstallMethod( InverseMutable, 
  "for a projective group element with Frobenius with vectorspace isomorphism",
  [IsProjGrpElWithFrobWithVSIsom and IsProjGrpElWithFrobWithVSIsomRep],
  function( el )
    local m, f, t;
    f := el!.frob^-1;
    t := el!.vsisom;
    m := rec( mat := ((InverseMutable(el!.mat))^f)^t, fld := el!.fld,
              frob := f, vsisom := t );
    Objectify( ProjElsWithFrobWithVSIsomType, m );
    return m;
  end );

###################################
# Methods for \^ (standard duality)
###################################

InstallOtherMethod( \^, "for a FFE vector and a trivial st. duality",
  [ IsVector and IsFFECollection, IsMapping and IsOne ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a compressed GF2 vector and a st. duality",
  [ IsVector and IsFFECollection and IsGF2VectorRep, IsMapping and IsOne ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit vector and a trivial st. duality",
  [ IsVector and IsFFECollection and Is8BitVectorRep, IsMapping and IsOne ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, "for a FFE matrix and a st. duality",
  [ IsMatrix and IsFFECollColl, IsStandardDualityOfVectorSpace ],
  function( m, f )
    return TransposedMat(Inverse(m));
  end );

InstallOtherMethod( \^, "for a FFE matrix and a trivial st. duality",
  [ IsMatrix and IsFFECollColl, IsMapping and IsOne ],
  function( m, f )
    return m;
  end );

InstallMethod( ProjElsWithFrobWithVSIsom,
  "for a list of triples of ffe matrice + frob aut + st duality, and a field",
  [IsList, IsField],
  function( l, f )
    local objectlist, m;
    objectlist := [];
    for m in l do
        Add(objectlist, ProjElWithFrobWithVSIsom(m[1],m[2],f,m[3]));
    od;
    return objectlist;
  end );

InstallMethod( CorrelationGroup, "for a full projective space",
  [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( ps )
    local corr,d,f,frob,g,newgens,q,tau;
    f := ps!.basefield;
    q := Size(f);
    d := ProjectiveDimension(ps);
    g := GL(d+1,q);
    frob := FrobeniusAutomorphism(f);
    tau := StandardDualityOfVectorSpace(f^d);
    newgens := List(GeneratorsOfGroup(g),x->[x,frob^0,tau^0]);
    Add(newgens,[One(g),frob,tau^0]);
    Add(newgens,[One(g),frob^0,tau]);
    newgens := ProjElsWithFrobWithVSIsom(newgens, f);
    corr := GroupWithGenerators(newgens);
    SetSize(corr, Size(g) / (q - 1) * Order(frob));
    return corr;
  end );

InstallGlobalFunction( OnPointsHyperplanesWithFrobWithVSIsom,
  function( sub, el )
    local vec, c, row;
    vec := sub^el!.vsisom;
    if IsVector(sub) and not IsMatrix(sub) then
       ## sub is a point (use code from "OnLines")
       vec := (OnPoints(sub, el!.mat))^el!.frob;
       c := PositionNonZero(vec);
       if c <= Length( vec )  then
          if not(IsMutable(vec)) then
             vec := ShallowCopy(vec);
          fi;
          MultRowVector(vec, Inverse( vec[c] ));
       fi;
       return vec;
    else
       ## sub is a hyperplane (use code from "OnSubspacesByCanonicalBasis")
       vec := (sub * el!.mat)^el!.frob;
       if not IsMutable( vec )  then
          vec := MutableCopyMat( vec );
       else
          for row in [1 .. Length(vec)]  do
            if not IsMutable( vec[row] )  then
                vec[row] := ShallowCopy( vec[row] );
            fi;
          od;
       fi;
       TriangulizeMat( vec );
       return vec;
    fi;
  end );

InstallMethod( Dimension, 
  "for a projective group with Frobenius with vspace isomorphism",
  [IsProjGroupWithFrobWithVSIsom],
  function( g )
    local gens;
    if HasParent(g) then
        return Dimension(Parent(g));
    fi;
    # Now start to investigate:
    gens := GeneratorsOfGroup(g);
    if Length(gens) > 0 then
        return Length(gens[1]!.mat);
    fi;
    Error("dimension could not be determined");
  end );

InstallMethod( ActionOnPointsHyperplanes, 
  "for a projective group with Frobenius with vspace isomorphism",
  [ IsProjGroupWithFrobWithVSIsom ],
  function( pg )
    local a,d,f,o,on,orb,orb2,v,zero, m, j, flip, vs;
    f := BaseField(pg);
    d := Dimension(pg);
    vs := f^d;
    o := One(f); zero := Zero(f);
    on := One(pg);
    v := ZeroMutable(on!.mat[1]);
    v[1] := o;  
    orb := [];
    for m in f^d do
      j := PositionNot(m, zero);
      	if j <= d and m[j] = o then
	  Add(orb, m);
	fi;
    od;
    flip := StandardDualityOfVectorSpace(f^d)!.fun;
    orb2 := [];
    for m in orb do
      j := BasisVectors(Basis(flip(Subspace(vs, [m]))));
      Add(orb2, j);
    od;
    orb := Concatenation(orb, orb2);
    a := ActionHomomorphism(pg, orb,
              OnPointsHyperplanesWithFrobWithVSIsom, "surjective");
    SetIsInjective(a,true);
    return a;
  end );

#InstallMethod( SetAsNiceMono, 
#  "for a projective group with Frobenius with vspace isomorphism and an action hom",
#  [IsProjGroupWithFrobWithVSIsom, IsGroupHomomorphism and IsInjective],
#  function( pg, a )
#    SetNiceMonomorphism(pg,a);
#    SetNiceObject(pg,Image(a));
#  end );

#InstallMethod( NiceMonomorphism, 
#  "for a projective group with Frobenius with vspace isomorphism (feasible case)",
#  [IsProjGroupWithFrobWithVSIsom and CanComputeActionOnPoints and
#   IsHandledByNiceMonomorphism], 50,
#  function( pg )
#    return ActionOnPointsHyperplanes( pg );
#  end );
  
#InstallMethod( NiceMonomorphism, 
#  "for a projective group with Frobenius with vspace isomorphism (nasty case)",
#  [IsProjGroupWithFrobWithVSIsom and IsHandledByNiceMonomorphism], 50,
#  function( pg )
#    local can;
#    can := CanComputeActionOnPoints(pg);
#    if not(can) then
#        Error("action on projective points not feasible to calculate");
#    else
#        return ActionOnPointsHyperplanes( pg );
#    fi;
#  end );


