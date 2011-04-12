#############################################################################
##
##  group2.gi              FinInG package
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
##  Copyright 2011 University of Western Australia, Perth
##                 University of St. Andrews
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##                 Università degli Studi di Padova
##
##  Implementation stuff for correlation groups
##
#############################################################################



########################################
#
# Things To Do:
#
# - test CorrelationGroup
# - Documentation
# - move "UnderlyingVectorSpace" to projspace.g*
#
########################################



###################################################################
# Just an extension of projectivespace.gi...
###################################################################
   
InstallMethod( \=,
  "for set of all subspaces of a projective space",
  [ IsAllSubspacesOfProjectiveSpace, IsAllSubspacesOfProjectiveSpace ],
  function(x,y)
  return ((x!.geometry!.dimension = y!.geometry!.dimension) and (x!.geometry!.basefield =
  y!.geometry!.basefield));
end );

###################################################################
# Code for the "standard duality" of a projective space. We want it to 
# be an involutory mapping. To be used later on in the objects 
# representing a correlation. 
# Difficulty: make it a mapping and overload the \* operator
# (because we know that delta^-1 = delta.
###################################################################

#added this 16/12/08. jdb
InstallMethod( IdentityMappingOfElementsOfProjectiveSpace, 
   "for a projective space",
   [IsProjectiveSpace],
   function(ps)
   local map;
   map := IdentityMapping(ElementsOfIncidenceStructure(ps));
   SetFilterObj(map,IsIdentityMappingOfElementsOfProjectiveSpace);
   return map;
end );

InstallMethod( StandardDualityOfProjectiveSpace, [IsProjectiveSpace],
  function( ps )
    ## In this method, we implement the standard duality acting
    ## on projective spaces.

    local map, f, S, ty, obj, one;

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
        return VectorSpaceToElement(ps, newb);  ## check that this normalises the element
      end;
    S := ElementsOfIncidenceStructure(ps);   ##    map := MappingByFunction(ElementsOfIncidenceStructure(ps), ElementsOfIncidenceStructure(ps), f, f);
    ty := TypeOfDefaultGeneralMapping( S, S,
                                   IsStandardDualityOfProjectiveSpace
				 and IsSPMappingByFunctionWithInverseRep
				 and IsBijective and HasOrder and HasIsOne);
   
    # We looked at the code to create a MappingByFunction. Restricted the filter
    # was sufficient to be able to overload \* for my new category of mappings.
    obj := rec( fun := f, invFun := f, preFun := f, ps := ps );
    one := IdentityMappingOfElementsOfProjectiveSpace(ps);
    ObjectifyWithAttributes(obj, ty, Order, 2, IsOne, false, One, one); #this command I learned in st andrews!
    Setter(InverseAttr)(obj,obj); #cannot set this attribute in previous line, because map does not exist then
    return obj;
end );

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
   "for multiplying a standard-duality of a projective space",
   [IsStandardDualityOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
   function(delta1,delta2)
   return One(delta1);
end);

InstallMethod( \^,
    "for psisom and zero",
    [ IsProjectiveSpaceIsomorphism, IsZeroCyc ],
    function(delta,p)
    return One(delta);
end);

InstallMethod( \=,
   "for comparing two standard-dualities of a projective space",
   [IsStandardDualityOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
   function(delta,eta)
   return Source(delta) = Source(eta);
end);

InstallMethod( \=,
   "for comparing a standard-duality of a projective space and a identitymap",
   [IsStandardDualityOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace],
   function(delta,eta)
   return false;
end);

InstallMethod( \=,
   "for comparing a identity map and a standard-duality of a projective space",
   [IsIdentityMappingOfElementsOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
   function(delta,eta)
   return false;
end);

InstallMethod( \=,
   "for comparing a identity map and a standard-duality of a projective space",
   [IsIdentityMappingOfElementsOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace],
   function(delta,eta)
   return Source(delta) = Source(eta);
end);

###################################################################
# Code for "projective elements with frobenius with vector space 
# isomorphism", 
# Such an object represent a collineation (delta = identity) OR 
# a correlation (delta = standard duality).
###################################################################

###################################################################
# Basic construction methods en viewing, displaying, printing...
###################################################################

InstallMethod( ProjElWithFrobWithPSIsom, 
  "for a ffe matrix and a Frobenius automorphism, a field and the st. duality",
  [IsMatrix and IsFFECollColl,
	  IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsStandardDualityOfProjectiveSpace],
  function( m, frob, f, delta )
    local el;
    el := rec( mat := m, fld := f, frob := frob, psisom := delta );
    Objectify( ProjElsWithFrobWithPSIsomType, el );
    return el;
  end );
  
InstallMethod( ProjElWithFrobWithPSIsom, 
  "for a ffe matrix and a Frobenius automorphism and a field, no 4th argument, will be identity",
  [IsMatrix and IsFFECollColl,
	  IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField],
  function( m, frob, f )
    local el,isom,q,n;
    q := Size(f); 
    n := Length(m);
    isom := IdentityMappingOfElementsOfProjectiveSpace(ProjectiveSpace(n,f));  ## I hope this works!
    el := rec( mat := m, fld := f, frob := frob, psisom := isom);
    Objectify( ProjElsWithFrobWithPSIsomType, el );
    return el;
  end );

InstallMethod( ProjElWithFrobWithPSIsom, 
  "for a ffe matrix and a Frobenius automorphism, a field and the identity mapping",
  [IsMatrix and IsFFECollColl,
	  IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsGeneralMapping and IsSPGeneralMapping and IsOne],
  function( m, frob, f, delta )
    local el;
    el := rec( mat := m, fld := f, frob := frob, psisom := delta );
    Objectify( ProjElsWithFrobWithPSIsomType, el );
    return el;
  end );

InstallMethod( ViewObj, "for a projective group element with Frobenius with duality",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function(el)
    Print("<projective element with Frobenius with projectivespace isomorphism");
    ViewObj(el!.mat);
    if IsOne(el!.frob) then
        Print(", F^0, ");
    else
        Print(", F^",el!.frob!.power,", ");
    fi;
    ViewObj(el!.psisom);
    Print(" >");
  end);

InstallMethod( Display, "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function(el)
    Print("<projective element with Frobenius, underlying matrix, \n");
    Display(el!.mat);
    if IsOne(el!.frob) then
        Print(", F^0");
    else
        Print(", F^",el!.frob!.power);
    fi;
    Print(", underlying projective space isomorphism:\n",el!.psisom,">\n");
  end );

InstallMethod( PrintObj, "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function(el)
    Print("ProjElWithFrobWithPSIsom(");
    PrintObj(el!.mat);
    Print(",");
    PrintObj(el!.frob);
    Print(",");
    PrintObj(el!.psisom);
    Print(")");
  end );

InstallOtherMethod( Representative, 
  "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    return [el!.mat,el!.frob,el!.psisom];
  end );

InstallMethod( BaseField, "for a projective group element with Frobenius with proj space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    return el!.fld;
  end );

InstallMethod( BaseField, "for a projective group with Frobenius with proj space isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
  function( g )
    local f,gens;
    if IsBound(g!.basefield) then
        return g!.basefield;
    fi;
    if HasParent(g) then
        f := BaseField(Parent(g));
        g!.basefield := f;
        return f;
    fi;
    # Now start to investigate:
    gens := GeneratorsOfGroup(g);
    if Length(gens) > 0 then
        g!.basefield := gens[1]!.fld;
        return g!.basefield;
    fi;
    # Now we have to give up:
    Error("base field could not be determined");
  end );


###################################################################
# code for multiplying,comparing... elements with themselves and 
# other elements...
###################################################################

InstallMethod( \=, 
  "for two projective group els with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
   IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
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
    if a!.psisom <> b!.psisom then return false; fi;
    return true;
  end );

InstallMethod( IsOne, 
  "for a projective group elm with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local s;
    if not(IsOne(el!.frob)) then return false; fi;
    if not(IsOne(el!.psisom)) then return false; fi;
    s := el!.mat[1][1];
    if IsZero(s) then return false; fi;
    s := s^-1;
    return IsOne( s*el!.mat );
  end );

InstallOtherMethod( OneImmutable, 
  "for a projective group elm with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local o;
    o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
              frob := el!.frob^0, psisom := el!.psisom^0);
    Objectify( ProjElsWithFrobWithPSIsomType, o);
    return o;
  end );

InstallOtherMethod( OneImmutable, 
  "for a projective group with Frobenius with projective space isomorphism",
  [IsGroup and IsProjGrpElWithFrobWithPSIsom],
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
              frob := gens[1]!.frob^0, psisom := gens[1]!.psisom^0 );
    Objectify( ProjElsWithFrobWithPSIsomType, o);
    return o;
  end );

InstallMethod( OneSameMutability, 
  "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local o;
    o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
              frob := el!.frob^0, psisom := el!.psisom^0 );
    Objectify( ProjElsWithFrobWithPSIsomType, o);
    return o;
  end );

InstallMethod( \*, 
  "for two projective group elements with frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
   IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( a, b )
    local el;  
    el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1)))^a!.psisom, fld := a!.fld, 
               frob := a!.frob * b!.frob, psisom := a!.psisom * b!.psisom );
    Objectify( ProjElsWithFrobWithPSIsomType, el);
    return el;
  end );

InstallMethod(\<,  [IsProjGrpElWithFrobWithPSIsom, IsProjGrpElWithFrobWithPSIsom],
  function(a,b)
    local aa,bb,pa,pb,sa,sb,i,va,vb;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    if a!.psisom < b!.psisom then
        return true;
    elif b!.psisom < a!.psisom then
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

## Let M be a matrix, f be a Frobenius aut, and t be a psisom.
## Then the inverse of Mft is 
##        t^-1 f^-1 M^-1 = M^(ft) f^-1 t^-1

InstallMethod( InverseSameMutability, 
  "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local m, f, t;
    f := el!.frob^-1;
    t := el!.psisom;
    m := rec( mat := ((InverseSameMutability(el!.mat))^f)^t, fld := el!.fld,
              frob := f, psisom := t);
    Objectify( ProjElsWithFrobWithPSIsomType, m );
    return m;
  end );

InstallMethod( InverseMutable, 
  "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local m, f, t;
    f := el!.frob;
    t := el!.psisom;
    m := rec( mat := ((InverseMutable(el!.mat))^f)^t, fld := el!.fld,
              frob := f^-1, psisom := t );
    Objectify( ProjElsWithFrobWithPSIsomType, m );
    return m;
  end );

#############################################################################################
# Methods for \* to multiply IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsom
#############################################################################################

InstallMethod( \*, 
  "for two projective group elements with frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
   IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( a, b )
    local el;  
    el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1)))^a!.psisom, fld := a!.fld, 
               frob := a!.frob * b!.frob, psisom := a!.psisom * b!.psisom );
    Objectify( ProjElsWithFrobWithPSIsomType, el);
    return el;
  end );

InstallMethod( \*, 
  "for two projective group elements with frobenius with projective space isomorphism",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep,
   IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( a, b )
    local el;  
    el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1))), fld := a!.fld, 
               frob := a!.frob * b!.frob, psisom := b!.psisom );
    Objectify( ProjElsWithFrobWithPSIsomType, el);
    return el;
  end );

InstallMethod( \*, 
  "for two projective group elements with frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
   IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( a, b )
    local el;  
    el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1)))^a!.psisom, fld := a!.fld, 
               frob := a!.frob * b!.frob, psisom := a!.psisom  );
    Objectify( ProjElsWithFrobWithPSIsomType, el);
    return el;
  end );

###################################
# Methods for \^ (standard duality)
###################################

InstallOtherMethod( \^, "for a FFE vector and a id. mapping of el. of ps.",
  [ IsVector and IsFFECollection, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a compressed GF2 vector and a id. mapping of el. of ps.",
  [ IsVector and IsFFECollection and IsGF2VectorRep, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit vector and a id. mapping of el. of ps.",
  [ IsVector and IsFFECollection and Is8BitVectorRep, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, "for a FFE matrix and a st. duality",
  [ IsMatrix and IsFFECollColl, IsStandardDualityOfProjectiveSpace ],
  function( m, f )
    return TransposedMat(Inverse(m));
  end );

InstallOtherMethod( \^, "for a FFE matrix and a id. mapping of el. of ps.",
  [ IsMatrix and IsFFECollColl, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( m, f )
    return m;
  end );

InstallOtherMethod( \^, "for an element of a projective space and a id. mapping of el. of ps.",
  [ IsSubspaceOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( p, f)
    return p;
  end );

InstallOtherMethod( \^, "for an elements of a projective space and a st. duality",
  [ IsSubspaceOfProjectiveSpace, IsStandardDualityOfProjectiveSpace ],
  function( p, f)
    return f!.fun(p);
  end );

#####################################################################
# Methods to construct a collection of ProjElsWithFrobWithPSIsom
#####################################################################

InstallMethod( ProjElsWithFrobWithPSIsom,
  "for a list of triples of ffe matrice + frob aut + st duality, and a field",
  [IsList, IsField],
  function( l, f )
    local objectlist, m;
    objectlist := [];
    for m in l do
        Add(objectlist, ProjElWithFrobWithPSIsom(m[1],m[2],f,m[3]));
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
    tau := StandardDualityOfProjectiveSpace(ps);
    newgens := List(GeneratorsOfGroup(g),x->[x,frob^0,tau^0]);
    Add(newgens,[One(g),frob,tau^0]);
    Add(newgens,[One(g),frob^0,tau]);
    newgens := ProjElsWithFrobWithPSIsom(newgens, f);
    corr := GroupWithGenerators(newgens);
    SetSize(corr, Size(g) / (q - 1) * Order(frob) * 2); #* 2 for the standard duality.
    return corr;
  end );

#####################################################################
# User friendly Methods to construct a correlation of a projective space.
#####################################################################

#This method will construct a funny object just using a matrix and a field.
#the two mappings are the identity mapping.
InstallMethod( CorrelationOfProjectiveSpace, 
  [ IsMatrix and IsFFECollColl, IsField],
  function( mat, gf )
    if Rank(mat) <> Size(mat) then
      Error("<mat> must not be singular");
    fi;
    return ProjElWithFrobWithPSIsom( mat, IdentityMapping(gf), gf);
  end );

#same as previous, but frob is given by user now.
InstallMethod( CorrelationOfProjectiveSpace,  
  [ IsMatrix and IsFFECollColl, IsRingHomomorphism and
    IsMultiplicativeElementWithInverse, IsField], 
  function( mat, frob, gf )
    if Rank(mat) <> Size(mat) then
      Error("<mat> must not be singular");
    fi;
    return ProjElWithFrobWithPSIsom( mat, frob, gf);
  end );

#same as previous, but delta is given by user now.
InstallMethod( CorrelationOfProjectiveSpace,  
  [ IsMatrix and IsFFECollColl, IsField, IsStandardDualityOfProjectiveSpace], 
  function( mat, gf, delta )
    if Rank(mat) <> Size(mat) then
      Error("<mat> must not be singular");
    fi;
    if delta!.ps!.basefield <> gf then
      Error("<delta> is not a duality of the correct projective space");
    fi;
    return ProjElWithFrobWithPSIsom( mat, IdentityMapping(gf), gf, delta);
  end );

#same as previous, but frob and delta is given by user now.
InstallMethod( CorrelationOfProjectiveSpace,  
  [ IsMatrix and IsFFECollColl, IsRingHomomorphism and
    IsMultiplicativeElementWithInverse, IsField, 
    IsStandardDualityOfProjectiveSpace], 
  function( mat, frob, gf, delta )
    if Rank(mat) <> Size(mat) then
      Error("<mat> must not be singular");
    fi;
    if delta!.ps!.basefield <> gf then
      Error("<delta> is not a duality of the correct projective space");
    fi;
    return ProjElWithFrobWithPSIsom( mat, frob, gf, delta);
  end ); 

InstallMethod( UnderlyingMatrix, [ IsProjGrpElWithFrobWithPSIsom and 
                                   IsProjGrpElWithFrobWithPSIsomRep],
  c -> c!.mat );
  
InstallMethod( FieldAutomorphism, [ IsProjGrpElWithFrobWithPSIsom and 
                                    IsProjGrpElWithFrobWithPSIsomRep],
  c -> c!.frob );

InstallMethod( ProjectiveSpaceIsomorphism, [ IsProjGrpElWithFrobWithPSIsom and 
                                    IsProjGrpElWithFrobWithPSIsomRep],
  c -> c!.psisom );

#####################################################################
# Embedding from a collineation group to a correlation group.
#####################################################################
InstallOtherMethod( Embedding,
    "for a collineation group",
	[IsProjectiveGroupWithFrob, IsProjGroupWithFrobWithPSIsom],
	function(group,corr)
	return GroupHomomorphismByFunction(group,corr,x->ProjElWithFrobWithPSIsom(x!.mat,x!.frob,x!.fld));
end );

#####################################################################
# Actions
# We follow the same approach as in goup.gi, i.e. first define the 
# action of the funnay algebraic elements on vectorspaces
# then creating on action that does the general stuff.
# We will use the fact the st. duality is able to act on subspaces of a 
# projective space already.
# 
#####################################################################

InstallGlobalFunction( OnProjPointWithFrobWithPSIsom,
  function( line, el )
    local vec,c;
    vec := OnPoints(line,el!.mat)^el!.frob;
    c := PositionNonZero(vec);
    if c <= Length( vec )  then
        if not(IsMutable(vec)) then
            vec := ShallowCopy(vec);
        fi;
        MultRowVector(vec,Inverse( vec[c] ));
    fi;
    return vec;
  end );

InstallGlobalFunction( OnProjSubspacesWithFrobWithPSIsom,
  function( line, el )
    local vec,c;
    vec := OnRight(line,el!.mat)^el!.frob;
    if not(IsMutable(vec)) then
        vec := MutableCopyMat(vec);
    fi;
    TriangulizeMat(vec);
    return vec;
  end );

InstallGlobalFunction( OnProjSubspacesReversing,
  function( sub, el )
    local vec,newsub,ps,ty,newvec,rk;
    vec := Unwrap(sub);
    ps := AmbientSpace(sub!.geo);
    ty := sub!.type;
    newvec := (vec*el!.mat)^el!.frob;
    if ty = 1 then
      newvec := [newvec];
    fi;       
    if not IsOne(el!.psisom) then
      newvec := NullspaceMat(TransposedMat(newvec));
    fi;
    rk := Rank(newvec);
    if rk = 1 then 
      newvec := newvec[1];
    fi;
    return VectorSpaceToElement(ps,newvec);
end );

InstallMethod( Dimension, 
  "for a projective group with Frobenius with vspace isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
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
  [ IsProjGroupWithFrobWithPSIsom ],
  function( pg )
    local a,d,f,o,orb,orb2,zero, m, j, flip, vs, ps;
    f := BaseField(pg);
    d := Dimension(pg);
    ps := ProjectiveSpace(d-1,f);
    vs := f^d;
    o := One(f); zero := Zero(f);
    orb := [];
    for m in vs do
      j := PositionNot(m, zero);
      	if j <= d and m[j] = o then
	  Add(orb, m);
	fi;
    od;
    flip := function(j)
       local vec;
       vec := TriangulizedNullspaceMat(TransposedMat([j]));
       return vec;
    end;
    orb2 := [];
    for m in orb do
        Add(orb2, flip(m));
    od;
    orb := List(Concatenation(orb,orb2),x->VectorSpaceToElement(ps,x)); #corrected 12/4/11
    a := ActionHomomorphism(pg, orb,
              OnProjSubspacesReversing, "surjective");
    SetIsInjective(a,true);
    return a;
  end );

InstallMethod( CanComputeActionOnPoints, 
  "for a projective group with frob with pspace isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
  function( g )
    local d,q;
    d := Dimension( g );
    q := Size( BaseField( g ) );
    if (q^d - 1)/(q-1) > DESARGUES.LimitForCanComputeActionOnPoints then
        return false;
    else
        return true;
    fi;
  end );

InstallMethod( SetAsNiceMono, 
  "for a projective group with Frobenius with pspace isomorphism and an action hom",
  [IsProjGroupWithFrobWithPSIsom, IsGroupHomomorphism and IsInjective],
  function( pg, a )
    SetNiceMonomorphism(pg,a);
    SetNiceObject(pg,Image(a));
  end );

InstallMethod( NiceMonomorphism, 
  "for a projective group with Frobenius with pspace isomorphism (feasible case)",
  [IsProjGroupWithFrobWithPSIsom and CanComputeActionOnPoints and
   IsHandledByNiceMonomorphism], 50,
  function( pg )
    return ActionOnPointsHyperplanes( pg );
  end );
  
InstallMethod( NiceMonomorphism, 
  "for a projective group with Frobenius with pspace isomorphism (nasty case)",
  [IsProjGroupWithFrobWithPSIsom and IsHandledByNiceMonomorphism], 50,
  function( pg )
    local can;
    can := CanComputeActionOnPoints(pg);
    if not(can) then
        Error("action on projective points not feasible to calculate");
    else
        return ActionOnPointsHyperplanes( pg );
    fi;
  end );

InstallMethod( ViewObj, 
  "for a projective group with frobenius with pspace isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
  function( g )
    Print("<projective group with Frobenius with proj. space isomorphism>");
  end );

InstallMethod( ViewObj, 
  "for a trivial projective group with frobenius with pspace isomorphism ",
  [IsProjGroupWithFrobWithPSIsom and IsTrivial],
  function( g )
    Print("<trivial projective group with Frobenius with proj. space isomorphism>");
  end );

InstallMethod( ViewObj, 
  "for a projective group with Frobenius with proj. space isomorphism with gens",
  [IsProjGroupWithFrobWithPSIsom and HasGeneratorsOfGroup],
  function( g )
    local gens;
    gens := GeneratorsOfGroup(g);
    if Length(gens) = 0 then
        Print("<trivial projective group with Frobenius with proj. space isomorphism>");
    else
        Print("<projective group with Frobenius with proj. space isomorphism with ",Length(gens),
              " generators>");
    fi;
  end );

InstallMethod( ViewObj, 
  "for a projective group with frobenius with pspace isomorphism with size",
  [IsProjGroupWithFrobWithPSIsom and HasSize],
  function( g )
    if Size(g) = 1 then
        Print("<trivial projective group with Frobenius with proj. space isomorphism>");
    else
        Print("<projective group with Frobenius with proj. space isomorphism of size ",Size(g),">");
    fi;
  end );

InstallMethod( ViewObj, 
  "for a projective group with frobenius with pspace isomorphism with gens and size",
  [IsProjGroupWithFrobWithPSIsom and HasGeneratorsOfGroup and HasSize],
  function( g )
    local gens;
    gens := GeneratorsOfGroup(g);
    if Length(gens) = 0 then
        Print("<trivial projective group with Frobenius with proj. space isomorphism>");
    else
        Print("<projective group with Frobenius with proj. space isomorphism of size ",Size(g)," with ",
              Length(gens)," generators>");
    fi;
  end );


