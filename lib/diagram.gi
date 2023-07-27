#############################################################################
##
##  diagram.gi              FinInG package
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
##  Implementation stuff for diagram geometries
##
#############################################################################

#################################################################
# Technicalities: Availability test for GRAPE and GraphViz/dotty
#################################################################

Whereisdot :=  function()	# Undocumented since technical
local str, outputtext;
    if not ARCH_IS_UNIX() then
       Info(InfoFinInG, 1, "Package `FinInG': non-Unix architecture! Don't know how to make diagram drawings in that case.");
       return false;
    else # See whether 'dot' is installed
    	 str:=""; outputtext:=OutputTextString(str, true);
	 Process(DirectoryCurrent(), Filename(DirectoriesSystemPrograms(), "which"),  InputTextNone(), outputtext, ["dot"]);  
	 if str = "" 
        then
      Info(InfoFinInG, 1, "Package `FinInG': dot not installed on your system. Please install GraphViz on your system.");
      Info(InfoFinInG, 1, "Package `FinInG': or find a friend who can compile your dot-files.");
      	return false;
      	fi;
    fi;
    return true;
end;


#############################################################################
##
##  Coset geometries
##
#############################################################################

#
#############################################################################
#O CosetGeometry
# 
# Constructs a Coset geometry from a group and a list of subgroups
##

InstallMethod( CosetGeometry, "for groups and list of subgroups",[ IsGroup , IsHomogeneousList ],
  function( g, l )

    ##  We assume the types of the geometry index the set l, the ordering
    ##  of l is important.

    local geo, ty;
    geo := rec( group := g, parabolics := l );
    ty := NewType( GeometriesFamily, IsCosetGeometry and IsCosetGeometryRep );
    ObjectifyWithAttributes( geo, ty,
           AmbientGeometry, geo,
           TypesOfElementsOfIncidenceStructure, [1..Size(l)],
	   RankAttr, Size(l),
           RepresentativesOfElements, l );
    ## To speed up IncidenceGraph (and hence also DiagramOfGeometry)
    if HasIsHandledByNiceMonomorphism(g) and IsHandledByNiceMonomorphism(g) and FINING.Fast then
       SetIsHandledByNiceMonomorphism(geo, true);
    fi;
    return geo;
  end );

# CHECKED 140913
#########################################################################
#O Rank2Residues
# 
# Adds all rank 2 residues to an incidence geometry
# Technical and undocumented
## 
InstallMethod( Rank2Residues, [ IsIncidenceGeometry ],
  function( geo )
    
    local types, residues, res, x, ty;
    types := TypesOfElementsOfIncidenceStructure( geo );
    residues := []; 
    for x in Combinations( types, 2 ) do
        res := rec( edge := x, geo := geo );
        ty := NewType( Rank2ResidueFamily, IsRank2Residue and IsRank2ResidueRep );
        Objectify( ty, res );
        Add(residues, res);
    od;
     
    Setter( Rank2ResiduesAttr )( geo, residues );
    return residues;
  end );

# CHECKED 25/3/2014
##########################################################################
#O MakeRank2Residue
# 
# Actually computes the given rank 2 residue of given type <edge>
# Technical and undocumented
##
InstallMethod( MakeRank2Residue, [ IsRank2Residue ],
  function( res )

    local geo, flag;
      geo:=res!.geo;
      if IsCosetGeometry(geo) then
        flag:=FlagOfIncidenceStructure(geo, StandardFlagOfCosetGeometry(geo)!.els{Difference(TypesOfElementsOfIncidenceStructure(geo),res!.edge)});
#I know this is ugly but I have no time to define subflags
        res!.residuegeometry:=CanonicalResidueOfFlag(geo,flag);
      else
        Error("Don't know how to compute residue!\n");
# We could use the generic residue here, with a warning.
      fi;
end );

####################################################
## Natural action on coset geometry element
##
####################################################

InstallGlobalFunction( OnCosetGeometryElement,
  function( c, t )
    return Wrap(c!.geo, c!.type, OnRight(c!.obj, t));
end ); 

# 
#############################################################################
#O  \^( <x>, <gm> )
##
InstallOtherMethod( \^, 
	"for an element of a coset geometry and an element of its group",
	[IsElementOfCosetGeometry, IsMultiplicativeElementWithInverse],
	function(x, gm)
		return OnCosetGeometryElement(x,gm);
	end );

# 
#############################################################################
#O  \^( <x>, <gm> )
##
InstallOtherMethod( \^, 
	"for a flag of a coset geometry and an element of its group",
	[IsFlagOfCosetGeometry, IsMultiplicativeElementWithInverse],
	function(x, gm)
		return FlagOfIncidenceStructure(x!.geo, OnTuples(x!.els,gm));
	end );

# Obsolete since there is generic version in geometry.g*
#############################################################################
#O  Type ( <flag> )
##
#InstallMethod( Type, 
#	"for a flag of a coset geometry",
#	[IsFlagOfCosetGeometry],
#	function(flag)
#		return Set(List(flag!.els,Type));
#	end );

# CHECKED 24/3/2014 PhC
# added check 17/12/14 jdb
#############################################################################
#O  FlagOfIncidenceStructure( <cg>, <els> )
# returns the flag of the coset geometry <cg> with elements in <els>.
# the method does check whether the input really determines a flag.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a coset geometry and a list of pairwise incident elements",
	[ IsCosetGeometry, IsElementOfIncidenceStructureCollection ],
	function(cg,els)
		local list,i,test,type,flag;
		list := Set(ShallowCopy(els));
		if Length(list) > Rank(cg) then
		  Error("A flag must contain at most Rank(<cg>) elements.\n");
		fi;
        test := List(list,x->AmbientGeometry(x));
        if not ForAll(test,x->x=cg) then
            Error("not all elements have <incgeo> as ambient geometry");
        fi;
        test := Set(List([1..Length(list)-1],i -> IsIncident(list[i],list[i+1])));
		if (test <> [ true ] and test <> []) then
		  Error("<els> does not determine a flag.\n");
		fi;
		flag := rec(geo := cg, types := List(list,x->x!.type), els := list);
		ObjectifyWithAttributes(flag, IsFlagOfCGType, IsEmptyFlag, false, RankAttr, Size(list));
		return flag;
	end);

#############################################################################
#O  FlagOfIncidenceStructure( <ps>, <els> )
# returns the empty flag of the projective space <ps>.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a coset geometry and an empty list",
	[ IsCosetGeometry, IsList and IsEmpty ],
	function(cg,els)
		local flag;
		flag := rec(geo := cg, types := [], els := []  );
		ObjectifyWithAttributes(flag, IsFlagOfCGType, IsEmptyFlag, true, RankAttr, 0 );
		return flag;
	end);

# Obsolete since there is a generic version 140913
#############################################################################
#O \= ( < flag1 >, <flag2 > )
# Returns true if <flag1> and <flag2> represent the same flag in a 
# coset geometry 
# 
##
#InstallOtherMethod( \=, [ IsFlagOfCosetGeometry, IsFlagOfCosetGeometry ],
#  function( flag1, flag2 )
#    local result, cg1, cg2;
#    cg1:=flag1!.geo;
#    cg2:=flag2!.geo;
#    if cg1 <> cg2 then
#      Print("#I the two flags are not in the same incidence structure!\n");
#      return false;
#    fi;
#    result:= (Set(flag1!.types)=Set(flag2!.types)) and (Set(flag1!.els)=Set(flag2!.els));
#    return result;
#  end );

# 

# 140913 PhC
#############################################################################
#O \= ( < cg1 >, <cg2 > )
# Returns true if <cg1> and <cg2> are the same coset geometry
# 
##
InstallOtherMethod( \=, [ IsCosetGeometry, IsCosetGeometry 
],
  function( cg1, cg2 )
    local result, g1, g2, pabs1, pabs2;
    result:=(cg1!.group = cg2!.group);
    result:=result and (cg1!.parabolics = cg2!.parabolics);
    return result;
  end );

#

# CHECKED 140913 PhC
##########################################################################
#O ElementsOfIncidenceStructure
#
# Gives elements of type <j> in coset geometry <cg>
##
InstallMethod( ElementsOfIncidenceStructure, [IsCosetGeometry, IsPosInt],
  function( cg, j )

    local vars;
    if j > Rank(cg) then 
      Error("<cg> has no elements of type <j>.\n");
    fi;
    vars := rec( geometry := cg, type := j );
    Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsElementsOfCosetGeometry and
                                IsElementsOfCosetGeometryRep),
        vars );
    return vars;
end );

# CHECKED 140913 PhC
#############################################################################
#O  ElementsOfIncidenceStructure( <cg> )
# returns all the elements of the coset geometry <cg> 
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for a coset geometry",
	[IsCosetGeometry],
	function( cg )
		return Objectify(
			NewType( ElementsCollFamily, IsAllElementsOfCosetGeometry and IsAllElementsOfCosetGeometryRep ),
				rec( geometry := cg,
					type := "all") #added this field in analogy with the collection that contains all subspaces of a vector space.
				);
	end);

# CHECKED 140913 PhC
#############################################################################
#O  RandomElement( <cg> )
# returns a random element of random type in the given coset geometry
# <cg>
##
InstallMethod( RandomElement, 
	"for a coset geometry",
	[IsCosetGeometry],
	function(cg)
		local i;
		i:=Random(TypesOfElementsOfIncidenceStructure(cg));
		return Random(ElementsOfIncidenceStructure(cg,i));
	end );

# CHECKED 140913 PhC
#############################################################################
#O  RandomChamber( <cg> )
# returns a random chamber in the given coset geometry
# <cg>
##
InstallOtherMethod( RandomChamber, 
	"for a coset geometry",
	[IsCosetGeometry],
	function(cg)
		local ele, els;
		ele:=Random(cg!.group);
		els:=OnTuples(StandardFlagOfCosetGeometry(cg)!.els,ele);
		return FlagOfIncidenceStructure(cg,els);
	end );

# CHECKED 140913 PhC
#############################################################################
#O  RandomFlag( <cg> )
# returns a random flag in the given coset geometry
# <cg>
##
InstallOtherMethod( RandomFlag, 
	"for a coset geometry",
	[IsCosetGeometry],
	function(cg)
		local type, flag;
		type:=Random(Combinations(TypesOfElementsOfIncidenceStructure(cg)));
		flag:=RandomChamber(cg)!.els{type};
		return FlagOfIncidenceStructure(cg,flag);
	end );

# 140806 Not needed! Already defined generically 
#############################################################################
#O  ElementsOfFlag( <flag> )
# returns elements of a flag 
# 
##
#InstallMethod( ElementsOfFlag, 
#	"for a coset geometry flag",
#	[IsFlagOfCosetGeometry],
#	function(flag)
#		return flag!.els;
#	end );


# 
#############################################################################
#O  Random( <alleleofcg> )
# returns a random element a coset geometry
# 
##
InstallMethod( Random, 
	"for element category of coset geometry",
	[IsAllElementsOfCosetGeometry],
	function(allele)
		return RandomElement(allele!.geometry);
	end );

#
###########################################################################
#O Size
#
# Returns the number of elements in the object <vs> of type ElementsOfCosetGeometry
##
InstallMethod(Size, [IsElementsOfCosetGeometry],
  function( vs )

    local cg;
    cg := vs!.geometry;
    return IndexNC(cg!.group, cg!.parabolics[vs!.type]);
  end );

#
###########################################################################
#O Wrap
#
# Make an object from an element of a coset geometry
##
InstallMethod( Wrap, "for a coset geometry and an object (coset)",
  [IsCosetGeometry, IsPosInt, IsObject],
  function( geo, type, o )

    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily,
      IsElementOfCosetGeometryRep and IsElementOfCosetGeometry ), w );
    return w;
end );

#
##########################################################################
#O Iterator
#
# Iterator
##
InstallMethod(Iterator, "for elements of a coset geometry",
        [IsElementsOfCosetGeometry],
  function( vs )

    local cg, j, g, h, iter, newiter;
    cg := vs!.geometry;
    j := vs!.type;
    g := cg!.group;
    h := cg!.parabolics[j];
    iter := Iterator(RightCosets(g,h));    
    ## Wrap 'em up
    newiter := IteratorByFunctions( rec(
            NextIterator := function( i )
              local x;
              x := NextIterator(i!.S);
              return Wrap(cg, j, x);
            end,
            IsDoneIterator := function(i)
              return IsDoneIterator(i!.S);
            end,
            ShallowCopy := function(i)
              return rec( S := ShallowCopy(i!.S) );
            end,
            S := iter ));
    return newiter;
end );

# CHECKED 06/09/11 PhC
#############################################################################
#O  IsIncident( <ele1>, <ele2> )
#
# for elements ele1 and ele2 from the same coset geometry. Checks nonempty 
# intersection unless types are the same, then only EQUAL elements can be 
# incident.
##

InstallMethod( IsIncident, 
		"for elements of a coset geometry", 
              	[IsElementOfCosetGeometry, IsElementOfCosetGeometry],
  function( x, y )

    local vx, vy, tx, ty, g, h, k;
    if x!.geo <> y!.geo then Error("The two elements do not belong to the same incidence structure.\n"); fi;
    vx := x!.obj; vy := y!.obj;
    tx := x!.type; ty := y!.type;
    if (tx = ty) then 
      if vx=vy then
	return true;
      else # There was a problem here when making the incidence graph, because of loops. 
        return false;
      fi;
    fi;
    ## Let Ha and Kb be two right cosets, and let g=ab^-1.
    ## Then Ha and Kb intersect if and only if g is an element of the double
    ## coset HK.
    g := Representative(vx) * Representative(vy)^-1;
    h := ActingDomain(vx);
    k := ActingDomain(vy);
    return g in DoubleCoset(h, One(h), k);
end );

###################################################
## Extract components from Coset geometry object
##
##
###################################################

# CHECKED 28/11/11 PhC
#############################################################################
#O  ParabolicSubgroups ( <cg> )
# for a coset geometry <cg> returns the defining parabolic subgroups.
##
InstallMethod( ParabolicSubgroups, "for coset geometries",
               [ IsCosetGeometry ],  cg -> cg!.parabolics );

# CHECKED 28/11/11 PhC
#############################################################################
#O  AmbientGroup ( <cg> )
# for a coset geometry <cg> returns the defining group.
##
InstallMethod( AmbientGroup, "for coset geometries",
               [ IsCosetGeometry ],  cg -> cg!.group );

# CHECKED 28/11/11 PhC
#############################################################################
#O  BorelSubgroup ( <cg> )
# for a coset geometry <cg> returns the Borel subgroup. This is the 
# intersection of the parabolic subgroups.
##
InstallMethod( BorelSubgroup, "for coset geometries",
               [ IsCosetGeometry ],  cg -> Intersection(cg!.parabolics) );

##############################################################
## Investigate some properties for Coset geometries
##
##
##############################################################

# CHECKED 18/3/2014 PhC
#############################################################################
#O IsFlagTransitiveGeometry
#  
# Checks whether a Coset incidence structure is flag transitive
##
InstallMethod( IsFlagTransitiveGeometry, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    ## From Buekenhout's chapter in the "Handbook of Incidence Geometry"
    ## a coset geometry with parabolics {Gi} is flag-transitive if
    ## (G1 G2) \cap (G1 G3) \cap ... \cap (G1 Gn) = G1( G2 \cap...\cap Gn),
    ## (G2 G3) \cap ...\cap (G2 Gn) = G2( G3 \cap ... \cap Gn),
    ## ..., (G(n-2) G(n-1)) \cap (G(n-2) G(n)) = G(n-2)( G(n-1)\cap Gn )
    ## For each case, we have that the right-hand side is contained in 
    ## the left-hand side, so it suffices to compute sizes of the entities
    ## in each equation.

    local g, parabolics, gi, gj, orb, trans, rank, 
          left, int, newint, act, right, i, j;
  
    g := cg!.group;
    parabolics := cg!.parabolics;
    rank := Size(parabolics);

    ## Note that |G1( G2 \cap...\cap Gn)| = |( G2 \cap...\cap Gn) : (G1\cap G2 \cap...\cap Gn)|.
    ## So to efficiently compute the right hand side, we do the "shortest one" first
    ## and iterate. To compute the left hand sides is much more difficult.
    ## For the first case, we need to compute the intersections of the orbits
    ## of G2, G3, ..., Gn on the trivial coset G1 in the action of G on
    ## the right cosets of G1.
   
    ## Start from the end, where the number of intersections are shortest
    ## and go upwards.

    int := Intersection( parabolics[rank - 1], parabolics[rank] );

    for i in [2..rank-1] do
        # compute right-hand side
        newint := Intersection( int, parabolics[rank - i] );
        right := Size(int) / Size(newint);
       
        # compute left-hand side
        gi := parabolics[rank-i];
        act := FactorCosetAction(g, gi);
        orb := [1..Index(g,gi)];
        for j in [rank-i+1..rank] do
            gj := Image(act, parabolics[j]);
            orb := Intersection(orb, Orbit(gj, 1)); 
        od;
        left := Size(orb);
        if left > right then 
           return false;
        fi;
        int := newint;
    od;
    return true;
end );

# CHECKED 24/3/2014 PhC
#############################################################################
#O IsFirmGeometry
#  
# Checks whether a coset geometry is firm
##
InstallMethod( IsFirmGeometry, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )

    local parabolics, pis, without_i, int, borel, i;
    parabolics := ParabolicSubgroups( cg );
    pis := [];
    for i in [1..Size(parabolics)] do
        without_i := Difference( [1..Size(parabolics)], [ i ] );
        int := Intersection( parabolics{without_i} );
        Add(pis, int);
    od;

    if HasIsFlagTransitiveGeometry( cg ) and IsFlagTransitiveGeometry( cg ) then
       ## In this case, the coset geometry is firm
       ## if and only if the borel subgroup is not
       ## contained in any Pi = \cap{Hj: j<>i}.

       borel := BorelSubgroup( cg );
       return not ForAny(pis, t -> IsSubset(borel, t));
    else
       ## In this case, the coset geometry is firm
       ## if and only if for every i, Pi is not contained in Hi
       ## (where the Hi's are the parabolics).
       
       return not ForAny([1..Size(parabolics)], i -> 
              IsSubset(parabolics[i], pis[i]) );
    fi;
end );

# CHECKED 24/3/2014 PhC
#############################################################################
#O IsThinGeometry
#  
# Checks whether a coset geometry is thin
##
InstallMethod( IsThinGeometry, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )

    local parabolics, pis, without_i, int, borel, i, result;
    parabolics := ParabolicSubgroups( cg );
    pis := [];
    for i in [1..Size(parabolics)] do
        without_i := Difference( [1..Size(parabolics)], [ i ] );
        int := Intersection( parabolics{without_i} );
        Add(pis, int);
    od;
    result:= ForAll(pis, psg -> Index(psg, BorelSubgroup(cg))=2 );
    if result then 
      Setter(IsThickGeometry)(cg,false); 
      Setter(IsFirmGeometry)(cg, true);
    fi;
    return result;
end );

# CHECKED 27/3/2014 PhC
#############################################################################
#O IsThickGeometry
#  
# Checks whether a coset geometry is thick
##
InstallMethod( IsThickGeometry, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )

    local parabolics, pis, without_i, int, borel, i, result;
    parabolics := ParabolicSubgroups( cg );
    pis := [];
    for i in [1..Size(parabolics)] do
        without_i := Difference( [1..Size(parabolics)], [ i ] );
        int := Intersection( parabolics{without_i} );
        Add(pis, int);
    od;
    result:= ForAll([1..Size(parabolics)], i -> 
              Index(pis[i], BorelSubgroup(cg))>2 );
    if result then 
      Setter(IsThinGeometry)(cg,false); 
      Setter(IsFirmGeometry)(cg,true);
    fi;
    return result;
end );

# CHECKED 06/09/11 PhC
#############################################################################
#O  IsConnected( <cg> )
# Returns true iff the coset geometry cg is connected
##

InstallMethod( IsConnected, 
		"for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    ## A coset geometry is connected if and only if the
    ## parabolics generate the group.

    local parabolics, g, gens;
    g := cg!.group;
    parabolics := ParabolicSubgroups( cg );
    gens := Union( List(parabolics, GeneratorsOfGroup) );
    return Group( gens ) = g;
end );

# CHECKED 07/02/12 PhC 
#############################################################################
#O IsResiduallyConnected
#  
# Returns true iff the coset geometry is residually connected
##
InstallMethod( IsResiduallyConnected, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    ## A coset geometry is residually connected if and only
    ## if for every subset J of the types I with |I-J|>1, we have
    ## \cap{Hj: j in J} = < \cap{Hi: i in J\cup {j}} : j in I-J >
  local rank, typeset, typesubsets, k, lhs, rhs, subset, test, int, iter;
    rank:=Rank(cg);
    typesubsets:=[];
    typeset:=TypesOfElementsOfIncidenceStructure(cg);
    for k in [1..rank-2] do
      Append(typesubsets, Combinations(typeset, k));
    od;
    iter := Iterator(typesubsets);
    test:=true;
    while not IsDoneIterator(iter) and test do
      subset:=NextIterator(iter);
      lhs := Size(Intersection(ParabolicSubgroups(cg){subset}));
      rhs := [];
      int:=Intersection(ParabolicSubgroups(cg){subset});
      for k in Difference(typeset,subset) do
        Add(rhs, Intersection(int,ParabolicSubgroups(cg)[k]));
      od;
      rhs:=Size(SubgroupNC(cg!.group,Union(rhs)));
      if lhs<>rhs then test:=false; fi;
    od;
    return test;
end );

# CHECKED 24/3/2014 PhC
#############################################################################
#O StandardFlagOfCosetGeometry(<cg>)
#  
# Returns standard chamber of a coset geometry <cg>.
##
InstallMethod( StandardFlagOfCosetGeometry, "for coset geometries",
	       [ IsCosetGeometry ],
  function(cg)
  local parabolics, flag;

  parabolics:=cg!.parabolics;
  flag:=List([1..Size(parabolics)], i -> Wrap(cg,i,RightCoset(parabolics[i],Identity(cg!.group))));
  return FlagOfIncidenceStructure(cg,flag);
end );



# CHECKED 27/3/2014
#############################################################################
# FlagToStandardFlag
#  
# 
##
InstallMethod( FlagToStandardFlag, "for coset geometries",
                [ IsCosetGeometry, IsFlagOfCosetGeometry ],
  function( cg, flag )
  
    ## This operation returns an element g which maps 
    ## "flag" to the standard flag consisting of trivial
    ## cosets of parabolic subgroups.
    ## Suppose [G(1) a(1), ..., G(n) a(n)] is a flag.
    ## Let x(1) = a(1)^-1 and then define x(i) inductively
    ## by the condition
    ##  x(i)^-1 in G(1) \cap G(2)\cap ... \cap G(i-1)\cap G(i)a(i)x(1)x(2)...x(i-1)
    ## Then g = x(1)x(2)...x(n) maps our flag to [G(1),...,G(n)].
    
    local g, x, reps, pabs, i, int, ginv;
    
    # initialise
    flag:=flag!.els;
    reps := List(flag, t -> Representative(t!.obj) );
    pabs := ParabolicSubgroups( cg ){ List(flag, t->t!.type) };
    g := reps[1]^-1;
    int := pabs[1];
    for i in [2..Size(flag)] do
        if i > 2 then 
           int := Intersection(int, pabs[i-1]); 
        fi;
        
        ginv := g^-1;
        repeat
            x := PseudoRandom(int);
        until x * ginv in flag[i]!.obj;
        
        g := g * x^-1;
    od;    
    return g;
  end );

# CHECKED 27/3/2014 PhC
#############################################################################
# CanonicalResidueOfFlag
#  
# 
##
InstallMethod( CanonicalResidueOfFlag, "for coset geometries",
               [ IsCosetGeometry, IsFlagOfCosetGeometry ],

  function( cg, flag )

    ## return coset geometry which is isomorphic to residue of the given flag
    ## have non-empty intersection with all of the elements
    ## of flag

    local typesflag, types, parabolics, i, resg, respabs;

     typesflag := Set(flag!.types);
#    if IsFlagTransitiveGeometry( cg ) then
#       typesflag := Set(flag, t->t!.type);
       if IsEmpty( typesflag ) then
         return cg;
       fi;
       types := Difference( TypesOfElementsOfIncidenceStructure(cg), typesflag);
       if IsEmpty( types ) then
          return CosetGeometry( Group(()), [] );
       fi;
 
       parabolics := ParabolicSubgroups( cg ){types};;
           
       resg := Intersection( ParabolicSubgroups( cg ){typesflag} );
       respabs := [];
       for i in parabolics do
          Add( respabs, Intersection( resg, i ) );
       od;
#     else
#       Error("not implemented for not flag-transitive geometries.\n");
#     fi;
     
     return CosetGeometry( resg, respabs ); 
  end );

# CHECKED 140913 PhC
#############################################################################
#O ResidueOfFlag
#  
# Computes the residue of the given flag in AmbientGeometry(flag)
##
InstallOtherMethod( ResidueOfFlag, "for coset geometries",
               [ IsFlagOfCosetGeometry ],

  function( flag )

    ## return all right cosets of parabolics which
    ## have non-empty intersection with all of the elements
    ## of flag

    local cg, typesflag, types, r, parabolics, i, resg, respabs;

    cg:=flag!.geo;
    if IsFlagTransitiveGeometry( cg ) then
       typesflag := Set(flag!.types);
       if IsEmpty( typesflag ) then
         return cg;
       fi;
       types := Difference( TypesOfElementsOfIncidenceStructure(cg), typesflag);
       if IsEmpty( types ) then
          return CosetGeometry( Group(()), [] );
       fi;
 
       parabolics := ParabolicSubgroups( cg ){types};;
           
       resg := Intersection( ParabolicSubgroups( cg ){typesflag} );
       respabs := [];
       for i in parabolics do
          Add( respabs, Intersection( resg, i ) );
       od;
       
       ## up to this point, we have the residue of the standard flag, 
       ## and so we need to map back to our original flag
       
       ## The mathematics here needs checking/testing!
       
       r := FlagToStandardFlag( cg, flag );  
       r:=Inverse(r);
       respabs := List(respabs, t -> t^r);
       resg := resg^r;
     else
       Error("not implemented for not flag-transitive geometries.\n");
     fi;
     
     return CosetGeometry( resg, respabs ); 
  end );

# 
#############################################################################
# IncidenceGraph
#  
# 
##
InstallMethod( IncidenceGraph, [ IsCosetGeometry and IsHandledByNiceMonomorphism ],
  function( geo )
  
  ## This operation returns the multiparitite incidence graph
  ## associated to "geo", and it also sets a mutable attribute
  ## IncidenceGraphAttr which can be called hence.  
  ## Here we have a speedup from the NiceMonomorphism

    local fastgeo, gamma, hom;

    #the following is obsolete: grape is a NeededOtherPackage (PackageInfo.g)
    #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
    #   Error("You must load the Grape package in order to use IncidenceGraph.\n");
    #fi;

    if FINING.Fast then
      Print("#I Using NiceMonomorphism...\n");   
      hom := NiceMonomorphism(geo!.group);
      fastgeo:=CosetGeometry(NiceObject(geo!.group), List(geo!.parabolics, h -> Image(hom, h)));
      gamma := IncidenceGraph( fastgeo );
    else
      gamma := IncidenceGraph( geo );
    fi;
    Setter( IncidenceGraphAttr )( geo, gamma );
    return gamma;
  end );

# 
#############################################################################
# IncidenceGraph
#  
# 
##
InstallMethod( IncidenceGraph, [ IsCosetGeometry ],
  function( geo )
  
  ## This operation returns the multiparitite incidence graph
  ## associated to "geo", and it also sets a mutable attribute
  ## IncidenceGraphAttr which can be called hence.  
  ## Slower version
  
    local g, vars, gamma, allvars, reps, hom1, hom2, im1, im2, d, em1, em2, gens, newgens, diagonal;

    #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
    #   Error("You must load the Grape package.\n");
    #fi;

    g := geo!.group;

    if Size(ParabolicSubgroups(geo)) = 2 then
      
       hom1 := FactorCosetAction(g, ParabolicSubgroups(geo)[1]);
       hom2 := FactorCosetAction(g, ParabolicSubgroups(geo)[2]);
       im1 := Image(hom1);
       im2 := Image(hom2);
       d := DirectProduct(im1,im2);
       em1 := Embedding(d,1);
       em2 := Embedding(d,2);
       gens := GeneratorsOfGroup(g);
       newgens := List(gens, x -> Image(em1,Image(hom1,x)) * Image(em2,Image(hom2,x)));
       diagonal:=Group(newgens);
       gamma := NullGraph(diagonal);   
   	   ## [A,B]
       AddEdgeOrbit(gamma, [1, DegreeAction(im1)+1]);
       AddEdgeOrbit(gamma, [DegreeAction(im1)+1, 1]);

    else

       allvars := List( [1..Size(TypesOfElementsOfIncidenceStructure(geo))],
                    i -> ElementsOfIncidenceStructure(geo, i) );
       vars := Concatenation( List(allvars, AsList) );
       gamma := Graph( g, vars, OnCosetGeometryElement, 
                function(x,y) return IsIncident(x,y); end, true);
    fi; 
    Setter( IncidenceGraphAttr )( geo, gamma );
    return gamma;
  end );

#
################################################################
# O AutGroupIncidenceStructureWithNauty( <inc> )
#
#   Computes automorphism group of incidence structure via Nauty
#   Should first check whether <inc> has an incidence graph
#   At the moment this works for coset geometries
#
#   The resulting group is a permutation group and its action on 
#   the original incidence structure is not explicitly given
#
InstallMethod( AutGroupIncidenceStructureWithNauty, 
	       [ IsCosetGeometry ],
  function( inc )

  local colorclasses, sum, i, incgrph;

# automorphisms should fix the type set. This is done by using 
# colorClasses in Nauty. We use the fact that when defining the 
# incidence graph, the vertices are an enumeration of all elements 
# of type 1, followed by alle elements of type 2, ...
# So the color classes are easy to give.
#
    colorclasses:=[];
    sum:=0;
    for i in TypesOfElementsOfIncidenceStructure(inc) do
	Add(colorclasses,
	[sum+1..sum+NrElementsOfIncidenceStructure(inc,i)]);
	sum:=sum+NrElementsOfIncidenceStructure(inc,i);
    od;

    incgrph:=IncidenceGraph(inc);

    return(AutGroupGraph(incgrph, colorclasses));

  end);

#
################################################################
# O CorGroupIncidenceStructureWithNauty( <inc> )
#
#   Computes correlation group of incidence structure via Nauty
#   Should first check whether <inc> has an incidence graph
#   At the moment this works for coset geometries
#
InstallMethod( CorGroupIncidenceStructureWithNauty, 
	       [ IsCosetGeometry ],
  function( inc )
    return(AutGroupGraph(IncidenceGraph(inc)));
end );

#
################################################################
# O IsIsomorphicIncidenceStructureWithNauty( <inc1>, <inc2> )
#
#   Computes whether <inc1> and <inc2> are isomorphic incidence 
#   structures, using nauty on coloured graphs.
#
InstallMethod( IsIsomorphicIncidenceStructureWithNauty, 
	       [ IsCosetGeometry, IsCosetGeometry ],
  function( inc1, inc2 )

  local colorclasses1, colorclasses2, sum, i, incgrph1, incgrph2;

    colorclasses1:=[];
    sum:=0;
    for i in TypesOfElementsOfIncidenceStructure(inc1) do
	Add(colorclasses1,
	[sum+1..sum+NrElementsOfIncidenceStructure(inc1,i)]);
	sum:=sum+NrElementsOfIncidenceStructure(inc1,i);
    od;

    colorclasses2:=[];
    sum:=0;
    for i in TypesOfElementsOfIncidenceStructure(inc2) do
	Add(colorclasses2,
	[sum+1..sum+NrElementsOfIncidenceStructure(inc2,i)]);
	sum:=sum+NrElementsOfIncidenceStructure(inc2,i);
    od;

    if colorclasses2 <> colorclasses1 then
      return false;
    fi;

    incgrph1:=IncidenceGraph(inc1);
    incgrph2:=IncidenceGraph(inc2);

    return(IsIsomorphicGraph(rec(graph:=incgrph1, colourClasses:=colorclasses1), rec(graph:=incgrph2, colourClasses:=colorclasses1)));

  end);

#############################################################################
# View/Print methods
#############################################################################

InstallMethod( ViewObj, "for diagrams", 
  [ IsDiagram and IsDiagramRep ],
  function( diag )
    Print("< Diagram >");
  end );

InstallMethod( ViewObj, "for diagram with geometry", 
  [ IsDiagram and IsDiagramRep and HasGeometryOfDiagram],
  function( diag )
    local geo;
    geo := GeometryOfDiagram( diag );
    Print("< Diagram of ");
    ViewObj(geo);
    Print(" >");
  end );

InstallMethod( ViewObj, "for coset geometry",
  [ IsCosetGeometry and IsCosetGeometryRep ],
  function( geo )
    Print("CosetGeometry( ", geo!.group, " )");
  end );

InstallMethod( ViewObj, "for flag of coset geometry",
  [ IsFlagOfCosetGeometry ],
  function( flag )
    Print("<Flag of coset geometry < ", flag!.geo, " >>");
  end );

InstallMethod( PrintObj, "for flag of coset geometry", 
  [ IsFlagOfCosetGeometry ],
  function( flag )
    Print("Flag of coset geometry ", flag!.geo, " with elements ", flag!.els );
  end );

InstallMethod( PrintObj, "for coset geometry", 
  [ IsCosetGeometry and IsCosetGeometryRep ],
  function( geo )
    Print("CosetGeometry( ", geo!.group, " , ", geo!.parabolics , " )");
  end );

InstallMethod( ViewObj, "for all elements of coset geometry", 
  [ IsElementsOfCosetGeometry and IsElementsOfCosetGeometryRep ],
  function( vs )
    Print("<elements of type ", vs!.type," of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, "for coset geometry", [ IsElementsOfCosetGeometry and
  			IsElementsOfCosetGeometryRep ],
  function( vs )
    Print("ElementsOfIncidenceStructure( ",vs!.geometry," , ",vs!.type,")");
  end );

InstallMethod( ViewObj, "for coset geometry", [ IsElementOfCosetGeometry ],
  function( v )
    Print("<element of type ", v!.type," of ");
    ViewObj(v!.geo);
    Print(">");
  end );

InstallMethod( PrintObj, "for element of coset geometry", [ IsElementOfCosetGeometry ],
  function( v )
    Print(v!.obj);
  end );

InstallMethod( ViewObj, "for vertex of diagram", [ IsVertexOfDiagram and IsVertexOfDiagramRep ],
  function( v )
    Print("Diagram vertex(",v!.type,")");
  end );

InstallMethod( PrintObj, "for vertex of diagram", [ IsVertexOfDiagram and IsVertexOfDiagramRep ],
  function( v )
    Print("Vertex(",v!.type,")");
  end );

InstallMethod( ViewObj, "for edge of diagram", [ IsEdgeOfDiagram and IsEdgeOfDiagramRep ],
  function( e )
    Print("Diagram edge(",e!.edge,")");
  end );

InstallMethod( PrintObj, "for edge of diagram", [ IsEdgeOfDiagram and IsEdgeOfDiagramRep ],
  function( e )
    Print("Edge(",e!.edge,")");
  end );

InstallMethod( ViewObj, "for rank 2 residue", [ IsRank2Residue and IsRank2ResidueRep ],
  function( e )
    Print("Rank 2 residue of type ",e!.edge," of ", e!.geo);
  end );

InstallMethod( PrintObj, "for rank 2 residue", [ IsRank2Residue and IsRank2ResidueRep  ],
  function( e )
    Print("Rank2Residue(",e!.edge,") of ", e!.geo);
  end );

#############################################################################
# Methods for diagrams of geometries.
#############################################################################

# 
#############################################################################
#O \= ( < vertex1 >, <vertex2 > )
# Returns true if <vertex1> and <vertex2> represent the same type in a 
# diagram.
# 
##
InstallMethod( \=, [ IsVertexOfDiagram and IsVertexOfDiagramRep, 
                     IsVertexOfDiagram and IsVertexOfDiagramRep ],
  function( u, v )
    return u!.type = v!.type;
  end );

# 
#############################################################################
#O \= ( < edge1 >, < edge 2 > )
# Equality of edges in a diagram.
# 
##
InstallMethod( \=, [ IsEdgeOfDiagram and IsEdgeOfDiagramRep, 
                     IsEdgeOfDiagram and IsEdgeOfDiagramRep ],
  function( u, v )
    return u!.edge = v!.edge;
  end );

# CHECKED 140913 PhC
#############################################################################
#F DiagramOfGeometry
# 
# 
##
InstallMethod( DiagramOfGeometry, "for flag-transitive coset geometry", [IsCosetGeometry],

  function( cg )
    local rank2residues, vertices, types, x, v, edges, parameters, e, diagram, parabolics;

##########Removed on 20/03/2012 by PhC because FT test is time-consuming.
# The documentation mentions this
#
#    if not IsFlagTransitiveGeometry(cg) then
#            Error("usage DiagramOfGeometry: only works for flag-transitive geometries.\n");
#    fi;
    rank2residues := Rank2Residues( cg );
    Perform(rank2residues, MakeRank2Residue);

    parabolics := cg!.parabolics;
	vertices := [];
    types := TypesOfElementsOfIncidenceStructure( cg );

  ## Wrapping up... 
    for x in types do
      v := rec( type := x );
      Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v);
      Add( vertices, v );
    od;

    edges := [];

  ## Wrapping up...
    for x in rank2residues do
        parameters := Rank2Parameters( x!.residuegeometry );
        if not parameters[1] = [2,2,2] then
           e := rec( edge := x!.edge );
           Objectify( NewType( EdgeOfDiagramFamily, 
                        IsEdgeOfDiagram and IsEdgeOfDiagramRep ), e);
           SetParametersEdge(e, parameters[1]);
           if not HasOrderVertex( vertices[x!.edge[1]] ) then
              SetOrderVertex( vertices[x!.edge[1]], parameters[2][1] );
              SetNrElementsVertex( vertices[x!.edge[1]], IndexNC(cg!.group, parabolics[x!.edge[1]]) );
              SetStabiliserVertex( vertices[x!.edge[1]], parabolics[x!.edge[1]] );
           fi;
           if not HasOrderVertex( vertices[x!.edge[2]] ) then
              SetOrderVertex( vertices[x!.edge[2]], parameters[3][1] );
              SetNrElementsVertex( vertices[x!.edge[2]], IndexNC(cg!.group, parabolics[x!.edge[2]]) );
              SetStabiliserVertex( vertices[x!.edge[2]], parabolics[x!.edge[2]] );
           fi;
           Add( edges, e );
        fi;
    od;

	diagram := rec( vertices := vertices, edges := edges );;
    Objectify( NewType( DiagramFamily, IsDiagram and IsDiagramRep ), diagram);

    SetGeometryOfDiagram( diagram, cg );
    return diagram;
  end );

# CHECKED 24/3/2014 PhC
#############################################################################
#O DrawDiagram
# 
# 
##
InstallGlobalFunction( DrawDiagram, 
  function( arg )
    local diagram, filename, vertices, edges, longstring, v, e, vertexverbosity, edgeverbosity, arglen;
   
    vertexverbosity:=0; #default orders and nr of elements
    edgeverbosity:=1;   #default shorthand for generalized n-gons
    arglen:=Length(arg);
    ############################# Checking arguments and initializing
    if not(arglen in [2,3,4]) then
      Error("usage DrawDiagram: must have at least 2 and at most 4 arguments.\n");
    elif not(IsDiagram(arg[1]) and IsString(arg[2])) then
      Error("usage DrawDiagram: first argument must be a diagram, second argument must be a filename string.\n");
    fi;

    diagram:=arg[1];
    filename:=arg[2];
    vertices := diagram!.vertices;
    edges := diagram!.edges;  

    if arglen > 2 then
      if IsPosInt(arg[3]) or IsZero(arg[3]) then
        vertexverbosity:=arg[3];
      else
        Error("usage DrawDiagram: third argument must be a vertex verbosity natural number.\n");
      fi;
      if (arglen = 4) then
      	 if (IsPosInt(arg[4]) or IsZero(arg[4])) then
           edgeverbosity:=arg[4];
         else
           Error("Usage DrawDiagram: fourth argument must be an edge verbosity natural number.\n");
	 fi;
      fi;
    fi;
    ##################Done checking!
    longstring := "digraph DIAGRAM{\n rankdir = LR;\n";
    for v in vertices do
      longstring:=Concatenation(longstring, "subgraph cluster", String(v!.type), " {\n");
      longstring:=Concatenation(longstring, "color=transparent\n");    
      longstring:=Concatenation(longstring, "node [shape=circle, width=0.1]; ", String(v!.type), " [label=\"\"];\n");    
      longstring:=Concatenation(longstring, "labelloc=b\n");    
      if vertexverbosity =2 then
        longstring:=Concatenation(longstring, "label=\"", "\";\n}\n"); 
      elif vertexverbosity =1 then
        longstring:=Concatenation(longstring, "label=\"", String(OrderVertex(v)),
                   "\";\n}\n"); 
      else
        longstring:=Concatenation(longstring, "label=\"", String(OrderVertex(v)), "\\n", String(NrElementsVertex(v)),
                   "\";\n}\n"); 
      fi;
    od;

    for e in edges do
      longstring:=Concatenation(longstring, String(e!.edge[1]), " -> ", String(e!.edge[2]));
      if edgeverbosity = 2 then #no label, i.e. basic diagram
        longstring:=Concatenation(longstring, " [label = \"", "\", arrowhead = none ];\n");
      elif edgeverbosity = 1 and Size(Set(ParametersEdge(e)))= 1 then #shorthand generalized n-gons
        longstring:=Concatenation(longstring, " [label = \"", String(ParametersEdge(e)[1]), "\", arrowhead = none ];\n");
      else
        longstring:=Concatenation(longstring, " [label = \"", String(ParametersEdge(e)[2]), " ", 
                  String(ParametersEdge(e)[1]), " ", 
                  String(ParametersEdge(e)[3]), "\", arrowhead = none ];\n");
      fi;
    od;    

    longstring:=Concatenation(longstring, "}\n");    
    PrintTo( Concatenation(filename, ".dot") , longstring );
    ######## Checking the availability of GraphViz dot in path...
    if Whereisdot() then
        Exec( Concatenation("dot -Tps ", filename, ".dot -o ", filename, ".ps") );
    else
        Info(InfoWarning, 1, "Package `FinInG': Only .dot file written for diagram.");
    fi;	
    return;
  end );

# CHECKED 25/3/2014 PhC
#############################################################################
#O DrawDiagramWithNeato
# 
# 
##
InstallGlobalFunction( DrawDiagramWithNeato, 
  function( arg )
    local diagram, filename, vertices, edges, longstring, v, e, vertexverbosity, edgeverbosity, arglen;
   
    vertexverbosity:=0; #default orders and nr of elements
    edgeverbosity:=1;   #default shorthand for generalized n-gons
    arglen:=Length(arg);
    ############################# Checking arguments and initializing
    if not(arglen in [2,3,4]) then
      Error("usage DrawDiagram: must have at least 2 and at most 4 arguments.\n");
    elif not(IsDiagram(arg[1]) and IsString(arg[2])) then
      Error("usage DrawDiagram: first argument must be a diagram, second argument must be a filename string.\n");
    fi;

    diagram:=arg[1];
    filename:=arg[2];
    vertices := diagram!.vertices;
    edges := diagram!.edges;  

    if arglen > 2 then
      if IsPosInt(arg[3]) or IsZero(arg[3]) then
        vertexverbosity:=arg[3];
      else
        Error("usage DrawDiagram: third argument must be a vertex verbosity natural number.\n");
      fi;
      if (arglen = 4) then
      	 if (IsPosInt(arg[4]) or IsZero(arg[4])) then
           edgeverbosity:=arg[4];
         else
           Error("Usage DrawDiagram: fourth argument must be an edge verbosity natural number.\n");
	 fi;
      fi;
    fi;
    ##################Done checking!
    longstring := "digraph DIAGRAM{\n rankdir = LR;\n";
    for v in vertices do
      longstring:=Concatenation(longstring, "subgraph cluster", String(v!.type), " {\n");
      longstring:=Concatenation(longstring, "color=transparent\n");    
      longstring:=Concatenation(longstring, "node [shape=circle, width=0.1]; ", String(v!.type), " [label=\"\"];\n");    
      longstring:=Concatenation(longstring, "labelloc=b\n");    
      if vertexverbosity =2 then
        longstring:=Concatenation(longstring, "label=\"", "\";\n}\n"); 
      elif vertexverbosity =1 then
        longstring:=Concatenation(longstring, "label=\"", String(OrderVertex(v)),
                   "\";\n}\n"); 
      else
        longstring:=Concatenation(longstring, "label=\"", String(OrderVertex(v)), "\\n", String(NrElementsVertex(v)),
                   "\";\n}\n"); 
      fi;
    od;

    for e in edges do
      longstring:=Concatenation(longstring, String(e!.edge[1]), " -> ", String(e!.edge[2]));
      if edgeverbosity = 2 then #no label, i.e. basic diagram
        longstring:=Concatenation(longstring, " [label = \"", "\", arrowhead = none ];\n");
      elif edgeverbosity = 1 and Size(Set(ParametersEdge(e)))= 1 then #shorthand generalized n-gons
        longstring:=Concatenation(longstring, " [label = \"", String(ParametersEdge(e)[1]), "\", arrowhead = none ];\n");
      else
        longstring:=Concatenation(longstring, " [label = \"", String(ParametersEdge(e)[2]), " ", 
                  String(ParametersEdge(e)[1]), " ", 
                  String(ParametersEdge(e)[3]), "\", arrowhead = none ];\n");
      fi;
    od;    

    longstring:=Concatenation(longstring, "}\n");    
    PrintTo( Concatenation(filename, ".dot") , longstring );
    ######## Checking the availability of GraphViz dot in path...
    if Whereisdot() then
        Exec( Concatenation("neato -Tps ", filename, ".dot -o ", filename, ".ps") );
    else
        Info(InfoWarning, 1, "Package `FinInG': Only .dot file written for diagram.");
    fi;	
    return;
  end );

####################################
## Draw diagram using ASCII characters, mainly for Display
## Obsolete !
##
###################################

# 
#############################################################################
#F Drawing_Diagram
#  Left over from Bamberg times
# 
##
InstallGlobalFunction( Drawing_Diagram,
  function( verts, edges, way )
    local mat, v, e, v1, v2, c1, c2, posvertices,
          breadth, height, edges2, label, unit;

    ## The unit we use for the character size of an edge
    unit := 3;

    ## First find dimensions of diagram

    breadth := Maximum( List(way, t->t[2]) );
    height := Maximum( List(way, t->t[1]) );
    edges2 := List(edges, t -> [Position(verts, t!.edge[1]), Position(verts, t!.edge[2]), t] );;

    # We now work out where to put the lines,
    # and what type of lines they are  
    # 1: ---
    # 2: |
    # 3: /
    # 4: \

    mat := NullMat( (unit + 1)*(height - 1) + 1, (unit + 1) * (breadth - 1) + 1, Integers);

    ## put vertices in (-1)
    posvertices := List( way, v -> 
           [ (unit + 1) * (v[1] - 1) + 1, (unit + 1) *(v[2] - 1) + 1] );

    for v in posvertices do
      mat[v[1]][v[2]] := -1;
    od;

    ## put lines in

    for e in edges2 do
        v1 := posvertices[ e[1] ];
        v2 := posvertices[ e[2] ];

        ## start at middle of edge

        c1 := (v1[1]+v2[1])/2;
        c2 := (v1[2]+v2[2])/2;
        if HasResidueLabelForEdge( e[3] ) then
           label := ResidueLabelForEdge( e[3] );
           if label = "4" then
              if v1[1] = v2[1] then 
                 mat[c1]{[c2-1,c2,c2+1]} := [ "=", "=", "="];
              elif v1[2] = v2[2] then
                 mat{[c1-1,c1,c1+1]}[c2] := ["||", "||", "||"];
              elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
                 mat[c1+1][c2-1] := "//";
                 mat[c1][c2] := "//";
                 mat[c1-1][c2+1] := "//";
              else
                 mat[c1-1][c2+1] := "\\\\";
                 mat[c1][c2] := "\\\\";
                 mat[c1+1][c2-1] := "\\\\";
              fi;

           elif label = "3" or label = "-" then
              if v1[1] = v2[1] then 
                 mat[c1]{[c2-1,c2,c2+1]} := [ "-", "-", "-"];
              elif v1[2] = v2[2] then
                 mat{[c1-1,c1,c1+1]}[c2] := ["|", "|", "|"];
              elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
                 mat[c1+1][c2-1] := "/";
                 mat[c1][c2] := "/";
                 mat[c1-1][c2+1] := "/";
              else
                 mat[c1-1][c2-1] := "\\";
                 mat[c1][c2] := "\\";
                 mat[c1+1][c2+1] := "\\";
              fi;

           else  
              if v1[1] = v2[1] then 
                mat[c1]{[c2-1,c2,c2+1]} := ["-", label, "-"];
              elif v1[2] = v2[2] then
                mat{[c1-1,c1,c1+1]}[c2] := ["|", label, "|"];
              elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
                mat[c1+1][c2-1] := "/";
                mat[c1][c2] := label;
                mat[c1-1][c2+1] := "/";
              else
                mat[c1-1][c2+1] := "\\";
                mat[c1][c2] := label;
                mat[c1+1][c2-1] := "\\";
              fi;
           fi;
        else
           if v1[1] = v2[1] then 
              mat[c1]{[c2-1,c2,c2+1]} := [1,1,1]; 
           elif v1[2] = v2[2] then
              mat[c1][c2] := 2;
           elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
              mat[c1][c2] := 3;
           else
              mat[c1][c2] := 4;
           fi;
        fi;
    od;
    return mat;
  end ); 

#############################################
## Display diagram using ASCII
##
##
#############################################

InstallMethod( Display, [ IsDiagram and IsDiagramRep ],
  function( diag )
    local way, verts, edges, breadth, height, i, j, mat;
    way := diag!.drawing;
    verts := diag!.vertices;
    edges := diag!.edges;
    mat := Drawing_Diagram( verts, edges, way ); 

    for i in [1..NrRows(mat)] do
      for j in [1..NrCols(mat)] do 
          if mat[i,j] = 0 then Print(" ");
            # if IsOddInt(j) then Print( " " );
            # else Print( "  " );
            # fi;
          elif mat[i,j] = -1 then
             Print( "o" );
          elif mat[i,j] = 1 then
             Print( "-" );
          elif mat[i,j] = 2 then
             Print( "|" );
          elif mat[i,j] = 3 then
             Print( " /" );
          elif mat[i,j] = 4 then
             Print( " \\" );
          else 
             Print( mat[i,j] );
          fi;
      od;
      Print("\n");
  od;
  return; 
  end );

####################################################
## Special diagrams
##
##
####################################################

# 
#############################################################################
#F DiagramOfGeometry ( < projsp > )
# 
# 
##
InstallMethod( DiagramOfGeometry, "for a projective space", [ IsProjectiveSpace ],
  function( geo )
    local vertices, types, x, v, edges, newedges, e, way, diagram, q;

    vertices := [];
    types := TypesOfElementsOfIncidenceStructure( geo );
    q := Size( geo!.basefield );

  ## Wrapping up... 
    for x in types do
      v := rec( type := x );
      Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v);
      SetOrderVertex(v, q);
      Add( vertices, v );
    od;

    edges := List([1..Size(types)-1], i -> vertices{[i,i+1]} );
    newedges := [];

  ## Wrapping up...
    for x in edges do
      e := rec( edge := x );
      Objectify( NewType( EdgeOfDiagramFamily, 
                      IsEdgeOfDiagram and IsEdgeOfDiagramRep ), e);
      SetResidueLabelForEdge( e, "3");
      Add( newedges, e );
    od;

  ## simple path diagram

    way := List([1..Size(types)], i -> [1,i] );
    diagram := rec( vertices := vertices, edges := newedges, drawing := way );;
    Objectify( NewType( DiagramFamily, IsDiagram and IsDiagramRep ), diagram);
    SetGeometryOfDiagram( diagram, geo );
    return diagram;
  end );

# CHECKED 140913
#############################################################################
#F Rk2GeoDiameter ( < cg > , < type >)
# Computes the point (type 1) or line (type 2) diamater of a rank 2 coset 
# geometry.
##
InstallMethod( Rk2GeoDiameter, "for a coset geometry", [IsCosetGeometry,
IsPosInt],
  function( cg, type ) # type in {1,2}
    local parabs, d, g, gpchain;
    if Rank(cg) > 2 then Error("usage Rk2GeoDiameter: this is only for rank 2 geometries.\n"); fi;
    if IsZero(type) or type > 2 then
      Error("usage Rk2GeoDiameter: the only possible types for a rank 2 coset geometry are 1 (points) and 2 (lines).\n");
    fi;
    Print("#I If this takes long, you might want to try Rank2Parameters ...\n"); 
    parabs:=ParabolicSubgroups(cg);
    g:=AmbientGroup(cg);
    d:=0;
    gpchain:=parabs[type];
    while gpchain <> g do
      d:=d+1;
      gpchain:=Set(List(Cartesian(gpchain,parabs[((type + d) mod 2) + 1]), Product));
    od;
    return d;
   end );

# CHECKED 140913
#############################################################################
#F Rk2GeoGonality ( < cg > )
# Return half the girth of the incidence graph of <cg>
##
InstallMethod( Rk2GeoGonality, "for a coset geometry", [IsCosetGeometry],
  function( cg ) 
    local params;
    if Rank(cg) > 2 then Error("usage Rk2GeoGonaloty: this is only for rank 2 geometries.\n"); fi;
    params:= Rank2Parameters(cg);
    return params[1][1];
   end );


# 
#############################################################################
#O GeometryOfRank2Residue ( < rk2res > )
# Returns the geometry component of a rank 2 residue.
# 
##
InstallMethod( GeometryOfRank2Residue, "for a rank 2 residue", [IsRank2Residue],
  function( residue )
    return residue!.residuegeometry;
  end );

# CHECKED 17/03/2014 PhC
#############################################################################
#F Rank2Parameters ( <geo> )
# 
# Computes all parameters of the rank 2 geometry <geo>
# Returns a list of length 3 with
# - [g, dp, dl] as first entry. That is a list with (half) the girth,
# the point- and the line diameter of the geometry.
# - [sp, np] as second entry. That is the point order and the number of points
# - [sl, nl] as third entry. That is the line order and number of lines.
#
# For the moment this only works for a coset geometry geo because 
# IncidenceGraph is only inplemented for that kind of geometries and we also 
# compute the number of elements as indices of parabolic subgroups. 
# 
##
InstallMethod( Rank2Parameters, "for a coset geometry of rank 2", [IsCosetGeometry], 
  function(geo)

local incgr, reps, g, dp, dl, sp, sl, np, nl, locinfo;

#  ***** Check that the input is kosher ******
  if Size(geo!.parabolics)<>2 then
    Error("Usage Rank2Parameters: is ONLY for geometries of rank two!\n");
  fi;

  incgr:=IncidenceGraph(geo);
  reps:=incgr!.representatives; #we assume the GRAPE graph has an element of each type in its 'representative' field. This is indeed the case when the geometry is flag-transitive.
  locinfo:=LocalInfo(incgr,reps[1]); #from GRAPE
  g:=locinfo!.localGirth / 2;
  dp:=locinfo!.localDiameter;
  sl:=locinfo!.localParameters[1][3]-1;
  locinfo:=LocalInfo(incgr,reps[2]);
  dl:=locinfo!.localDiameter;
  sp:=locinfo!.localParameters[1][3]-1;
  np:=IndexNC(geo!.group, geo!.parabolics[1]);
  nl:=IndexNC(geo!.group, geo!.parabolics[2]);
  return[[g,dp,dl], [sp,np], [sl,nl]];
end );

#############################################################################
# Methods for LT for elements of coset geometries.
# Are needed when you make incidence graph since GRAPE wants to 
# order the elements.
#
# CHECKED PhC 110414
#############################################################################

InstallOtherMethod( \<,
  [ IsElementOfCosetGeometry and IsElementOfCosetGeometryRep,
    IsElementOfCosetGeometry and IsElementOfCosetGeometryRep ],
  function( x, y )
  if x!.type = y!.type then
    return x!.obj < y!.obj;
  else
    return x!.type < y!.type;
  fi;
end );

# 
#############################################################################
#F DiagramOfGeometry ( < classpolsp > )
# produces the diagram of a classical polar space.
# 
##
#method to compute the diagram of a cps. Maybe move to another file to make this
#file shorter?

InstallMethod( DiagramOfGeometry, [ IsClassicalPolarSpace ],
  function( geo )
    local types, v, e, way, diagram, s, t, vertices, 
          orders, x, edges, newedges, flavour;
    vertices := [];
    types := TypesOfElementsOfIncidenceStructure( geo );
    s := Size( geo!.basefield );
    for x in [1..Size(types)] do
      v := rec( type := types[x] );
      Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v);
      if x < Size(types) then 
         SetOrderVertex(v, s);
      fi;
      Add( vertices, v );
    od;
    if HasPolarSpaceType( geo ) then
       flavour := PolarSpaceType( geo );       
       if flavour = "symplectic" or flavour = "parabolic" then 
          t := s;
       elif flavour = "elliptic" then
          t := s^2;
       elif flavour = "hyperbolic" then
          t := 1;
       elif flavour = "hermitian" then
          if IsEvenInt( geo!.dimension ) then
             t := Sqrt(s)^3;
          else
             t := Sqrt(s);
          fi;
       fi;
       SetOrderVertex(vertices[Size(types)], t);
    fi;

    edges := List([1..Size(types)-1], i -> vertices{[i,i+1]} );
    newedges := [];
    for x in [1..Size(edges)] do
      e := rec( edge := edges[x] );
      Objectify( NewType( EdgeOfDiagramFamily, 
                      IsEdgeOfDiagram and IsEdgeOfDiagramRep ), e);
      if x < Size(edges) then
          SetResidueLabelForEdge( e, "3");
      else
          SetResidueLabelForEdge( e, "4");
      fi;
      Add( newedges, e );
    od;
    way := List([1..Size(types)], i -> [1,i] );
    diagram := rec( vertices := vertices, edges := newedges, drawing := way );;
    Objectify( NewType( DiagramFamily, IsDiagram and IsDiagramRep ), diagram);
    SetGeometryOfDiagram( diagram, geo );
    return diagram;
  end );



