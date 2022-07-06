#############################################################################
##
##  geometry.gi              FinInG package
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
##  Implementation stuff for geometries.
##
#############################################################################

#############################################################################
# General methods for incidence structures.
#############################################################################

# CHECKED 14/11/2011 jdb
# 140813 modified by pc : TypesOfIncidenceStructure
#############################################################################
#O  IncidenceStructure( <eles>, <inc>, <typ>, <typeset> )
# method for IncidenceStructure.
##
InstallMethod( IncidenceStructure, 
	"for a set, incidence, type function and type set",
	[ IsList, IsFunction, IsFunction, IsList ],
	function( eles, inc, typ, typeset )
		local geo, ty, t, typesetstrings, typesetstringsplural;
		geo := rec( elements := eles, increl := inc, 
					typefun := typ, typeset := typeset );
		ty := NewType( GeometriesFamily,
					IsIncidenceStructure and IsIncidenceStructureRep );
		typesetstrings:=[];
   		typesetstringsplural:=[];
		for t in typeset do
			Add(typesetstrings, Concatenation("element of type ", String(t)));
   			Add(typesetstringsplural, Concatenation("elements of type ", String(t)));
		od;
		ObjectifyWithAttributes( geo, ty, RankAttr, Size(typeset), TypesOfElementsOfIncidenceStructure, 
            typesetstrings, TypesOfElementsOfIncidenceStructurePlural, typesetstringsplural);
		return geo;
	end );

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Rank( <i> )
# generic method that calls RankAttr to return the rank of a particular 
# incidence structure.
## 
InstallMethod( Rank, 
	"for IsIncidenceStructure", 
	[IsIncidenceStructure], 
	i -> RankAttr(i) );

# added 140901, pc
#############################################################################
#O  IncidenceGraph( <incstr> )
# Generic method for incidence graph, with trivial group
##
InstallMethod( IncidenceGraph,
        "for an incidence structure",
        [ IsIncidenceStructure ],
        function( incstr )
			local all, adj, gamma;
            if IsBound(incstr!.IncidenceGraphAttr) then
                return incstr!.IncidenceGraphAttr;
            fi;
			all:=List([1..Rank(incstr)], i -> AsSet(ElementsOfIncidenceStructure(incstr,i)));
			all:=Union(all);
			adj:=function(i,j)
				if i=j then 
					return false; # Otherwise problem with loops
				else 
					return IsIncident(all[i],all[j]);
				fi;
			end;
			gamma := Graph( Group(()), [1..Size(all)], OnPoints, adj, true);
            Setter( IncidenceGraphAttr )( incstr, gamma );
			return gamma;
        end );

#############################################################################
# Elements of incidence structures.
#############################################################################

######################################################################
##
## The following operations just make "nice" versions of functions for
## specific geometries when the corresponding functions are defined.
## this means: methods for the functions here are generic and relying
## on installed methods for more particular objects.
##
######################################################################

#############################################################################
#O  ElementsOfIncidenceStructure( <inc>, <j> )
# returns the elements of the incidence structure <inc> of type <j>
## 
InstallMethod( ElementsOfIncidenceStructure, 
    "for an incidence structure and a type",
	[IsIncidenceStructure, IsPosInt],
	function( inc, j )
		local r;
		r := Rank(inc);
		if j > r then
			Error("<inc> has no elements of type <j>");
		else
			return Objectify(
			NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep ),
				rec( geometry := inc,
					type := j )
					);
		fi;
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <ps>, <str> )
# returns the elements of an incidence structure <ps> provided there is an
# installed method for ElementsOfIncidenceStructure for the particular <ps>
# and <str> is occuring in TypesOfElementsOfIncidenceStructurePlural(<ps>).
## 
InstallMethod( ElementsOfIncidenceStructure,
	"for an incidence structure and a string",
	[IsIncidenceStructure, IsString],
	function( ps, str )
		local m;
		m := Position(TypesOfElementsOfIncidenceStructurePlural(ps), str);
		if m = fail then
			Error("No such element type!");
		else
		return ElementsOfIncidenceStructure(ps, m);
		fi;
		end);

# added 140813, pc
#############################################################################
#O  Iterator( <vs> )
# Iterator for ElementsOfIncidenceStructure of a given type.
##
InstallMethod( Iterator,
        "for IsElementsOfIncidenceStructure",
        [ IsElementsOfIncidenceStructure ],
        function( vs )
            local incstr, elements, wrapped, type;
            incstr := vs!.geometry;
            type := vs!.type;
            elements:=Filtered(incstr!.elements, e -> Position(incstr!.typeset,incstr!.typefun(e)) = type);
            wrapped:=List(elements, e -> Wrap(incstr, type, e));
            return IteratorList(wrapped);
        end );
 
# CHECKED 4/9/2011 jdb
# but maybe to be changed later on. See to do list on top of file.
#############################################################################
#O  Enumerator( <D> )
# generic method that enumerates D, using an Iterator for D
# assuming D belongs to IsElementsOfIncidenceStructure
##
InstallMethod( Enumerator,
	"generic method for IsElementsOfIncidenceStructure",
	[ IsElementsOfIncidenceStructure ],
	function ( D )
	local  iter, elms;
	iter := Iterator( D );
	elms := [  ];
	while not IsDoneIterator( iter )  do
		Add( elms, NextIterator( iter ) );
	od;
	return elms;
	end);

#############################################################################
#O  NrElementsOfIncidenceStructure( <ps>, <str> )
# returns the number of elements of an incidence structure <ps> provided there 
# is an  installed method for ElementsOfIncidenceStructure for the 
# particular <ps> and <str> is occuring in 
# TypesOfElementsOfIncidenceStructurePlural(<ps>). 
## 
InstallMethod( NrElementsOfIncidenceStructure,
	"for an incidence structure and a string",
	[IsIncidenceStructure, IsString],
	function( ps, str )
		return Size(ElementsOfIncidenceStructure(ps, str));
		end);

#############################################################################
#O  NrElementsOfIncidenceStructure( <ps>, <typ> )
# returns the number of elements of an incidence structure <ps> provided there 
# is an  installed method for ElementsOfIncidenceStructure for the 
# particular <ps>.
## 
InstallMethod( NrElementsOfIncidenceStructure,
	"for an incidence structure and a type",
	[IsIncidenceStructure, IsPosInt],
	function( ps, typ )
		return Size(ElementsOfIncidenceStructure(ps, typ));
		end);

#############################################################################
# Hash functions for elements are important to use the orb package to compute
# orbits (quickly). We implement hash functions for elements and sets of elements.
# Note that the hash functions for elements rely on the hash functions for the underlying
# object. For cvec/cmats, such hash functions are provided in the package cvec of course
#
# The idea for a hashfunction of a set of elements is as follows: if set has length n,
# then we add all hashvalues of the objects in set. The initial length is estimated
# through the orb functionality. So if set has n elements, the resulting value might
# increase the size of a hash table for the individual elements. Therefore we call
# the hash function for the elements with length len/n.
# Note that more clever ideas might lead to faster orbit computation.
##
InstallGlobalFunction( HashFuncForElements,
	function( v, data )
		return data.func(v!.obj,data.data);
	end );

InstallGlobalFunction( HashFuncForSetElements,
	function( v, data )
        return Sum(List(v,x->data.func(x!.obj,data.data)));
	end );

InstallMethod( ChooseHashFunction,
	"for an element and a hash length",
	[ IsElementOfIncidenceStructure, IsPosInt ],
	function( v, len )
		local data;
		data := ChooseHashFunction( v!.obj, len );
		return rec( func := HashFuncForElements, data := data );
	end );

InstallMethod( ChooseHashFunction, 
	"for a set of elements and a hash length",
	[ CategoryCollections(IsElementOfIncidenceStructure), IsPosInt ],
	function( set, len )
		local v,data;
        v := set[1];
        data := ChooseHashFunction( v!.obj, Int(len/Length(set)) );
		return rec( func := HashFuncForSetElements, data := data );
	end );

#############################################################################
# General methods for collection of elements of incidence structures.
#############################################################################

# CHECKED 18/04/11 jdb
#############################################################################
#O  AmbientGeometry( <x> )
# general method that returns the ambient geometry of a collection of 
# elements of a particular type.
# notice the small difference with the previous method :-)
## 
InstallMethod( AmbientGeometry, 
	[ IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep ], 
	x -> x!.geometry );

# CHECKED 18/04/11 jdb
#############################################################################
#O  AmbientGeometry( <x> )
# general method that returns the ambient geometry of a collection of elements
## 
InstallMethod( AmbientGeometry, 
	[ IsAllElementsOfIncidenceStructure ],
	x -> x!.geometry );

# CHECKED 14/4/2011 jdb
#############################################################################
#O  Type( <x> )
# general method that returns the type of a collection of elements of a particular type.
## 
InstallMethod( Type, 
	"for IsElementsOfIncidenceStructure(Rep)",
	[IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep], 
	x -> x!.type );

#############################################################################
# General methods for particular elements of incidence structures.
#############################################################################

# CHECKED 18/04/11 jdb
# 140813 pc Changed filter for geo to IsIncidenceStructure (was 
# IsIncidenceGeometry)
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# general method that just wraps the data (<o>) as an element of a incidence geometry.
# does not do any checking. <geo> and <type> are obviously also arguments. 
# Not for users, not documented. More particular wrap methods can be derived from this.
##
InstallMethod( Wrap, 
	"for a geometry and an object",
	[IsIncidenceStructure, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( ElementsOfIncidenceStructureFamily, 
			IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ), w );
		return w;	
	end );

# CHECKED 17/04/11 jdb
#############################################################################
#O  Unwrap( <v> )
# general method that returns the data of an element <v>. 
# Not for users, not documented.
##
InstallMethod( Unwrap, 
	"for an element",
	[IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep],
	function( v )
		return v!.obj;
	end );

#jdb 30/10/15: This method is not used anymore after commenting out the Unwrapper stuff in geometry.gd
#see the comment there.
# CHECKED 17/04/11 jdb
#InstallMethod( \^,
#	"for an element of a incidence structure and an unwrapper",
#	[IsElementOfIncidenceStructure, IsUnwrapper ],
#	function( x, u )
#		return x!.obj;
#	end );

# added 3/9/14 jdb
#############################################################################
#O  UnderlyingObject( <x> )
# general method that returns the ambient geometry of a particular element.
## 
InstallMethod( UnderlyingObject,
    "for an element of an incidence structure",
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], 
	x -> x!.obj );

# added 3/9/14 jdb
#############################################################################
#O  ObjectToElement( <inc>, <type>, <obj> )
# general method that returns the element of type represtented by obj.
## 
InstallMethod( ObjectToElement,
    "for an incidence structure, a type, and an object",
	[ IsIncidenceStructure, IsPosInt, IsObject ],
	function(inc,t,obj)
        local typeset;
        typeset := inc!.typeset;
        if not t in typeset then
            Error("<t> is not a type of elements of <inc>");
        elif not obj in inc!.elements then
            Error("<t> does not represent any element of <inc>");
        elif not inc!.typefun(obj) = t then
            Error("<obj> does not represent an element of type <t>");
        else
            return Wrap(inc,t,obj);
        fi;
    end );

# CHECKED 18/04/11 jdb
#############################################################################
#O  AmbientGeometry( <x> )
# general method that returns the ambient geometry of a particular element.
## 
InstallMethod( AmbientGeometry, 
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], 
	x -> x!.geo );

# CHECKED 27/6/2011 jdb and pc
#############################################################################
#O  Intersection2( <el1>, <el2> )
# method for Intersection2, referring to Meet.
##
InstallOtherMethod( Intersection2,
	"for two elements of an incidence structure, relying on Meet",
	[IsElementOfIncidenceStructure, IsElementOfIncidenceStructure],
	function( el1, el2 )
		return Meet(el1,el2);
	end);

# CHECKED 14/4/2011 jdb
#############################################################################
#O  Type( <x> )
# general method that returns the type of a particular element.
## 
InstallMethod( Type, 
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], 
	x -> x!.type );

# CHECKED 18/4/2011 jdb
#############################################################################
#O  \=( <a>, <b> )
# test equality method for elements of an incidence structure
# relies to testing equality of underlying 'obj' fields of the arguments.
## 
InstallMethod( \=, 
	"for two IsElementOfIncidenceStructure",	
	[IsElementOfIncidenceStructure, IsElementOfIncidenceStructure], 
	function( a, b ) 
		return a!.obj = b!.obj; 
	end );

# CHECKED 18/4/2011 jdb, but also changed 18/4/2011. Volunteers to check again?
#############################################################################
#O  \<( <a>, <b> )
# LT for two elements of an incidence structure.
# first compares types of elements, in case of equality, 
# relies to comparing underlying 'obj' fields of the arguments.
## 
InstallMethod( \<, "for two IsElementOfIncidenceStructure",
	[IsElementOfIncidenceStructure, IsElementOfIncidenceStructure], 
	function( a, b ) 
		if a!.type = b!.type then
			return a!.obj < b!.obj;
		else
			return a!.type < b!.type;
		fi;
	end );

# CHECKED 17/04/11 jdb
#############################################################################
#O  \*( <a,b> )
# Alternative for IsIncident.
# Remind that IsIncident is an operation declared for elements of any incidence
# structure.
##
InstallOtherMethod( \*, 
	"for two elements of an incidence structure",
	[IsElementOfIncidenceStructure, IsElementOfIncidenceStructure],
	function( a, b )
		return IsIncident(b, a);
	end );

# added by pc 140813
############################################################################
#O  IsIncident( <x,y> )
# For two elements of an incidence structure, generic.
##
InstallMethod( IsIncident,
        "for two elements of an incidence structure",
        [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure],
        function( x, y )
		local geo;
		if AmbientGeometry(x) <> AmbientGeometry(y) then
			Error("Elements must have the same ambient geometry.");
		fi;
		geo:=AmbientGeometry(x);
                return geo!.increl(x!.obj,y!.obj);
        end );

#############################################################################
# General methods for flags of incidence structures.
#############################################################################

# added by pc 140813
# changed by jdb 161214 (added check after test).
#############################################################################
#O  FlagOfIncidenceStructure( <incgeo>, <els> )
# returns the flag of the incidence structure <incgeo> with elements in 
# the list <els>.
# the method checks whether the input really determines a flag.
##
InstallMethod( FlagOfIncidenceStructure,
        "for an incidence structure and a list of elements",
        [ IsIncidenceStructure, IsElementOfIncidenceStructureCollection ],
        function(incgeo,els)
                local list,i,test,type,flag;
                list := Set(ShallowCopy(els));
                if Length(list) > Rank(incgeo) then
                    Error("A flag can contain at most Rank(<incstr>) elements");
                fi;
                test := List(list,x->AmbientGeometry(x));
                if not ForAll(test,x->x=incgeo) then
                    Error("not all elements have <incgeo> as ambient geometry");
                fi;
                #if test[1] <> incgeo then
                #    Error("<els> is not a list of elements of <incgeo>");
                #fi;
                test := Set(Flat(List([1..Length(list)-1],i ->
                    List([i..Length(list)], j -> IsIncident(list[i], list[j])))));
                if (test <> [ true ] and test <> []) then
                  Error("<els> does not determine a flag>");
                fi;
                flag := rec(geo := incgeo, types := List(list,x->x!.type), 
                        els := list);
                ObjectifyWithAttributes(flag, 
                    IsFlagOfIncidenceStructureType, IsEmptyFlag, false, RankAttr,
                        Size(list));
                return flag;
        end);

# added by pc 140901
#############################################################################
#O  FlagOfIncidenceStructure( <incgeo>, <els> )
# returns the empty flag of incidence sturcture <incgeo>.
##
InstallMethod( FlagOfIncidenceStructure,
        "for an incidence structure and an empty list",
        [ IsIncidenceStructure, IsList and IsEmpty ],
        function(incgeo,els)
                local flag;
                flag := rec(geo := incgeo, types := [], els := []);
                ObjectifyWithAttributes(flag, 
                    IsFlagOfIncidenceStructureType, IsEmptyFlag, true, RankAttr, 0);
                return flag;
        end);

# CHECKED 18/4/2011 jdb
#############################################################################
#A  IsChamberOfIncidenceStructure( <flag> )
# returns true if <flag> is a chamber.
# remind that flags of projective spaces, polar spaces are constructed in 
# IsFlagOfIncidenceStructureRep, which makes this method generic
##
InstallMethod( IsChamberOfIncidenceStructure,
	"for a flag of an incidence structure",
	[ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep ],
	flag -> Length(flag!.types) = Rank(flag!.geo)
	);

# added 140813 pc
#############################################################################
#O  AmbientGeometry( <x> )
# general method that returns the ambient geometry of a flag
##
InstallMethod( AmbientGeometry,
        [ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep],
        x -> x!.geo );

# added by pc, moved to here 31/3/14
#############################################################################
#O  ElementsOfFlag( <flag> )
# returns elements of a flag 
# 
##
InstallMethod( ElementsOfFlag, 
	"for an incidence structure flag",
	[ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep ],
	function(flag)
		return flag!.els;
	end );

# added by pc, 140813
#############################################################################
#O  Size( <flag> )
# returns size of a flag
#
##
InstallMethod( Size,
        "for an incidence structure flag",
        [ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep ],
        function(flag)
                return Size(ElementsOfFlag(flag));
        end );

# added by pc, 140901
#############################################################################
#O  Rank( <flag> )
# returns rank of a flag
#
##
InstallMethod( Rank,
        "for IsFlagOfIncidenceStructure",
        [ IsFlagOfIncidenceStructure ],
        i -> RankAttr(i) );

# Added 140813, pc
#############################################################################
#O  Type( <x> )
# general method that returns the type of a flag
##
InstallMethod( Type,
        "for IsElementsOfIncidenceStructure(Rep)",
        [ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep ],
        x -> Set(x!.els,Type) );

# added 140813, pc
#############################################################################
#O  ResidueOfFlag( <flag> )
# completely generic residue computation
##
InstallMethod( ResidueOfFlag,
        "for a flag",
        [ IsFlagOfIncidenceStructure ],
        function( flag )
                local ambigeo, geo, typfun, typset, resrank, eles, inc;
		ambigeo:=AmbientGeometry(flag);
		typset:=Difference([1..Rank(ambigeo)], Type(flag));
		resrank:=Size(typset);
		typfun:=function(ele)
			return Position(typset, Type(ele));
		end;
		eles:=List(typset, j -> List(ShadowOfFlag(ambigeo,flag,j)));
		inc:=function(x,y)
			return IsIncident(x,y);
		end;
        geo := IncidenceStructure(Union(eles), inc, typfun, [1..resrank]);
		SetAmbientGeometry(geo,ambigeo);
        return geo;
	end );

#############################################################################
# methods that allow us to make an ordered Set from a list of flags,
# and to make an ordered Set of a list of elements and flags.
#############################################################################

# added 2/9/14 pc+jdb
#############################################################################
#O  \=( <a>, <b> )
# test equality method for flags of an incidence structure
# relies to testing equality of underlying 'obj' fields of the arguments.
## 
InstallMethod( \=, 
	"for two IsElementOfIncidenceStructure",	
	[ IsFlagOfIncidenceStructure, IsFlagOfIncidenceStructure ],
	function( a, b )
		return Set(a!.els) = Set(b!.els);
	end );

# added 2/9/14 pc+jdb
#############################################################################
#O  \<( <a>, <b> )
# LT for two flags of an incidence structure.
# first compares types of elements, in case of equality, 
# relies to comparing underlying 'obj' fields of the arguments.
## 
InstallMethod( \<, "for two IsFlagOfIncidenceStructure",
	[ IsFlagOfIncidenceStructure, IsFlagOfIncidenceStructure ],
	function( a, b ) 
		if a!.types = b!.types then
			return a!.els < b!.els;
		else
			return a!.types < b!.types;
		fi;
	end );

# added 2/9/14 pc+jdb
#############################################################################
#O  \<( <a>, <b> )
# LT for a flag and an element.
# first compares types of elements, in case of equality, 
# relies to comparing underlying 'obj' fields of the arguments.
## 
InstallMethod( \<, "for two IsElementOfIncidenceStructure",
	[ IsFlagOfIncidenceStructure, IsElementOfIncidenceStructure ],
	function( a, b ) 
        if a!.geo <> b!.geo then
            Error("<flag> and <element> do not belong to the same geometry");
        fi;
        return false;
    end );

# added 2/9/14 pc+jdb
#############################################################################
#O  \<( <a>, <b> )
# LT for two flags of an incidence structure.
# first compares types of elements, in case of equality, 
# relies to comparing underlying 'obj' fields of the arguments.
## 
InstallMethod( \<, "for two IsElementOfIncidenceStructure",
	[ IsElementOfIncidenceStructure, IsFlagOfIncidenceStructure ],
	function( a, b )
        if a!.geo <> b!.geo then
            Error("<element> and <flag> do not belong to the same geometry");
        fi;
        return true;
    end );

# added by pc 140813
############################################################################
#O  IsIncident( <x,F> )
# For an element and a flag of an incidence structure
# An element is incident with a flag if it is incident with all elements 
# of the flag.
##
InstallMethod( IsIncident,
        "for an element and a flag of an incidence structure",
        [ IsElementOfIncidenceStructure, IsFlagOfIncidenceStructure ],
        function( x, f )
		local iter, inc;
		iter:=Iterator(ElementsOfFlag(f));
		inc:=true;
                while inc and not(IsDoneIterator(iter)) do
			inc:=IsIncident(x,NextIterator(iter));
		od;
		return inc;
        end );
        
# added by pc 140813
############################################################################
#O  IsIncident( <x,F> )
# For an element and a flag of an incidence structure
# An element is incident with a flag if it is incident with all elements 
# of the flag.
##
InstallMethod( IsIncident,
        "for an element and a flag of an incidence structure",
        [IsFlagOfIncidenceStructure, IsElementOfIncidenceStructure],
        function( f, x )
		local iter, inc;
		iter:=Iterator(ElementsOfFlag(f));
		inc:=true;
                while inc and not(IsDoneIterator(iter)) do
			inc:=IsIncident(x,NextIterator(iter));
		od;
		return inc;
        end );

#############################################################################
#O  \in( <x>, <y> )
# x: an element of an incidence structure, y a flag of that incidence structure
##
InstallOtherMethod( \in, 
	"for an element and a flag of an incidence structure",
	[ IsElementOfIncidenceStructure, IsFlagOfIncidenceStructure ],
	function( x, y )
		if x!.geo <> y!.geo then
			Error( "<x> and <y> have not the same ambient geometry" );
		else
            return x in y!.els;
        fi;
	end );

# CHECKED 11/09/11 jdb
# this is a method that is generic for more geometries than projective spaces, so
# this was moved from projectivespace.gi to geometry.gi on 5/4/2018.
#############################################################################
#O  \in( <x>, <dom> )
# returns true if <x> belongs to the elements collected in <dom> It is checked if their
# geometry matches.
##
InstallMethod( \in,
    "for an element and domain",
    # 1*SUM_FLAGS+3 increases the ranking for this method
    # 5/4/2018: jdb wonders if the above line is necessary.
    [IsElementOfIncidenceStructure, IsAllElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
    function( x, dom )
        return x in dom!.geometry;
    end );

# CHECKED 11/09/11 jdb
# this was moved from projectivespace.gi to geometry.gi on 5/4/2018.
#############################################################################
#O  \in( <x>, <dom> )
# returns true if <x> belongs to the elements collected in <dom> It is checked if their
# geometry matches.
##
InstallMethod( \in,
    "for an element and set of elements of an incidence structure",
    # 1*SUM_FLAGS+3 increases the ranking for this method
    # 5/4/2018: jdb wonders if the above line is necessary.
    [IsElementOfIncidenceStructure, IsElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
    function( x, dom )
        return x in dom!.geometry and x!.type = dom!.type;
    end );

# new since 5/4/2018 jdb
# this was necessary, since the above method causes prblems when dom is
# IsShadowElementsOf...
# this new method is generic. Since e.g. IsShadowSubspacesOfProjectiveSpace
# is not a subcategory of IsShadowElementsOfIncidenceStructure, we have
# to create an operation for Lie geometries (and probably others) as well.
#############################################################################
#O  \in( <x>, <dom> )
# returns true if <x> belongs to the elements collected in <dom> It is checked if their
# geometry matches.
##
#InstallMethod( \in,
#    "for an element and collection of shadow elements of an incidence structure",
    # 1*SUM_FLAGS+3 increases the ranking for this method
    # 5/4/2018: jdb wonders if the above line is necessary.
#    [IsElementOfIncidenceStructure, IsShadowElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
#    function( x, dom )
#        return x in dom!.geometry and x!.type = dom!.type and IsIncident(x,dom!.flag); #there is no representation declared for IsShadowElementsOfIncidenceStructure, so we have no idea whether a flag is available.
#    end );

#############################################################################
# General methods for shadows of elements/flags
#############################################################################

# added by pc 140901
############################################################################
#O  ShadowOfElement( <inc>, <f> ,<j> )
# Completely generic shadow computation
# For an element of an incidence structure and a type j
# Returns all elements of type j which are incident to e
##
InstallMethod( ShadowOfElement,
        "for an element of an incidence structure and a type",
        [IsIncidenceStructure, IsElementOfIncidenceStructure, IsPosInt],
        function( inc, f, j )
            local iter, shad, x;
            return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and 
                                    IsShadowElementsOfIncidenceStructure),
                rec( geometry := inc,
                     type := j,
                     parentflag := FlagOfIncidenceStructure(inc,[f]) )
                );
        end );

# CHECKED 18/4/2011 jdb
#############################################################################
#O  ShadowOfElement( <ps>, <v>, <str> )
# returns the shadow elements of type <str> in element <v> in the inc str <ps>
# relies on installed method for ShadowOfElement for the particular <ps>
# and <str> is occuring in TypesOfElementsOfIncidenceStructurePlural(<ps>).
##
InstallMethod( ShadowOfElement,
	"for an incidence structure, an element of, and a string",
	[IsIncidenceStructure, IsElementOfIncidenceStructure, IsString],
	function( ps, v, str )
		local m;
		m := Position(TypesOfElementsOfIncidenceStructurePlural(ps), str);
		if m = fail then
			Error("No such element type!");
		else
			return ShadowOfElement(ps, v, m);
		fi;
		end);

# CHECKED 19/4/2011 jdb
# moved to geometry.gi and made generic 3/9/14
#############################################################################
#O  ElementsIncidentWithElementOfIncidenceStructure( <el>, <i> )
# returns the elements of type <i> in <el>, relying on ShadowOfElement 
# for particular <el>.
## 
InstallMethod( ElementsIncidentWithElementOfIncidenceStructure, "for IsElementOfLieGeometry",
	[ IsElementOfIncidenceStructure, IsPosInt],
	function( el, i )
		return ShadowOfElement(el!.geo, el, i);
	end );

# added by pc 140813
############################################################################
#O  ShadowOfFlag( <F,j> )
# Completely generic shadow computation
# For a flag of an incidence structure and a type j
# Returns all elements of type j which are incident to the flag F
##
InstallMethod( ShadowOfFlag,
        "for an incidence structure, flag of an incidence structure and a type",
        [IsIncidenceStructure, IsFlagOfIncidenceStructure, IsPosInt],
        function( inc, f, j )
            local iter, shad, x;
            return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and 
                                    IsShadowElementsOfIncidenceStructure),
                rec( geometry := inc,
                     type := j,
                     parentflag := f )
                );
        end );

# CHECKED 18/4/2011 jdb
#############################################################################
#O  ShadowOfFlag( <ps>, <v>, <str> )
# returns the shadow elements of <flag>, i.e. the elements of <ps> of type <j> 
# incident with all elements of <flag>.
# relies on installed method for ShadowOfElement for the particular <ps>
# and <str> is occuring in TypesOfElementsOfIncidenceStructurePlural(<ps>).
##
InstallMethod( ShadowOfFlag,
	"for an incidence structure, a flag, and a string",
	[IsIncidenceStructure, IsFlagOfIncidenceStructure, IsString],
	function( ps, vs, str )
		local m;
		m := Position(TypesOfElementsOfIncidenceStructurePlural(ps), str);
		if m = fail then
			Error("No such element type!");
		else
			return ShadowOfFlag(ps, vs, m);
		fi;
	end);

# CHECKED 27/4/2011 jdb
#############################################################################
#O  ShadowOfFlag( <ps>, <list>, <j> )
# returns the shadow elements of the flag <list>, provided <list> determines
# a flag in <ps>. Relies completely on FlagOfIncidenceStructure for <ps>,
# so this implementation is generic.
##
InstallMethod( ShadowOfFlag,
	"for an incidence structure, a list, and an integer",
	[IsIncidenceStructure, IsList, IsPosInt],
	function( ps, list, j )
		return ShadowOfFlag(ps,FlagOfIncidenceStructure(ps,list),j);
	end);

# CHECKED 27/4/2011 jdb
#############################################################################
#O  ShadowOfFlag( <ps>, <list>, <str> )
# returns the shadow elements of the flag <list>, provided <list> determines
# a flag in <ps>. Relies completely on FlagOfIncidenceStructure for <ps>,
# and on ShadowOfFlag for <ps> <v> and <str>, so this implementation is generic.
##
InstallMethod( ShadowOfFlag,
	"for an incidence structure, a list, and an integer",
	[IsIncidenceStructure, IsList, IsString],
	function( ps, list, str )
		return ShadowOfFlag(ps,FlagOfIncidenceStructure(ps,list),str);
	end);

# added by pc 140901
#############################################################################
#O  Iterator( <vs>) 
# returns an iterator for <vs>, a collection of shadow elemenst of an incidence structure
##
InstallMethod( Iterator, 
	"for shadow elements of an incidence structure",
	[ IsShadowElementsOfIncidenceStructure ],
	function( vs )
        local j, f, shad, x, iter;
        f := vs!.parentflag;
        j := vs!.type;
		if j in Type(f) then
			return IteratorList([f!.els[Position(Type(f),j)]]);
		fi;
        iter:=Iterator(ElementsOfIncidenceStructure(vs!.geometry,j));
        shad:=[];
            for x in iter do
                    if IsIncident(x,f) then Add(shad,x); fi;
            od;
        return IteratorList(shad);
    end );

#############################################################################
# Viewing/Printing/Displaying methods.
#############################################################################
# 5 CHECKED 4/09/11 jdb

InstallMethod( ViewObj, 
	"for IsElementOfIncidenceStructure",
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ],
	function( v )
		Print("<a ",TypesOfElementsOfIncidenceStructure(v!.geo)[v!.type]," in ");
		ViewObj(v!.geo);
		Print(">");
	end );

InstallMethod( ViewObj,
        "for IsFlagOfIncidenceStructure",
        [ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep ],
        function( v )
                Print("<a flag of ");
                ViewObj(v!.geo);
                Print(">");
        end );

InstallMethod( PrintObj, 
	"for IsElementOfIncidenceStructure",
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ],
	function( v )
		Print( v!.obj );
	end );

InstallMethod( Display, 
	"for IsElementOfIncidenceStructure",
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ],
	function( v )
		Display(v!.obj);
	end );

# JB (31/01/2013): One thing that I would like in GAP is to have Display for vectors just
# like that for matrices. So here is a method which makes this happen
# JDB: I agree, but this causes problems, see remark of Alexander of 8/12/14.
#InstallMethod( Display,
#	"for IsVector",
#	[ IsVector ],
#	function( v )
#		Display( [v] );
#	end );

InstallMethod( ViewObj, 
	"for IsAllElementsOfIncidenceStructure",
	[ IsAllElementsOfIncidenceStructure ],
	function( vs )
		Print("<Elements of ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

InstallMethod( PrintObj, 
	"for IsAllElementsOfIncidenceStructure",
	[ IsAllElementsOfIncidenceStructure ],
	function( vs )
		Print("ElementsOfIncidenceStructure( ",vs!.geometry," )");
	end );

# Finally a generic ViewObj method for shadow elements.
#############################################################################
InstallMethod( ViewObj,
	"for shadow elements of a Lie geometry", 
	[ IsShadowElementsOfIncidenceStructure ],
	function( vs )
		Print("<shadow elements of type ", vs!.type," in ");
		ViewObj(vs!.geometry);
		Print(">");
	end );
  
InstallMethod( ViewObj, 
	"for IsElementsOfIncidenceStructure",
	[ IsElementsOfIncidenceStructure ],
	function( vs )
		Print("<Elements of type ", vs!.type," of ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

InstallMethod( PrintObj, 
	"for IsElementsOfIncidenceStructure",
	[ IsElementsOfIncidenceStructure ],
	function( vs )
		Print("ElementsOfIncidenceStructure( ",vs!.geometry," ) of type ",vs!.type);
	end );

#############################################################################
# Display methods:
#############################################################################

InstallMethod( ViewObj,
	"for an incidence structure",
	[ IsIncidenceStructure ],
	function(x)
		Print("Incidence structure of rank ", RankAttr(x));
	end );

InstallMethod( PrintObj,
	"for an incidence structure",
	[ IsIncidenceStructure ],
	function(x)
		Print("Incidence structure of rank ", RankAttr(x)," with elements of type ",x!.typeset);
	end );

InstallMethod( Display,
	"for an incidence structure",
	[ IsIncidenceStructure ],
	function(x)
		Print("Incidence structure of rank ", RankAttr(x)," with elements of type ",x!.typeset);
	end );

#############################################################################
# methods for the two general properties
# We are gratefull to Harald Gropp for pointing out the difference clearly 
# between a configuration and a constellation during the conference 
# Combinatorics 2014.
#############################################################################

InstallMethod( IsConfiguration,
    "for an incidence structure",
    [ IsIncidenceStructure],
    function(x)
    Info(InfoFinInG, 2, "The authors are grateful to Harald Gropp for pointing out clearly the difference between a configuration and a constellation.");
    return true;
    end );
    
InstallMethod( IsConstellation,
    "for an incidence structure",
    [ IsIncidenceStructure],
    function(x)
    Info(InfoFinInG, 2, "The authors are grateful to Harald Gropp for pointing out clearly the difference between a configuration and a constellation.");
    return true;
    end );

