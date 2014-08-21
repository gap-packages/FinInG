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
##  Copyright 2014	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for geometries.
##
#############################################################################

########################################
#
# Things To Do:
#
# - testing
# - Should we change the name of RankAttr? Note
#   that we have installed a method for Rank here which
#   just calls RankAttr anyway...
#   jdb: I left as is. In principle, we could declare Rank as an attribute
#        but maybe this is already a declared attribute, i did not check.
# - Enumerators for subspaces of vectorspaces? This is not present in GAP, and could be
#   used to create "real" Enumerators for projective spaces. To be discussed.
# - check and document operation IncidenceStructure (almost bottom of file).
########################################

Print("loading: geometry\c");

InstallValue( FINING, rec() );

#############################################################################
# General methods.
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
		Objectify( NewType( ElementsOfIncidenceStructureFamily, IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ), w );
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

# CHECKED 17/04/11 jdb
InstallMethod( \^,
	"for an element of a incidence structure and an unwrapper",
	[IsElementOfIncidenceStructure, IsUnwrapper ], 
	function( x, u ) 
		return x!.obj;
	end );

# CHECKED 18/04/11 jdb
#############################################################################
#O  AmbientGeometry( <x> )
# general method that returns the ambient geometry of a particular element.
## 
InstallMethod( AmbientGeometry, 
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], 
	x -> x!.geo );

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

# added 140813 pc
#############################################################################
#O  AmbientGeometry( <x> )
# general method that returns the ambient geometry of a flag
##
InstallMethod( AmbientGeometry,
        [ IsFlagOfIncidenceStructure ],
        x -> x!.geo );


# CHECKED 14/4/2011 jdb
#############################################################################
#O  Type( <x> )
# general method that returns the type of a particular element.
## 
InstallMethod( Type, 
	[ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], 
	x -> x!.type );

# CHECKED 14/4/2011 jdb
#############################################################################
#O  Type( <x> )
# general method that returns the type of a collection of elements of a particular type.
## 
InstallMethod( Type, 
	"for IsElementsOfIncidenceStructure(Rep)",
	[IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep], 
	x -> x!.type );

# Added 140813, pc
#############################################################################
#O  Type( <x> )
# general method that returns the type of a flag
##
InstallMethod( Type,
        "for IsElementsOfIncidenceStructure(Rep)",
        [IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep],
        x -> Set(x!.els,Type) );


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
		if AmbientGeometry(x) <> AmbientGeometry(x) then
			Error("Elements must have the same ambient geometry.");
		fi;
		geo:=AmbientGeometry(x);
                return geo!.increl(x!.obj,y!.obj);
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
        [IsElementOfIncidenceStructure, IsFlagOfIncidenceStructure],
        function( x, f )
		local iter, inc;
		iter:=Iterator(ElementsOfFlag(f));
		inc:=true;
                while inc and not(IsDoneIterator(iter)) do
			inc:=IsIncident(x,NextIterator(iter));
		od;
		return inc;
        end );

# added by pc 180813
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
                test := Set(Flat(List([1..Length(list)-1],i -> 
List([i..Length(list)], j -> IsIncident(list[i], list[j])))));
                if (test <> [ true ] and test <> []) then
                  Error("<els> does not determine a flag>");
                fi;
                flag := rec(geo := incgeo, types := List(list,x->x!.type), 
els := list);
                ObjectifyWithAttributes(flag, IsFlagOfIncidenceStructureType, IsEmptyFlag, false);
                return flag;
        end);


# added by pc 140813
############################################################################
#O  ShadowOfFlag( <F,j> )
# Completely generic shadow computation
# For a flag of an incidence structure and a type j
# Returns all elements of type j which are incident to the flag F
##
InstallMethod( ShadowOfFlag,
        "for a flag of an incidence structure and a type",
        [IsFlagOfIncidenceStructure, IsPosInt],
        function( f, j )
                local iter, shad, x;
		if j in Type(f) then
			return f!.els[Position(Type(f),j)];
		fi;
                iter:=Iterator(ElementsOfIncidenceStructure(f!.geo,j));
                shad:=[];
                for x in iter do
                        if IsIncident(x,f) then Add(shad,x); fi;
                od;
                return shad;
        end );


InstallGlobalFunction( HashFuncForElements,
	function( v, data )
		return data.func(v!.obj,data.data);
	end );

InstallMethod( ChooseHashFunction, 
	"for an element and a hash length",
	[ IsElementOfIncidenceStructure, IsPosInt ],
	function( v, len )
		local data;
		data := ChooseHashFunction( v!.obj, len );
		return rec( func := HashFuncForElements, data := data );
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
        [ IsFlagOfIncidenceStructure and 
IsFlagOfIncidenceStructureRep ],
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

InstallMethod( Display, "for IsVector", [ IsVector ],
	function( v )
		Display( [v] );
	end );

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

######################################################################
##
## The following operations just make "nice" versions of functions for
## specific geometries when the corresponding functions are defined.
## this means: methods for the functions here are generic and relying
## on installed methods for more particular objects.
##
######################################################################

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

# 
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
	
# CHECKED 4/9/2011 jdb
# but maybe to be changed later on. See to do list on top of file.
#############################################################################
#O  Enumerator( <D> )
# generic method that enumerates D, using an Iterator for D
# assuming D belongs to IsElementsOfIncidenceStructure
##
InstallMethod( Enumerator,
	"generic method for IsElementsOfIncidenceStructure",
	[IsElementsOfIncidenceStructure],
	function ( D )
	local  iter, elms;
	iter := Iterator( D );
	elms := [  ];
	while not IsDoneIterator( iter )  do
		Add( elms, NextIterator( iter ) );
	od;
	return elms;
	end);
  
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
		local geo, ty, t, typesetstrings;
		geo := rec( elements := eles, increl := inc, 
					typefun := typ, typeset := typeset );
		ty := NewType( GeometriesFamily,
					IsIncidenceStructure and IsIncidenceStructureRep );
		typesetstrings:=[];
		for t in typeset do
			Add(typesetstrings, Concatenation("element of type ", String(t)));
		od;
		ObjectifyWithAttributes( geo, ty, RankAttr, 
Size(typeset), TypesOfElementsOfIncidenceStructure, typesetstrings);
		#SetAmbientSpace(geo,geo);
		#Objectify(ty,geo);
		return geo;
	end );

# added 140813, pc
#############################################################################
#O  ElementsOfIncidenceStructure( <incstr>, <type> )
# Generic method to return all elements of type <type>
##
InstallMethod( ElementsOfIncidenceStructure,
        "for an incidence structure and a type",
        [ IsIncidenceStructure, IsPosInt ],
        function( incstr, j )
                local elements, wrapped;
		elements:=Filtered(incstr!.elements, e -> 
Position(incstr!.typeset,incstr!.typefun(e))=j);
		wrapped:=List(elements, e -> Wrap(incstr, 
incstr!.typeset[j], e));
		return wrapped;
        end );


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
		eles:=List(typset, j -> ShadowOfFlag(flag,j));

		inc:=function(x,y)
			return IsIncident(x,y);
		end;

                geo := IncidenceStructure(Union(eles), inc, typfun, 
[1..resrank]);
                SetAmbientGeometry(geo,ambigeo);
                #Objectify(ty,geo);
                return geo;
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
    Info(InfoFinInG, 10, "The authors are grateful to Harald Gropp for pointing out clearly the difference between a configuration and a constellation.");
    return true;
    end );
    
InstallMethod( IsConstellation,
    "for an incidence structure",
    [ IsIncidenceStructure],
    function(x)
    Info(InfoFinInG, 10, "The authors are grateful to Harald Gropp for pointing out clearly the difference between a configuration and a constellation.");
    return true;
    end );

