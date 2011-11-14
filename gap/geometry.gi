#############################################################################
##
##  geometry.gi              FinInG package
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
##  Copyright 2011	Colorado State University, Fort Collins
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

InstallValue( DESARGUES, rec() ); #name of this variable will remind us for ever that this package was called Desargues in earlier days.

#############################################################################
# General methods.
#############################################################################

# CHECKED 18/04/11 jdb
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# general method that just wraps the data (<o>) as an element of a incidence geometry.
# does not do any checking. <geo> and <type> are obviously also arguments. 
# Not for users, not documented. More particular wrap methods can be derived from this.
##
InstallMethod( Wrap, 
	"for a geometry and an object",
	[IsIncidenceGeometry, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( ElementsOfIncidenceStructureFamily, IsElementOfIncidenceStructureRep ), w );
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
	[ IsAllElementsOfIncidenceStructure and IsAllElementsOfIncidenceStructureRep ],
	x -> x!.geometry );

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
#############################################################################
#O  IncidenceStructure( <eles>, <inc>, <typ>, <typeset> )
# method for IncidenceStructure.
##
InstallMethod( IncidenceStructure, 
	"for a set, incidence, type function and type set",
	[ IsList, IsFunction, IsFunction, IsList ],
	function( eles, inc, typ, typeset )
		local geo, ty;
		geo := rec( elements := eles, increl := inc, 
					typefun := typ, typeset := typeset );
		ty := NewType( GeometriesFamily,
					IsIncidenceStructure and IsIncidenceStructureRep );
		ObjectifyWithAttributes( geo, ty, RankAttr, Size(typeset));
		#SetAmbientSpace(geo,geo);
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

