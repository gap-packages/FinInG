#############################################################################
##
##  geometry.gi              Desargues package
##                                                              John Bamberg
## 						                                 		Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
## 			                             				      Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2006 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Implementation stuff for geometries
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
#
########################################


InstallValue( DESARGUES, rec() );

InstallMethod( Wrap, "for a geometry and an object",
  [IsIncidenceGeometry, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily, IsElementOfIncidenceStructureRep ), w );
    return w;
  end );

InstallMethod( Unwrap, "for an element",
  [IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep],
  function( v )
    return v!.obj;
  end );

InstallMethod( \^, [ IsElementOfIncidenceStructure, IsUnwrapper ], 
  function( x, u ) 
    return x!.obj;
  end );


InstallMethod( AmbientGeometry, [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], x -> x!.geo );

InstallMethod( AmbientGeometry, [ IsAllElementsOfIncidenceStructure and IsAllElementsOfIncidenceStructureRep ], x -> x!.geometry );

InstallMethod( Type, [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ], x -> x!.type );

InstallMethod( ViewObj, "for a wrapped element",
  [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ],
  function( v )
    Print("<a ",TypesOfElementsOfIncidenceStructure(v!.geo)[v!.type]," in ");
    ViewObj(v!.geo);
    Print(">");
  end );

InstallMethod( PrintObj, "for a wrapped element",
  [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ],
  function( v )
    Print( v!.obj );
  end );

InstallMethod( Display, "for a wrapped element",
  [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ],
  function( v )
    Display(v!.obj);
  end );

InstallMethod( ViewObj, [ IsAllElementsOfIncidenceStructure ],
  function( vs )
    Print("<Elements of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsAllElementsOfIncidenceStructure ],
  function( vs )
    Print("ElementsOfIncidenceStructure( ",vs!.geometry," )");
  end );

InstallMethod( Rank, "for an incidence structure",
  [IsIncidenceStructure],
  function( i )
    return RankAttr(i);
  end );

InstallMethod( \=, [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure], 
  function( a, b ) 
    return a!.obj = b!.obj; 
  end );

InstallMethod( \<, [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure], 
  function( a, b ) 
    return a!.obj < b!.obj; 
  end );

HashFuncForElements := function( v, data )
  return data.func(v!.obj,data.data);
end;

InstallMethod( ChooseHashFunction, "for an element and a hash length",
  [ IsElementOfIncidenceStructure, IsPosInt ],
  function( v, len )
    local data;
    data := ChooseHashFunction( v!.obj, len );
    return rec( func := HashFuncForElements, data := data );
  end );

######################################################################
##
## The following operations just make "nice" versions of functions for
## specific geometries when the corresponding functions are defined.
##
######################################################################

InstallMethod( ElementsOfIncidenceStructure, [IsIncidenceStructure, IsString],
  function( ps, str )
    local m;
    m := Position(TypesOfElementsOfIncidenceStructurePlural(ps), str);
    if m = fail then
      Error("No such element type!");
    else
      return ElementsOfIncidenceStructure(ps, m);
    fi;
  end);

InstallMethod( ShadowOfElement, [IsIncidenceStructure, IsElementOfIncidenceStructure, IsString],
  function( ps, v, str )
    local m;
    m := Position(TypesOfElementsOfIncidenceStructurePlural(ps), str);
    if m = fail then
      Error("No such element type!");
    else
      return ElementsOfIncidenceStructure(ps, v, m);
    fi;
  end);

InstallMethod( ShadowOfFlag, [IsIncidenceStructure, IsList, IsString],
  function( ps, vs, str )
    local m;
    m := Position(TypesOfElementsOfIncidenceStructurePlural(ps), str);
    if m = fail then
      Error("No such element type!");
    else
      return ElementsOfIncidenceStructure(ps, vs, m);
    fi;
  end);

InstallMethod( Enumerator, [IsElementsOfIncidenceStructure],
  ## This operation simply makes an enumerator out
  ## of an iterator.
  function ( D )
    local  iter, elms;
    iter := Iterator( D );
    elms := [  ];
    while not IsDoneIterator( iter )  do
      Add( elms, NextIterator( iter ) );
    od;
    return elms;
  end);

 ## overload "in" to mean incident

#InstallMethod( \in, "for two elements",  [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure],
#  function( a, b )
#    return IsIncident(b, a) and (a!.type < b!.type); #made a little change here
#  end ); #to let in correspond with set theoretic containment. jdb 8/2/9
  
InstallMethod( \in, "for two elements",  [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure],
  function( a, b )
    return IsIncident(b, a);
  end );

InstallMethod( IncidenceStructure, "for a set, incidence, type function and type set",
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
