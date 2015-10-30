#from geometry.gd

DeclareFilter( "IsUnwrapper" );
DeclareGlobalVariable( "_" );
InstallValue( _, Objectify( NewType( NewFamily( "UnwrapperFamily" ), IsUnwrapper ), rec() ));

DeclareOperation( "\^", [IsElementOfIncidenceStructure, IsUnwrapper ] );


#from geometry.gi

# CHECKED 17/04/11 jdb
#############################################################################
#O  \^( <e>, <u> )
# unwrapping the element x
## 
InstallMethod(
InstallMethod( \^,
	"for an element of a incidence structure and an unwrapper",
	[IsElementOfIncidenceStructure, IsUnwrapper ],
	function( x, u )
		return x!.obj;
	end );


#from liegeometry.gd
DeclareOperation( "\^", [IsEmptySubspace, IsUnwrapper] );

#from liegeometry.gi

# CHECKED 7/09/2011 jdb
#############################################################################
#O  \^( <e>, <u> )
# unwrapping the empty subspace
## 
InstallMethod( \^, "unwrapping an empty subspace",
  [ IsEmptySubspace, IsUnwrapper ],
  function( e, u )
    return [];
  end );

#from projectivespace.gi

# CHECKED 6/09/11 jdb
#############################################################################
#O  \^( <v>, <u> )
# If the object "v" to be unwrapped is a point of a vector space, then we do not want to use
# return v!.obj, but we want to return a list with one vector, i.e. [v!.obj]
# e.g. if p is a point of a projective space
# gap> p^_; 
# will return a list, with the coordinate vector of the point p
##
InstallMethod( \^,
	"for a subspace of a projective space and an unwrapper",
	[ IsSubspaceOfProjectiveSpace, IsUnwrapper ],
	function( v, u )
		if v!.type = 1 then return [v!.obj];
        else return v!.obj;
		fi;
	end );
