# Viewmethod for the empty subspace

InstallMethod( ViewObj, [IsEmptySubspace],
  function(x)
    Print("< trivial subspace >");
  end );
  

# Methods for IsIncident with the EmtySubspace  

InstallMethod( IsIncident, "for the trivial subspace and a nontrivial subspace", 
   [ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
  function( x, y )
    return true;
  end );

InstallMethod( IsIncident, "for the trivial subspace and a nontrivial subspace", 
   [ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return true;
  end );

InstallMethod( IsIncident, "for the trivial subspace and a projective subspace", 
   [ IsEmptySubspace, IsProjectiveSpace ],
  function( x, y )
    return true;
  end );

InstallMethod( IsIncident, "for the trivial subspace and a projective subspace", 
   [ IsProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return true;
  end );
  
InstallMethod( IsIncident, "for the trivial subspace and the trivial subspace",
	[IsEmptySubspace, IsEmptySubspace],
	function (x,y)
		return true;
	end );
	
# Methods for Span with the EmptySubspace

InstallMethod( Span, "for the trivial subspace and a projective subspace", 
   [ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
  function( x, y )
    return y;
  end );

InstallMethod( Span, "for the trivial subspace and a projective subspace", 
   [ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return x;
  end );

InstallMethod( Span, "for the trivial subspace and a projective subspace", 
   [ IsEmptySubspace, IsProjectiveSpace ],
  function( x, y )
    return y;
  end );

InstallMethod( Span, "for the trivial subspace and a projective subspace", 
   [ IsProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return x;
  end );
  
InstallMethod( Span, "for the trivial subspace and the trivial subspace",
	[IsEmptySubspace, IsEmptySubspace],
	function (x,y)
		return x;
	end );


# Methods for Meet with the EmptySubspace

InstallMethod( Meet, "for the trivial subspace and a projective subspace", 
   [ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
  function( x, y )
    return x;
  end );

InstallMethod( Meet, "for the trivial subspace and a projective subspace", 
   [ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return y;
  end );

InstallMethod( Meet, "for the trivial subspace and a projective subspace", 
   [ IsEmptySubspace, IsProjectiveSpace ],
  function( x, y )
    return x;
  end );

InstallMethod( Meet, "for the trivial subspace and a projective subspace", 
   [ IsProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return y;
  end );

InstallMethod( Meet, "for the trivial subspace and the trivial subspace",
	[IsEmptySubspace, IsEmptySubspace],
	function (x,y)
		return x;
	end );
