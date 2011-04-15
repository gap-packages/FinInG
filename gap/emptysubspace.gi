InstallMethod( EmptySubspace, "for a projective space",
 [IsProjectiveSpace],
 function( pg )
   local  vs,x,w,ty;
	 vs:=UnderlyingVectorSpace(pg);
     x := ShallowCopy(Zero(vs));
	 w := rec( geo := pg, obj := x );
	 ty:= NewType( NewFamily("EmptySubspaceFamily"), IsEmptySubspace and IsEmptySubspaceRep );
    ObjectifyWithAttributes( w, ty, 
						AmbientSpace, pg,
						ProjectiveDimension, -1);
    return w;
end );

# Viewmethod for the empty subspace

InstallMethod( ViewObj, [IsEmptySubspace],
  function(x)
    Print("< trivial subspace >");
  end );
  
InstallMethod( PrintObj, [IsEmptySubspace],
  function(x)
    PrintObj(Zero(UnderlyingVectorSpace(AmbientSpace(x))));
  end );

InstallMethod( Display, [IsEmptySubspace],
  function(x)
    Print("< trivial subspace >");
  end );

# 
InstallMethod( \=, "for two empty subspaces",
        [IsEmptySubspace, IsEmptySubspace],
        function(e1,e2);
        return AmbientSpace(e1) = AmbientSpace(e2);
end );


# Methods for IsIncident with the EmtySubspace  

InstallMethod( IsIncident, "for the trivial subspace and a trivial subspace", 
   [ IsEmptySubspace, IsEmptySubspace ],
  function( x, y )
    return true;
  end );
  
InstallMethod( IsIncident, "for the trivial subspace and a non trivial subspace", 
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

# Methods for \* with the EmtySubspace  

InstallOtherMethod( \*, "for the non-trivial subspace and a trivial subspace",  [IsSubspaceOfProjectiveSpace, IsEmptySubspace],
  function( a, b )
    return IsIncident(b, a);
  end );

InstallOtherMethod( \*, "for the trivial subspace and a nontrivial subspace",  [IsEmptySubspace, IsSubspaceOfProjectiveSpace],
  function( a, b )
    return IsIncident(b, a);
  end );

InstallOtherMethod( \*, "for the projective space and the trivial subspace",  [IsProjectiveSpace, IsEmptySubspace],
  function( a, b )
    return IsIncident(b, a);
  end );

InstallOtherMethod( \*, "for the trivial subspace and a projective space",  [IsEmptySubspace, IsProjectiveSpace],
  function( a, b )
    return IsIncident(b, a);
  end );

InstallOtherMethod( \*, "for the trivial subspace and a projective space",  [IsEmptySubspace, IsEmptySubspace],
  function( a, b )
    return IsIncident(b, a);
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

#InstallMethod( Dimension, "for the empty subspace",
#	[IsEmptySubspace],
#	x->ProjectiveDimension(x)
#	 );
	
