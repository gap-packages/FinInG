DeclareFilter( "IsEmptySubspace", IsSubspaceOfProjectiveSpace );
DeclareGlobalVariable( "EmptySubspace" );

InstallValue( EmptySubspace, Objectify( NewType( 
      NewFamily( "EmptySubspaceFamily" ), IsEmptySubspace ), rec() ));

SetProjectiveDimension(EmptySubspace, -1);
SetIsTrivial(EmptySubspace, true);

InstallMethod( ViewObj, [IsEmptySubspace],
  function(x)
    Print("< trivial subspace >");
  end );

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
