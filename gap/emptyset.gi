DeclareFilter( "IsEmptySubspace", IsSubspaceOfProjectiveSpace );
DeclareGlobalVariable( "EmptySubspace" );

InstallValue( EmptySubspace, Objectify( NewType( 
      NewFamily( "EmptySubspaceFamily" ), IsEmptySubspace ), rec() ));

SetProjectiveDimension(EmptySubspace, -1);
SetIsTrivial(EmptySubspace, true);

InstallMethod( ViewObj, [IsEmptySubspace],
  function(x)
    Print("< empty subspace >");
  end );

InstallMethod( IsIncident, "for the empty subspace and a nonempty subspace", 
   [ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
  function( x, y )
    return true;
  end );

InstallMethod( IsIncident, "for the empty subspace and a nonempty subspace", 
   [ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return true;
  end );

InstallMethod( IsIncident, "for the empty subspace and a projective subspace", 
   [ IsEmptySubspace, IsProjectiveSpace ],
  function( x, y )
    return true;
  end );

InstallMethod( IsIncident, "for the empty subspace and a projective subspace", 
   [ IsProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return true;
  end );

InstallMethod( Span, "for the empty subspace and a projective subspace", 
   [ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
  function( x, y )
    return y;
  end );

InstallMethod( Span, "for the empty subspace and a projective subspace", 
   [ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return x;
  end );

InstallMethod( Span, "for the empty subspace and a projective subspace", 
   [ IsEmptySubspace, IsProjectiveSpace ],
  function( x, y )
    return y;
  end );

InstallMethod( Span, "for the empty subspace and a projective subspace", 
   [ IsProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return x;
  end );

InstallMethod( Meet, "for the empty subspace and a projective subspace", 
   [ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
  function( x, y )
    return x;
  end );

InstallMethod( Meet, "for the empty subspace and a projective subspace", 
   [ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return y;
  end );

InstallMethod( Meet, "for the empty subspace and a projective subspace", 
   [ IsEmptySubspace, IsProjectiveSpace ],
  function( x, y )
    return x;
  end );

InstallMethod( Meet, "for the empty subspace and a projective subspace", 
   [ IsProjectiveSpace, IsEmptySubspace ],
  function( x, y )
    return y;
  end );
