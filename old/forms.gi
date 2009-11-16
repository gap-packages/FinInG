#############################################################################
##
##  forms.gi              Desargues package
##                                                              John Bamberg
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                             Sven Reichard
##
##  Copyright 2006 University of Western Australia
##                 Lehrstuhl D fuer Mathematik
##
##  Implementation stuff for sesquilinear and quadratic 
##  forms in a polar space.
##
#############################################################################

#############################################################################
# Constructor methods:
#############################################################################

InstallMethod( MakeBlockHyperbolicPair,
  [ IsMatrix and IsFFECollColl, IsField, IsPosInt ],

  function( m, f, i)
    local c1, c2, c, pos, id, d, o, pairs, v, j;
    d := Size(m);
    o := Zero(f);
    id := IdentityMat(d, f);
    pairs := [];
    v := Difference(f^d, [List([1..d], j -> o)]);

    for pos in [1..i] do
        c1 := id[2*i-1];
        while c1*m*c1 <> o or ForAny(pairs, x-> c1*m*x <> o) do
          c1 := Random(v);
        od; 
      
      ## Find t.i. c2 which is linearly indepent to c1

        c2 := Random(v);
        while c2*m*c2 <> o or Rank([c1,c2]) < 2 or c1*m*c2 = o 
              or ForAny(pairs, x-> c2*m*x <> o) do
              c2 := Random(v);
        od;

      ## normalise 
        c2 := c2/(c1*m*c2);
        pairs := Concatenation(pairs, [c1,c2]);
    od;

    return pairs;
  end );

InstallMethod( WittIndex, [IsSesquilinearForm], 
  function( m )
    local type, d, i;
    type := m!.type;
    d := Size(m!.matrix);
    if type = "symplectic" or 
      type = "hyperbolic" or 
      (type = "hermitian" and IsEvenInt(d))
          then i := d/2;
    elif type = "parabolic" or 
        (type = "hermitian" and IsOddInt(d)) 
            then i := (d-1)/2;
    else i := d/2 - 1; 
    fi;
    return i;
  end);


InstallMethod( ChangeFormToCanonicalModuloGerm, [IsSesquilinearForm],

  function( m )
    return MakeBlockHyperbolicPair(m!.matrix, m!.basefield, WittIndex(m)); 
  end );

InstallMethod( ChangeFormToCanonical, [IsSesquilinearForm],

## This operation returns an isometry g such that g m g^T is
## the form arising from the block diagonal matrix
## with each block equal to J=[[0,1],[+-1,0]]. 

## this works modular the germ for the moment

  function( m )
    local type, d, basechange, germsize, c, v, o, mm, f;

    type := m!.type;
    d := Size(m!.matrix);
    f := m!.basefield;

    if IsEvenInt(d) and WittIndex = d/2 then 
      basechange :=  ChangeFormToCanonicalModuloGerm(m);
    elif IsOddInt(d) then 
      basechange :=  ChangeFormToCanonicalModuloGerm(m);

      ## Here we find an extra vector which is orthogonal to
      ## the hyperbolic pairs we have already obtained. Note
      ## that we are only changing form up to similarity.
      
      o := Zero(f);
      v := f^d;
      c := Random( v );
      mm := m!.matrix;
      while c * mm * c = o or ForAny(basechange, x -> c * mm * x <> o) do
        c:= Random( v );
      od;
      Add(basechange, c);
    else 

      ## In this case, we need to find two vectors

      basechange :=  ChangeFormToCanonicalModuloGerm(m);       
      o := Zero(f);
      v := f^d;
    #  c := Random( v ); 
    #  while ForAny(basechange, x -> c * mm * x <> o) do
    #    c:= Random( v );
    #  od;
    fi;
    
    return basechange;

  end );

InstallMethod( MatrixToSesquilinearForm, 
  [ IsMatrix and IsFFECollColl, IsField, IsString ],
  function( m, f, s )
    local newm;

    newm := rec( matrix := m, basefield := f, type := s );
    Objectify(NewType( SesquilinearFormFamily ,  IsSesquilinearFormRep),  newm );
    if IsEvenInt(Size(f)) and
        (s = "elliptic" or s = "hyperbolic" or s = "parabolic") then
      SetIsQuadraticForm(newm, true);
    elif s = "hermitian" then SetIsHermitianForm(newm, true);
    else SetIsBilinearForm(newm, true);
    fi;

    return newm;
  end );

#############################################################################
# Display methods:
#############################################################################

InstallMethod( ViewObj, 
  [ IsHermitianForm ],
  function( f )
    Print("< hermitian form >");
  end );

InstallMethod( PrintObj,
  [ IsHermitianForm ],
        function( f )
        Print("Hermitian form given by Gram Matrix\n",f!.matrix,"\n");
        end );

InstallMethod( Display,
   [ IsHermitianForm ],
   function( f )
      Print("Hermitian form given by Gram Matrix\n");
      Display(f!.matrix);
   end);


InstallMethod( ViewObj, 
  [ IsQuadraticForm ],
  function( f )
    Print("< quadratic form >");
  end );

InstallMethod( PrintObj,
  [ IsQuadraticForm ],
        function( f )
          Print("Quadratic form given by Gram Matrix\n",f!.matrix,"\n");
        end );

InstallMethod( Display,
  [ IsQuadraticForm ],
     function( f )
     Print("Quadratic form given by Gram Matrix\n");
     Display(f!.matrix);
  end );


InstallMethod( ViewObj, 
  [ IsBilinearForm ],
  function( f )
    Print("< bilinear form >");
    end );

InstallMethod( PrintObj,
  [ IsQuadraticForm ],
        function( f )
          Print("Bilinear form given by Gram Matrix\n",f!.matrix,"\n");
        end );

InstallMethod( Display,
  [ IsBilinearForm ],
  function( f )
    Print("Bilinear form given by Gram Matrix\n");
    Display(f!.matrix);
  end);