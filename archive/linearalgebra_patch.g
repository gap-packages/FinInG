BindGlobal( "ShallowCopy_SubspacesDim2",
    iter -> rec( V          := iter!.V,
                 field      := iter!.field,
                 n          := iter!.n,
                 k          := iter!.k,
                 choiceiter := StructuralCopy( iter!.choiceiter ),  ## this is the only change made
                 actchoice  := iter!.actchoice,
                 spaceiter  := ShallowCopy( iter!.spaceiter ) ) );

InstallMethod( Iterator,
    "for subspaces collection of a (finite) full row module",
    [ IsSubspacesVectorSpace and IsSubspacesFullRowSpaceDefaultRep ],
    function( D )
    local V,      # the vector space
          n,      # dimension of `V'
          k,
          iter;   # iterator, result

    V:= D!.structure;

    if not IsFinite( V ) then
      TryNextMethod();
    fi;

    k:= D!.dimension;
    n:= Dimension( V );

    if IsInt( D!.dimension ) then
      iter:= IteratorByFunctions( rec(
                 IsDoneIterator := IsDoneIterator_SubspacesDim,
                 NextIterator   := NextIterator_SubspacesDim,
                 ShallowCopy    := ShallowCopy_SubspacesDim2,
                 V              := V,
                 field          := LeftActingDomain( V ),
                 n              := n,
                 k              := k,
                 choiceiter     := Iterator( Combinations( [ 1..n ],
                                                 D!.dimension ) ) ) );

      iter!.actchoice:= NextIterator( iter!.choiceiter );
      iter!.spaceiter:= IteratorByBasis( CanonicalBasis( FullRowSpace(
           iter!.field, n * k - k * (k - 1) / 2
                               - Sum( iter!.actchoice ) ) ) );
      Add( iter!.actchoice, n+1 );

    else
      iter:= IteratorByFunctions( rec(
                 IsDoneIterator := IsDoneIterator_SubspacesAll,
                 NextIterator   := NextIterator_SubspacesAll,
                 ShallowCopy    := ShallowCopy_SubspacesAll,
                 V              := V,
                 dim            := n,
                 actdim         := 0,
                 actdimiter     := Iterator( Subspaces( V, 0 ) ) ) );
    fi;
    return iter;
    end );

InstallMethod( MutableCopyMat, "for an 8bit matrix",
  [ Is8BitMatrixRep and IsList ],
  function( m )
    local mm;
    mm := List(m,ShallowCopy);
    ConvertToMatrixRep(mm,Q_VEC8BIT(m[1]));
    return mm;
  end );

InstallMethod( MutableCopyMat, "for a gf2 matrix",
  [ IsGF2MatrixRep and IsList ],
  function( m )
    local mm;
    mm := List(m,ShallowCopy);
    ConvertToMatrixRep(mm,2);
    return mm;
  end );

