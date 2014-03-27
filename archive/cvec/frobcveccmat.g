InstallOtherMethod( \^, "for a cvec and a trivial frobenius automorphism",
  [IsCVecRep and IsFFECollection, IsMapping and IsOne],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a mutable cvec and a trivial frobenius automorphism",
  [IsCVecRep and IsFFECollection and IsMutable, IsMapping and IsOne],
  function( v, f )
    return ShallowCopy(v);
  end );

InstallOtherMethod( \^, "for a mutable cvec and a frobenius automorphism",
  [IsCVecRep and IsFFECollection and IsMutable, IsFrobeniusAutomorphism],
  function( v, f )
    local w,i;
    w := ShallowCopy(v);
    for i in [1..Length(w)] do
        w[i] := v[i]^f;
    od;
    return w;
  end );

InstallOtherMethod( \^, "for a cvec and a frobenius automorphism",
  [IsCVecRep and IsFFECollection, IsFrobeniusAutomorphism],
  function( v, f )
    local w,i;
    w := ShallowCopy(v);
    for i in [1..Length(w)] do
        w[i] := v[i]^f;
    od;
    return MakeImmutable(w);
  end );

InstallMethod( \^, "for a cmat and a trivial frobenius automorphism",
  [IsCMatRep and IsFFECollColl, IsMapping and IsOne],
  function( v, f )
    return v;
  end );

InstallMethod( \^, "for a mutable cmat and a trivial frobenius automorphism",
  [IsCMatRep and IsFFECollColl and IsMutable, IsMapping and IsOne],
  function( v, f )
    return MutableCopyMat(v);
  end );

InstallMethod( \^, "for a mutable cmat and a frobenius automorphism",
  [IsCMatRep and IsFFECollColl and IsMutable, IsFrobeniusAutomorphism],
  function( v, f )
    local w,i,j,rl,x,y;
    w := MutableCopyMat(v);
    rl := RowLength(w);
    for i in [1..Length(w)] do
        x := v[i];
        y := w[i];
        for j in [1..rl] do
            y[j] := x[j]^f;
        od;
    od;
    return w;
  end );

InstallMethod( \^, "for a cmat and a frobenius automorphism",
  [IsCMatRep and IsFFECollColl, IsFrobeniusAutomorphism],
  function( v, f )
    local w,i,j,rl,x,y;
    w := MutableCopyMat(v);
    rl := RowLength(w);
    for i in [1..Length(w)] do
        x := v[i];
        y := w[i];
        for j in [1..rl] do
            y[j] := x[j]^f;
        od;
    od;
    return MakeImmutable(w);
  end );


