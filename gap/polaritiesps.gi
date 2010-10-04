#############################################################################
##
##  polarities.gi              FinInG package
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
##  Copyright 2008 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Implementation stuff for polarities of projective spaces
##
#############################################################################

#############################################################################
# operations to create polarities of projective spaces 
#############################################################################

# operation to construct polarity of projective space using a sesquilinear form.
# to be considered as the standard.

InstallMethod(PolarityOfProjectiveSpaceOp,
  "for a sesquilinear form of the underlying vectorspace",
  [IsSesquilinearForm and IsFormRep],
  function(form)
  local mat,field,n,aut,v,ps,q,delta,polarity;
  if IsDegenerateForm(form) then
    Error("<form> must not be degenerate");
  fi;
  mat := GramMatrix(form);
  field := BaseField(form);
  n := Size(mat);
  aut := CompanionAutomorphism(form);
  v := field^n;
  ps := ProjectiveSpace(n-1,field);
  delta := StandardDualityOfProjectiveSpace(ps);
  polarity :=  ProjElWithFrobWithPSIsom(mat,aut,field,delta);
  polarity!.form := form;
  SetFilterObj( polarity, IsPolarityOfProjectiveSpace );
  SetFilterObj( polarity, IsPolarityOfProjectiveSpaceRep );
  return polarity;
end );

#############################################################################
# standard View, Display and Print operations 
#############################################################################

InstallMethod( ViewObj, 
  "for a polarity",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function(el)
  local dim, field;
  field := el!.fld;
  dim := Size(el!.mat);
  Print("<polarity of PG(", dim-1, ", ", field, "), ");
  ViewObj(el!.mat);
  if IsOne(el!.frob) then
      Print(", F^0");
  else
      Print(", F^",el!.frob!.power);
  fi;
  Print(" >");
  end);

InstallMethod( PrintObj,
  "for a polarity",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function( f )
  local dim, field;
  field := f!.fld;
  dim := Size(f!.mat);
  Print("<polarity of PG(", dim-1, ", ", field, ")>, underlying matrix\n");
  PrintObj(f!.mat);
  Print(",");
  PrintObj(f!.frob);
  Print(") \n");
end );

InstallMethod( Display, "for a projective group element with Frobenius with projective space isomorphism",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function(f)
    local dim, field;
    dim := Size(f!.mat);
    field := f!.fld;
    Print("<polarity of PG(", dim-1, ", ", field, ")>, underlying matrix\n");
    Display(f!.mat);
    if IsOne(f!.frob) then
        Print(", F^0");
    else
        Print(", F^",f!.frob!.power);
    fi;
    Print(">\n");
  end );

#############################################################################
# Constructor operations 
#############################################################################

#First user function to create polarity of projective space.
#using a form. Just calls ...Op version of this operation.

InstallMethod(PolarityOfProjectiveSpace,
  "for a sesquilinear form of the underlying vectorspace",
  [IsSesquilinearForm and IsFormRep],
  function(form)
  return PolarityOfProjectiveSpaceOp(form);
end );

InstallMethod(PolarityOfProjectiveSpace,
  "for a matrix and a finite field",
  [IsMatrix,IsField and IsFinite],
  function(matrix,field)
  local form;
  if Rank(matrix) <> Size(matrix) then
    Error("<matrix> must not be singular");
  fi;
  form := BilinearFormByMatrix(matrix,field);  
  return PolarityOfProjectiveSpaceOp(form);
end );

InstallMethod(PolarityOfProjectiveSpace,
  "for a matrix, a finite field, and a frobenius automorphism",
  [IsMatrix, IsFrobeniusAutomorphism, IsField and IsFinite],
  function(matrix,frob,field)
  local form;
  if Rank(matrix) <> Size(matrix) then
    Error("<matrix> must not be singular");
  fi;
  if Order(frob)<>2 then
    Error("<frob> must be involutory or identity");
  else
    form := HermitianFormByMatrix(matrix,field);
    return PolarityOfProjectiveSpaceOp(form);
  fi;
end );

InstallMethod(HermitianPolarityOfProjectiveSpace,
  "for a sesquilinear form of a projective space",
  [IsMatrix,IsField and IsFinite],
  function(matrix,field)
  local form;
  if Rank(matrix) <> Size(matrix) then
    Error("<matrix> must not be singular");
  fi;
  if not IsInt(Sqrt(Size(field))) then
    Error("Size of <field> must be a square" );
  fi;
  form := HermitianFormByMatrix(matrix,field);  
  return PolarityOfProjectiveSpaceOp(form);
end );

#the next method returns the polarity associated to a polar space.
#recall that polarspaces and associated polarities are equivalent, except
#when q is even and the polar space is orthogonal, in this case, the associated 
#symplectic polarity is returned, so usable to compucte tangent hyperplanes etc,
#but not usable to construct a polarity, because we ask a non-degenerate form.
#the above condition is, due to definitions in desargues, and the definition of
#the "associated sesquilinear form of a polar space, equivalent with cheking
#whether the form returned by SesquilinearForm(ps) is degenerate. If not, we go!
#all necessary algebraic stuff is in the forms package.

InstallMethod(PolarityOfProjectiveSpace,
  "for a polar space",
  [IsClassicalPolarSpace],
  function(ps)
  local form;
  form := SesquilinearForm(ps);
  if IsDegenerateForm(form) then
    Error("no polarity of the ambient projective space can be associated to <ps>");
  else return PolarityOfProjectiveSpace(form);
  fi;
end );

#############################################################################
# operations, attributes and properties
#############################################################################

InstallMethod( GramMatrix, "for a polarity of a projective space",
   [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   x -> x!.mat );

InstallOtherMethod( BaseField, "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   x -> x!.fld );

InstallMethod( CompanionAutomorphism, "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   x -> x!.frob );

InstallMethod( SesquilinearForm, "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   el -> el!.form );


InstallMethod( IsHermitianPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsHermitianForm(x!.form) );

InstallMethod( IsOrthogonalPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsOrthogonalForm(x!.form) );

InstallMethod( IsSymplecticPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsSymplecticForm(x!.form) );

InstallMethod( IsPseudoPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsPseudoForm(x!.form) );

InstallMethod( PolarSpace, "from a polarity of a projective space",
  [ IsPolarityOfProjectiveSpace ],
  function( polarity )
    local form, ps;
    form := SesquilinearForm(polarity);
    if not IsPseudoForm(form) then
       ps := PolarSpace( form );
    else
       Error("<polarity> is pseudo and does not induce a polar space");
    fi;
    return ps;
  end );

#installs the method for sub^phi with sub a subspace of a projectivespace and
# phi a polarity of a projective space. To be discussed whether this should be done
# in general or only for polarities.
# uses OnProjSubspacesReversing, defined in group2.g*
InstallOtherMethod( \^,
  "for an element of a projective space and a polarity of a projective space",
  [ IsSubspaceOfProjectiveSpace, IsPolarityOfProjectiveSpace],
  function(sub,phi)
  return OnProjSubspacesReversing(sub,phi);
end );

InstallMethod( GeometryOfAbsolutePoints, 
  "for a polarity of a projective space",
  [ IsPolarityOfProjectiveSpace ],
  function( polarity )
    local form, geom, ps, vect, mat, n, sub;
    form := SesquilinearForm(polarity);
    if IsPseudoForm(form) then
       mat := polarity!.mat;
       n := Length(mat);
       vect := List([1..n],i->mat[i][i]);
       sub := NullspaceMat(TransposedMat([vect]));
       ps := ProjectiveSpace(n-1,polarity!.fld);
       return VectorSpaceToElement(ps,sub);
    else
       return PolarSpace(form);
    fi;
    return ps;
  end );

InstallMethod( AbsolutePoints,
  "for a polarity of a projective space",
  [ IsPolarityOfProjectiveSpace ],
  function( polarity )
    return Points(GeometryOfAbsolutePoints(polarity));
  end );

#the following is obsolete now.
#InstallMethod( IsDegeneratePolarity, "for a polarity of a projective space",
#    [ IsPolarityOfProjectiveSpace ],
#  function( polarity )
#    local form;
#    form := Form( polarity );
#    return IsDegenerateForm( form );
#  end );

#InstallGlobalFunction( OnProjSubspacesReversing,
#  function( line, el )
#    local vec,c;
#    vec := (OnRight(line,el!.mat)^el!.frob)^el!.duality;
#    if not(IsMutable(vec)) then
#        vec := MutableCopyMat(vec);
#    fi;
#    TriangulizeMat(vec);
#    return vec;
#  end );
