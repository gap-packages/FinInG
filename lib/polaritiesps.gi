#############################################################################
##
##  polaritiesps.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2018	Colorado State University
##                  Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universiteit Gent
##					University of St. Andrews
##					University of Western Australia
##                  Vrije Universiteit Brussel
##
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
  n := NrRows(mat);
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
  dim := NrRows(el!.mat);
  Print("<polarity of PG(", dim-1, ", ", field, ")");
  #ViewObj(el!.mat);
  #if IsOne(el!.frob) then
  #    Print(", F^0");
  #else
  #    Print(", F^",el!.frob!.power);
  #fi;
  Print(" >");
  end);

InstallMethod( PrintObj,
  "for a polarity",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function( f )
  local dim, field;
  field := f!.fld;
  dim := NrRows(f!.mat);
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
    dim := NrRows(f!.mat);
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
  if Rank(matrix) <> NrRows(matrix) then
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
  if Rank(matrix) <> NrRows(matrix) then
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
  if Rank(matrix) <> NrRows(matrix) then
    Error("<matrix> must not be singular");
  fi;
  if IsOddInt(DegreeOverPrimeField(field)) then
    Error("Size of <field> must be a square" );
  fi;
  form := HermitianFormByMatrix(matrix,field);  
  return PolarityOfProjectiveSpaceOp(form);
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

#installs the method for sub^phi with sub a subspace of a projectivespace and
# phi a polarity of a projective space. To be discussed whether this should be done
# in general or only for polarities.
# uses OnProjSubspacesExtended, defined in group2.g*
InstallOtherMethod( \^,
  "for an element of a projective space and a polarity of a projective space",
  [ IsSubspaceOfProjectiveSpace, IsPolarityOfProjectiveSpace],
  function(sub,phi)
  return OnProjSubspacesExtended(sub,phi);
end );

#the following is obsolete now.
#InstallMethod( IsDegeneratePolarity, "for a polarity of a projective space",
#    [ IsPolarityOfProjectiveSpace ],
#  function( polarity )
#    local form;
#    form := Form( polarity );
#    return IsDegenerateForm( form );
#  end );

#InstallGlobalFunction( OnProjSubspacesExtended,
#  function( line, el )
#    local vec,c;
#    vec := (OnRight(line,el!.mat)^el!.frob)^el!.duality;
#    if not(IsMutable(vec)) then
#        vec := MutableCopyMat(vec);
#    fi;
#    TriangulizeMat(vec);
#    return vec;
#  end );
