#############################################################################
# some extensions to the functionality of forms, needed for VectorSpaceToElement
# to be placed in next version of forms after refereeing process.
#############################################################################

#here are bugfixes of constructor methods for forms.
#it is now checked whether the matrix is a matrix over the given field.
#Loading desargues will overload the installed methods in forms.

InstallMethod( BilinearFormByMatrix, "for a ffe matrix and a field",
  [IsMatrix and IsFFECollColl, IsField and IsFinite],
  function( m, f )
    local el;
    if not (m[1][1] in f) then
      Error("<m> is not a matrix over <f>");
    fi;
    if IsZero(m) then
       el := rec( matrix := m, basefield := f, type := "trivial" );
       Objectify(NewType( TrivialFormFamily ,  IsFormRep),  el);
 	   return el;
    elif IsSymplecticMatrix(m,f) then
       el := rec( matrix := m, basefield := f, type := "symplectic" );
       Objectify(NewType( BilinearFormFamily ,  IsFormRep),  el);
	   return el;
    elif (IsOrthogonalMatrix(m) and IsOddInt(Size(f))) then 
       el := rec( matrix := m, basefield := f, type := "orthogonal" );
       Objectify(NewType( BilinearFormFamily ,  IsFormRep),  el);
 	   return el;
    elif (IsOrthogonalMatrix(m) and IsEvenInt(Size(f))) then
       el := rec( matrix := m, basefield := f, type := "pseudo" );
       Objectify(NewType( BilinearFormFamily ,  IsFormRep),  el);
 	   return el; 
    else
       Error("Invalid Gram matrix\n");
    fi;
  end );

InstallMethod( QuadraticFormByMatrix, "for a ffe matrix and a field",
  [IsMatrix and IsFFECollColl, IsField and IsFinite],
  function( m, f )
    local el;
    if not (m[1][1] in f) then
      Error("<m> is not a matrix over <f>");
    fi;
    el := rec( matrix := m, basefield := f, type := "quadratic" );
    if IsZero(m) then
       Objectify(NewType( TrivialFormFamily ,  IsFormRep),  el);
 	   return el;
    else
       el.matrix := RESET(m,Length(m),Size(f));
	Objectify(NewType( QuadraticFormFamily ,  IsFormRep),  el);
       return el;
    fi;
  end );

InstallMethod( HermitianFormByMatrix, "for a ffe matrix and a field",
  [IsMatrix and IsFFECollColl, IsField and IsFinite],
  function( m, f )
    local el;    
    if not (m[1][1] in f) then
      Error("<m> is not a matrix over <f>");
    fi;
    if not IsInt(Sqrt(Size(f))) then
        Error("No hermitian form exist when the order of <f> is not a square\n" );
    fi;
    if IsHermitianMatrix(m,f) then
       el := rec( matrix := m, basefield := f, type := "hermitian" );
       Objectify(NewType( HermitianFormFamily ,  IsFormRep),  el);
       return el;
    else
       Error("Given matrix does not define a hermitian form\n" );
    fi;   
  end );

#############################################################################
#small extension. Just gives a base of the radical instead of the radical
#itself.
#############################################################################

DeclareOperation("RadicalOfFormMat", [IsForm]);

InstallMethod( RadicalOfFormMat, [IsSesquilinearForm],
  function( f )
    local m, gf, d;
    if not IsReflexiveForm( f ) then
       Error( "Form must be reflexive\n");
    fi;
    m := f!.matrix;
    gf := f!.basefield;
    d := Size(m);
    return NullspaceMat( m );
  end );

InstallMethod( RadicalOfFormMat, [IsQuadraticForm],
  function( f )
    local m, null, gf, d;
    m := f!.matrix;
    m := m + TransposedMat(m);
    gf := f!.basefield;
    d := Size(m);
    null := NullspaceMat( m );
    if IsEvenInt(Size(gf)) then 
      null := Filtered(null, x -> IsZero(x^f)); #checks each vector for f(v)=0
    fi;
    return null;
  end );


#############################################################################
#small extension. Just gives the companion automorphism of ses forms.
#############################################################################

DeclareAttribute( "CompanionAutomorphism", IsSesquilinearForm );

InstallMethod( CompanionAutomorphism, [ IsSesquilinearForm ],
  function( form )
    local field, aut;
    field := BaseField( form );
    if IsHermitianForm( form ) then
       aut := FrobeniusAutomorphism( field );
       aut := aut ^ (Order(aut)/2);
    else
       aut := IdentityMapping( field );
    fi;
    return aut;
  end );

#############################################################################
#main part: add IsIsotropicVector, IsTotallyIsotropicSubspace and singular
#stuff.
#############################################################################

DeclareOperation("OrthogonalSubspaceMat", [IsForm, IsVector and IsFFECollection]);
DeclareOperation("OrthogonalSubspaceMat", [IsForm, IsMatrix]);

DeclareOperation("OrthogonalSubspace", [IsForm, IsVector and IsFFECollection]);
DeclareOperation("OrthogonalSubspace", [IsForm, IsMatrix]);

DeclareOperation("IsIsotropicVector", [IsForm, IsVector and IsFFECollection]);
DeclareOperation("IsTotallyIsotropicSubspace", [IsForm, IsMatrix]);

DeclareOperation("IsSingularVector", [IsQuadraticForm, IsVector and IsFFECollection]);
DeclareOperation("IsTotallySingularSubspace", [IsQuadraticForm, IsMatrix]);

#############################################################################
# We start with orthogonal subspaces to a given subspace wrt sesquilinear forms.
#############################################################################

#this method computes the base of the subspace orthogonal to a vector wrt a
#given bil form
InstallMethod(OrthogonalSubspaceMat,
  "for a form and a vector",
  [IsBilinearForm, IsVector and IsFFECollection],
  function(f,v)
  local mat;
  mat := f!.matrix;
  if Length(v) <> Length(mat) then
    Error("<v> has the wrong dimension");
  fi;
  return NullspaceMat(TransposedMat([v*mat]));
end );

#this method computes the base of the subspace orthogonal to a given one wrt a
#given bil form 
InstallMethod(OrthogonalSubspaceMat,
  "for a form and a basis of a subspace",
  [IsBilinearForm, IsMatrix],
  function(f,sub)
  local mat,perp;
  mat := f!.matrix;
  if Length(sub[1]) <> Length(mat) then
    Error("<sub> contains vectors of wrong dimension");
  fi;
  perp := TransposedMat(sub*mat);
  return NullspaceMat(perp);
end );

#this method computes the base of the subspace orthogonal to a vector wrt a
#given hermitian form 
InstallMethod(OrthogonalSubspaceMat,
  "for a form and a vector",
  [IsHermitianForm, IsVector and IsFFECollection],
  function(f,v)
  local mat,gf,t,vt;
  mat := f!.matrix;
  if Length(v) <> Length(mat) then
    Error("<v> has the wrong dimension");
  fi;
  gf := f!.basefield;
  t := Sqrt(Size(gf));
  vt := List(v,x->x^t);
  return NullspaceMat(mat*TransposedMat([vt]));
end );

#this method computes the base of the subspace orthogonal to a given one wrt a
#given hermitian form 
InstallMethod(OrthogonalSubspaceMat,
  "for a form and a basis of a subspace",
  [IsHermitianForm, IsMatrix],
  function(f,sub)
  local mat,gf,t,subt;
  mat := f!.matrix;
  if Length(sub[1]) <> Length(mat) then
    Error("<sub> contains vectors of wrong dimension");
  fi;
  gf := f!.basefield;
  t := Sqrt(Size(gf));
  subt := List(sub,x->List(x,y->y^t));
  return NullspaceMat(mat*TransposedMat(subt));
end );

#############################################################################
# Methods for IsIsotropicVector and IsTotallyIsotropicSubspace for sesquilinear
# forms.
#############################################################################

#this method returns true if and only if v is isotropic wrt a sesquilinear form
InstallMethod(IsIsotropicVector,
  "for a form and a vector",
  [IsSesquilinearForm, IsVector and IsFFECollection],
  function(f,v)
  return IsZero( [v,v]^f );
end );

#this method returns true if and only if sub is a basis of a t.i. subspace wrt a
#sesquilinear f. 
InstallMethod(IsTotallyIsotropicSubspace,
  "for a form and a basis of a subspace",
  [IsSesquilinearForm, IsMatrix],
  function(f,sub)
  ## JB: quicker method
    local mat;
    mat := f!.matrix;
    if f!.type = "hermitian" then
       return IsZero( (sub^CompanionAutomorphism( f )) * mat * TransposedMat(sub) );
    else 
       return IsZero( sub * mat * TransposedMat(sub) ); 
    fi;  
  #local osub,span;
  #osub := OrthogonalSubspaceMat(f,sub);
  #span := Concatenation(sub,osub);
  #return Rank(span)=Rank(osub);
end );

#############################################################################
# Now quadratic forms. Remember the subtle difference between orthogonality and
# singularity for subspaces.
#############################################################################

#this method computes the base of the subspace orthogonal to a given one wrt a
#given quadratic form 
InstallMethod(OrthogonalSubspaceMat,
  "for a form and a vector",
  [IsQuadraticForm, IsVector and IsFFECollection],
  function(f,v)
  local bilf;
  bilf := AssociatedBilinearForm(f);
  return OrthogonalSubspaceMat(bilf,v); #note that this call will perform dim
end );					#checks


#this method computes the base of the subspace orthogonal to a given one wrt a
#given quad form
InstallMethod(OrthogonalSubspaceMat,
  "for a form and a basis of a subspace",
  [IsQuadraticForm, IsMatrix],
  function(f,sub)
  
  
  
  local bilf;
  bilf := AssociatedBilinearForm(f);
  return OrthogonalSubspaceMat(bilf,sub); #note that this call will perform dim
end );					#checks


#############################################################################
# Methods for IsIsotropicVector and IsTotallyIsotropicSubspace for quadratic
# forms (look at the definitions!);
#############################################################################

#this method returns true if and only if v is isotropic wrt a quadratic form
InstallMethod(IsIsotropicVector,
  "for a quadratic form and a vector",
  [IsQuadraticForm, IsVector and IsFFECollection],
  function(f,v)
  return IsIsotropicVector(AssociatedBilinearForm(f),v); #performs dim checks
end );

#this method returns true if and only if sub is a basis of a t.i. subspace wrt a
#quadratic form
InstallMethod(IsTotallyIsotropicSubspace,
  "for a quadratic form and a basis of a subspace",
  [IsQuadraticForm, IsMatrix],
  function(f,sub)
  return IsTotallyIsotropicSubspace(AssociatedBilinearForm(f),sub); #performs dc.
end );

#############################################################################
# Methods for IsSingularVector and IsTotallySingularSubspace for quadratic
# forms (look at the definitions!);
#############################################################################

#this method returns true if and only if v is isotropic wrt a quadratic form
InstallMethod(IsSingularVector,
  "for a quadratic form and a vector",
  [IsQuadraticForm, IsVector and IsFFECollection],
  function(f,v)
  return v^f=Zero(BaseField(f));
end );

#this method returns true if and only if sub is a basis of a t.i. subspace wrt a
#quadratic form
InstallMethod(IsTotallySingularSubspace,
  "for a quadratic form and a basis of a subspace",
  [IsQuadraticForm, IsMatrix],
  function(f,sub)
  local fsub;
  fsub := Filtered(sub,x-> IsZero(x^f));
  if Length(fsub) <> Length(sub) then
    return false;
  else
    return IsTotallyIsotropicSubspace(AssociatedBilinearForm(f),sub);
  fi;
end );

