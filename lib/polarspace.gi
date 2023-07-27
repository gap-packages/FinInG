#############################################################################
##
##  polarspace.gi              FinInG package
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
##  Implementation stuff for polar spaces
##
#############################################################################

#############################################################################
# Low level help methods:
#############################################################################

FINING.LimitForCanComputeActionOnPoints := 1000000;
FINING.Fast := true;

#############################################################################
# Constructor method (not for users)!:
#############################################################################

# CHECKED 20/09/11 jdb
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# This is an internal subroutine which is not expected to be used by the user;
# they would be using VectorSpaceToElement. Recall that Wrap is declared in 
# geometry.gd. 
##
InstallMethod( Wrap, 
	"for a polar space and an object",
	[IsClassicalPolarSpace, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and 
		#Objectify( NewType( SoCPSFamily, IsElementOfIncidenceStructure and #keep this, maybe for future use.
		IsElementOfIncidenceStructureRep and IsSubspaceOfClassicalPolarSpace ), w );
    return w;
	end );

#############################################################################
# Constructor methods for polar spaces.
#############################################################################

# CHECKED 11/10/11 jb + jdb
#############################################################################
#O  PolarSpace( <m>, <f>, <g>, <act> )
# This method returns a polar space with a lot of knowledge. most likely not for user.
# <m> is a sesquilinear form. Furthermore, a field <f>, a group <g> and an action 
# function <act> are given.
##
InstallMethod( PolarSpace, 
	"for a sesquilinear form, a field, a group and an action function",
	[ IsSesquilinearForm, IsField, IsGroup, IsFunction ],
	function( m, f, g, act )
		local geo, ty, gram, eq, r, i1, i2, r2, fam, j, flag;
		if IsDegenerateForm( m ) then 
			Error("Form is degenerate");
		elif IsPseudoForm( m ) then
			Error("No Polar space can be associated with a pseudo form");
		fi;
		gram := m!.matrix;
		geo := rec( basefield := f, dimension := Length(gram)-1,
					vectorspace := FullRowSpace(f,Length(gram)) );
		if IsHermitianForm(m) then
			flag := IsHermitianVariety;
		else
			flag := IsQuadraticVariety;
		fi;
		if not IsAlternatingForm(m) then
			if WittIndex(m) = 2 then
				ty := NewType( GeometriesFamily,
						IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ and flag);
				else ty := NewType( GeometriesFamily,
							IsClassicalPolarSpace and IsClassicalPolarSpaceRep and flag);
			fi;
		else
			if WittIndex(m) = 2 then
				ty := NewType( GeometriesFamily,
						IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ);
				else ty := NewType( GeometriesFamily,
							IsClassicalPolarSpace and IsClassicalPolarSpaceRep);
			fi;
		fi;
		# at this stage, we are sure that if m is a bilinear form and the characteristic is even, 
		# then m will be symplectic and PolynomialOfForm(m) will produce an error.
		# on the other hand, if m is symplectic and the characteristic is odd, then PolynomialOfForm
		# will produce a 0*Z(q). 
		if IsEvenInt(Size(f)) and IsBilinearForm(m) then  
			eq := Zero(f);
		else
			eq := PolynomialOfForm( m );
		fi;
		if IsZero(eq) then
			i1 := List([1..Length(gram)],i->Concatenation("x",String(i)));
			i2 := List([1..Length(gram)],i->Concatenation("y",String(i)));
			#r := PolynomialRing(f,Concatenation(i1,i2):old);     ## there was an error here for gap4r5
			#i2 := IndeterminatesOfPolynomialRing(r){[Length(gram)+1..2*Length(gram)]};
			#there was a proble with the above lines. If say x_4 existed already, and i1 just ran until 3, than
			# x_4 got changed by y_1, which caused all polynomials in r to change also, which caused funny printing behaviour.
			# the 10000 is completely arbitrary, I guess nobody will use the first 10000 variables in forms, since nobody
			# will work in a 10000-dimensional vector space (I guess...).
			r := PolynomialRing(f,i1:old);     ## there was an error here for gap4r5
			i1 := IndeterminatesOfPolynomialRing(r){[1..Length(gram)]};
			r2 := PolynomialRing(f,[10001..10000+Length(gram)]:old);
			fam := FamilyObj(r2.1);
			for j in [1..Length(gram)] do
				SetIndeterminateName(fam,10000+j,i2[j]);
			od;
			i2 := IndeterminatesOfPolynomialRing(r2);
			eq := i1*gram*i2;
		fi;
		ObjectifyWithAttributes( geo, ty, 
								SesquilinearForm, m,
								CollineationGroup, g,
								CollineationAction, act,
								AmbientSpace, ProjectiveSpace(geo.dimension, f),
								EquationForPolarSpace, eq );
		return geo;
	end );

# CHECKED 11/10/11 jb + jdb
#############################################################################
#O  PolarSpaceStandard( <m>, <bool> )
# Method to crete a polar space using sesquilinear form <m>, where we know
# that this form is the standard one used in Fining. Not intended for users, 
# all checks are removed.
##
InstallMethod( PolarSpaceStandard, 
	"for a sesquilinear form",
	[ IsSesquilinearForm, IsBool ],
	function( m, bool )
		local geo, ty, gram, f, eq, r, i1, i2, r2, fam, j, flag;
		gram := m!.matrix;
		f := m!.basefield;
		geo := rec( basefield := f, dimension := Length(gram)-1,
				vectorspace := FullRowSpace(f,Length(gram)) );
		if IsHermitianForm(m) then
			flag := IsHermitianVariety;
		else
			flag := IsQuadraticVariety;
		fi;
		if not IsAlternatingForm(m) then
			if WittIndex(m) = 2 then
				ty := NewType( GeometriesFamily,
						IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ and flag);
				else ty := NewType( GeometriesFamily,
							IsClassicalPolarSpace and IsClassicalPolarSpaceRep and flag);
			fi;
		else
			if WittIndex(m) = 2 then
				ty := NewType( GeometriesFamily,
						IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ);
				else ty := NewType( GeometriesFamily,
							IsClassicalPolarSpace and IsClassicalPolarSpaceRep);
			fi;
		fi;
		# at this stage, we are sure that if m is a bilinear form and the characteristic is even, 
		# then m will be symplectic and PolynomialOfForm(m) will produce an error.
		# on the other hand, if m is symplectic and the characteristic is odd, then PolynomialOfForm
		# will produce a 0*Z(q). 
		if IsEvenInt(Size(f)) and IsBilinearForm(m) then  
			eq := Zero(f);
		else
			eq := PolynomialOfForm( m );
		fi;
		if IsZero(eq) then
			#Print("eq is zero\n");
			i1 := List([1..Length(gram)],i->Concatenation("x",String(i)));
			i2 := List([1..Length(gram)],i->Concatenation("y",String(i)));
			#r := PolynomialRing(f,Concatenation(i1,i2):old);     ## there was an error here for gap4r5
			#i2 := IndeterminatesOfPolynomialRing(r){[Length(gram)+1..2*Length(gram)]};
			#there was a proble with the above lines. If say x_4 existed already, and i1 just ran until 3, than
			# x_4 got changed by y_1, which caused all polynomials in r to change also, which caused funny printing behaviour.
			# the 10000 is completely arbitrary, I guess nobody will use the first 10000 variables in forms, since nobody
			# will work in a 10000-dimensional vector space (I guess...).
			r := PolynomialRing(f,i1:old);     ## there was an error here for gap4r5
			i1 := IndeterminatesOfPolynomialRing(r){[1..Length(gram)]};
			r2 := PolynomialRing(f,[10001..10000+Length(gram)]:old);
			fam := FamilyObj(r2.1);
			for j in [1..Length(gram)] do
				SetIndeterminateName(fam,10000+j,i2[j]);
			od;
			i2 := IndeterminatesOfPolynomialRing(r2);
			eq := i1*gram*i2;
		fi;
		ObjectifyWithAttributes( geo, ty, 
                            SesquilinearForm, m,
                            AmbientSpace, ProjectiveSpace(geo.dimension, f),
							EquationForPolarSpace, eq );
		if bool then
			SetIsStandardPolarSpace(geo,true); #if bool is false, we can use this operation to construct almost standard polar spaces :-)
		fi;
		return geo;
	end );

#############################################################################
#O  PolarSpaceStandard( <m>, <bool> )
# general method to create a polar space using quadratic form. Not intended 
# for the user. no checks.
##
InstallMethod( PolarSpaceStandard, 
	"for a quadratic form",
	[ IsQuadraticForm, IsBool ],
	function( m, bool )
		local geo, ty, gram, polar, f, flavour, eq, flag;
		f := m!.basefield;
		gram := m!.matrix;
		polar := AssociatedBilinearForm( m );
		geo := rec( basefield := f, dimension := Length(gram)-1,
					vectorspace := FullRowSpace(f,Length(gram)) );
		if WittIndex(m) = 2 then
			ty := NewType( GeometriesFamily,
					IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ and IsProjectiveVariety);
		else ty := NewType( GeometriesFamily,
					IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsProjectiveVariety);
		fi;
		eq := PolynomialOfForm( m );
		ObjectifyWithAttributes( geo, ty, 
								QuadraticForm, m,
								SesquilinearForm, polar,
								AmbientSpace, ProjectiveSpace(geo.dimension, f),
								EquationForPolarSpace, eq );
		if bool then
			SetIsStandardPolarSpace(geo,true);
		fi;
		return geo;
	end );

# CHECKED 20/09/11 jdb
#############################################################################
#O  PolarSpace( <m> )
# general method to crete a polar space using sesquilinear form <m>. It is checked
# whether the form is not pseudo.##
##
InstallMethod( PolarSpace, 
	"for a sesquilinear form",
	[ IsSesquilinearForm ],
	function( m )
		local geo, ty, gram, f, eq, r, i1, i2, r2, fam, j, flag;  
		if IsDegenerateForm( m ) then 
			Error("Form is degenerate");
		elif IsPseudoForm( m ) then
			Error("No Polar space can be associated with a pseudo form");
		fi;
		gram := m!.matrix;
		f := m!.basefield;
		geo := rec( basefield := f, dimension := Length(gram)-1,
				vectorspace := FullRowSpace(f,Length(gram)) );
		if IsHermitianForm(m) then
			flag := IsHermitianVariety;
		else
			flag := IsQuadraticVariety;
		fi;
		if not IsAlternatingForm(m) then
			if WittIndex(m) = 2 then
				ty := NewType( GeometriesFamily,
						IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ and flag);
				else ty := NewType( GeometriesFamily,
							IsClassicalPolarSpace and IsClassicalPolarSpaceRep and flag);
			fi;
		else
			if WittIndex(m) = 2 then
				ty := NewType( GeometriesFamily,
						IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ);
				else ty := NewType( GeometriesFamily,
							IsClassicalPolarSpace and IsClassicalPolarSpaceRep);
			fi;
		fi;
		# at this stage, we are sure that if m is a bilinear form and the characteristic is even, 
		# then m will be symplectic and PolynomialOfForm(m) will produce an error.
		# on the other hand, if m is symplectic and the characteristic is odd, then PolynomialOfForm
		# will produce a 0*Z(q). 
		if IsEvenInt(Size(f)) and IsBilinearForm(m) then  
			eq := Zero(f);
		else
			eq := PolynomialOfForm( m );
		fi;
		if IsZero(eq) then
			#Print("eq is zero\n");
			i1 := List([1..Length(gram)],i->Concatenation("x",String(i)));
			i2 := List([1..Length(gram)],i->Concatenation("y",String(i)));
			#r := PolynomialRing(f,Concatenation(i1,i2):old);     ## there was an error here for gap4r5
			#i2 := IndeterminatesOfPolynomialRing(r){[Length(gram)+1..2*Length(gram)]};
			#there was a proble with the above lines. If say x_4 existed already, and i1 just ran until 3, than
			# x_4 got changed by y_1, which caused all polynomials in r to change also, which caused funny printing behaviour.
			# the 10000 is completely arbitrary, I guess nobody will use the first 10000 variables in forms, since nobody
			# will work in a 10000-dimensional vector space (I guess...).
			r := PolynomialRing(f,i1:old);     ## there was an error here for gap4r5
			i1 := IndeterminatesOfPolynomialRing(r){[1..Length(gram)]};
			r2 := PolynomialRing(f,[10001..10000+Length(gram)]:old);
			fam := FamilyObj(r2.1);
			for j in [1..Length(gram)] do
				SetIndeterminateName(fam,10000+j,i2[j]);
			od;
			i2 := IndeterminatesOfPolynomialRing(r2);
			eq := i1*gram*i2;
		fi;
		ObjectifyWithAttributes( geo, ty, 
                            SesquilinearForm, m,
                            AmbientSpace, ProjectiveSpace(geo.dimension, f),
							IsStandardPolarSpace, false,
							EquationForPolarSpace, eq );
		return geo;
	end );

# CHECKED 20/09/11 jdb
#############################################################################
#O  PolarSpace( <m> )
#general method to create a polar space using quadratic form. Possible in even
#and odd char.
##
InstallMethod( PolarSpace, 
	"for a quadratic form",
	[ IsQuadraticForm ],
	function( m )
		local geo, ty, gram, polar, f, flavour, eq;
		if IsSingularForm( m ) then 
			Error("Form is singular"); 
		fi;
		f := m!.basefield;
		gram := m!.matrix;
		polar := AssociatedBilinearForm( m );
		geo := rec( basefield := f, dimension := Length(gram)-1,
					vectorspace := FullRowSpace(f,Length(gram)) );
		if WittIndex(m) = 2 then
			ty := NewType( GeometriesFamily,
					IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ and IsQuadraticVariety);
		else ty := NewType( GeometriesFamily,
					IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsQuadraticVariety);
		fi;
		eq := PolynomialOfForm(m);
		ObjectifyWithAttributes( geo, ty, 
								QuadraticForm, m,
								SesquilinearForm, polar,
								AmbientSpace, ProjectiveSpace(geo.dimension, f),
								IsStandardPolarSpace, false,
								EquationForPolarSpace, eq );
		return geo;
	end );

# CHECKED 20/09/11 jdb
#############################################################################
#O  PolarSpace( <m> )
# general method to setup a polar space using hermitian form. Is this method
# still necessary? Maybe not necessary, but maybe usefull, since we know slightly
# more properties of the polar space if we start from a hermitian form rather than
# from a sesquilinear form.
##
InstallMethod( PolarSpace, 
	"for a hermitian form",
	[ IsHermitianForm ],
	function( m )
		local geo, ty, gram, f, eq;
		if IsDegenerateForm( m ) then 
			Error("Form is degenerate");
		fi;
		gram := m!.matrix;
		f := m!.basefield;
		geo := rec( basefield := f, dimension := Length(gram)-1,
					vectorspace := FullRowSpace(f,Length(gram)) );
		if WittIndex(m) = 2 then
			ty := NewType( GeometriesFamily,
                  IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsClassicalGQ and IsHermitianVariety);
		else ty := NewType( GeometriesFamily,
                  IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianVariety);
		fi;
		eq := PolynomialOfForm(m);
		ObjectifyWithAttributes( geo, ty, 
                            SesquilinearForm, m,
                            AmbientSpace, ProjectiveSpace(geo.dimension, f),
							IsStandardPolarSpace, false,
							EquationForPolarSpace, eq );
		return geo;
	end );


#############################################################################
## Constructions of Canonical finite classical Polar Spaces
## Important remark. We use "Canonical" in the mathematical sense, i.e, a polar
## space that equals one of the "standard" polar spaces constructed by the methods
## below. So "Standard" refers to a particular form used to construct the polar space. 
## It is well known that two forms that are similar, i.e. differe on a constant factor,
## will yield the same polar space. So a polar space is canonical iff its forms is similar with
## the form of one of the polar spaces constructed with a method below. A polar space is 
## "standard" iff it is constructed with one of the methods below. So if the user
## uses his favorite form, his polar space can become canonical, but it can never become standard.
#############################################################################

#############################################################################
# Part I: A method to setup the representative of the maximal 
# subspaces for polar spaces of each type.
# Recall that the methods for methods for canonical gram matrices and canonical 
# quadratic forms are found in group.gi. All these methods are not intended for the user.
#############################################################################

# CHECKED 21/09/11 jdb
#############################################################################
#O  CanonicalOrbitRepresentativeForSubspaces( <type>, <d>, <f> )
## This function returns representatives for the
## maximal totally isotropic (singular) subspaces
## of the given canonical polar space
##
InstallMethod( CanonicalOrbitRepresentativeForSubspaces, 
	"for a string, an integer and a field",
	[IsString, IsPosInt, IsField],		
	function( type, d, f )
		local b, i, id, one, w, q, sqrtq;
		q := Size(f);
		id := IdentityMat(d, f);
		if type = "symplectic" then    
			b := List([1..d/2], i-> id[2*i-1] + id[2*i]);
		elif type = "hermitian" then     
			one := One(f);
			sqrtq := Sqrt(q);
      ## find element with norm -1
			w := First(AsList(f), t -> IsZero( t^(sqrtq+1) + one ) );
			if IsEvenInt(d) then   
				b := List([1..d/2], i-> w * id[2*i] + id[2*i-1]);
			else 
				b := List([1..(d-1)/2], i-> w * id[2*i] + id[2*i-1]);
			fi;
		elif type = "parabolic" then 
			b := id{List([1..(d-1)/2], i -> 2*i)};
		elif type = "hyperbolic" then 
			b := id{List([1..d/2], i -> 2*i-1)};
		elif type = "elliptic" then 
			b := id{List([1..(d/2-1)],i -> 2*i+1)};
		else Error( "type is unknown or not implemented");
		fi;
		return [b];
	end );

#############################################################################
# Part II: constructor methods, for the user of course.
#############################################################################

# CHECKED 21/09/11 jdb
# Cmat adapted 20/3/14
#############################################################################
#O  EllipticQuadric( <d>, <f> )
# returns the standard elliptic quadrac. See CanonicalQuadraticForm and CanonicalGramMatrix
# for details on the standard.
##
InstallMethod( EllipticQuadric, 
	"for an integer and a field",
	[ IsPosInt, IsField ],
	function( d, f )
		local eq,m,types,max,reps,q,form,creps;
		q := Size(f);
		if IsEvenInt(d) then
			Error("dimension must be odd");
			return;
		fi;
		if IsEvenInt(q) then
			m := CanonicalQuadraticForm("elliptic", d+1, f);
			form := QuadraticFormByMatrix(m, f);
		else 
			m := CanonicalGramMatrix("elliptic", d+1, f);  
			form := BilinearFormByMatrix(m, f);  
		fi; 
        eq := PolarSpaceStandard( form, true );
		SetRankAttr( eq, (d-1)/2 );
		types := TypesOfElementsOfIncidenceStructure( AmbientSpace(eq) );
		SetTypesOfElementsOfIncidenceStructure(eq, types{[1..(d-1)/2]});
		SetIsEllipticQuadric(eq, true);
		SetIsCanonicalPolarSpace(eq, true);
		SetPolarSpaceType(eq, "elliptic");
		if RankAttr(eq) = 2 then
			SetOrder( eq, [q, q^2]);
		fi;
		max := CanonicalOrbitRepresentativeForSubspaces("elliptic", d+1, f)[1];

	## Here we take the maximal totally isotropic subspace rep
	## max and make representatives for lower dimensional subspaces
	## it could be that max is empty, not Max of course, but the variable max.	

		if not IsEmpty(max) then
			reps := [ [max[1]] ]; #small change
			Append(reps, List([2..(d-1)/2], j -> max{[1..j]})); 
	## We would like the representative to be stored as
	## compressed matrices. Then they will be more efficient
	## to work with.
			
			creps := List(reps,x->NewMatrix(IsCMatRep, f, d+1, x));
			creps[1] := creps[1][1]; #max is not empty, first element should always be a 1xn matrix->cvec.
			
			#for m in reps do
			#	if IsMatrix(m) then
			#		ConvertToMatrixRep(m,f);
			#	else 
			#		ConvertToVectorRep(m,f);
			#	fi;
			#od;   

    ## Wrap 'em up #can be done without using VectorSpaceToElement (and hence withou computations) now :-)
			reps := List([1..(d-1)/2], j -> Wrap(eq, j, creps[j]) ); #one more small change :-)
			SetRepresentativesOfElements(eq, reps);
		else;
			SetRepresentativesOfElements(eq, []);
		fi;
		SetClassicalGroupInfo( eq, rec( degree := (q^((d+1)/2)+1)*(q^((d+1)/2-1)-1)/(q-1) ) );  
		return eq;
	end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  EllipticQuadric( <d>, <q> )
# returns the standard elliptic quadric. See EllipticQuadric method above.
##
InstallMethod( EllipticQuadric,
	"for two positive inteters <d> and <q>",	
	[ IsPosInt, IsPosInt ],
	function( d, q )  
		return EllipticQuadric(d,GF(q));
	end );

# CHECKED 21/09/11 jdb
# Cmat adapted 20/3/14
#############################################################################
#O  SymplecticSpace( <d>, <f> )
# returns the standard symplectic space. See CanonicalGramMatrix
# for details on the standard.
##
InstallMethod( SymplecticSpace,
	"for an integer and a field",
	[ IsPosInt, IsField ],
	function( d, f )
		local w,frob,m,types,max,reps,q,form,creps;
		if IsEvenInt(d) then
			Error("dimension must be odd");
			return;
		fi;

	## put compressed matrices here also
		q := Size(f);
		m := CanonicalGramMatrix("symplectic", d+1, f);     
		w := PolarSpaceStandard( BilinearFormByMatrix(m, f), true );
		SetRankAttr( w, (d+1)/2 );
		types := TypesOfElementsOfIncidenceStructure(AmbientSpace(w));
		if RankAttr(w) = 2 then
			SetOrder( w, [q, q]);
		fi;
		SetTypesOfElementsOfIncidenceStructure(w,types{[1..(d+1)/2]});
		SetIsSymplecticSpace(w,true);
		SetIsCanonicalPolarSpace(w, true);
		SetPolarSpaceType(w, "symplectic");
		max := CanonicalOrbitRepresentativeForSubspaces("symplectic", d+1, w!.basefield)[1];    

    ## Here we take the maximal totally isotropic subspace rep
    ## max and make representatives for lower dimensional subspaces
		reps := [ [ max[1] ] ]; #small change
		Append(reps, List([2..(d+1)/2], j -> max{[1..j]}));  

    ## We would like the representative to be stored as
    ## compressed matrices. Then they will be more efficient
    ## to work with.

		creps := List(reps,x->NewMatrix(IsCMatRep, f, d+1, x));
		creps[1] := creps[1][1]; #max is not empty, first element should always be a 1xn matrix->cvec.

		#for m in reps do
		#	if IsMatrix(m) then
		#		ConvertToMatrixRep(m,f);
		#	else 
		#		ConvertToVectorRep(m,f);
		#	fi;
		#od;   

    ## Wrap 'em up
		reps := List([1..(d+1)/2], j -> Wrap(w, j, creps[j]) ); #one more small change :-)
		SetRepresentativesOfElements(w, reps);
        SetClassicalGroupInfo( w, rec(  degree := (q^(d+1)-1)/(q-1) ) );    
		return w;
	end );


# CHECKED 21/09/11 jdb
#############################################################################
#O  SymplecticSpace( <d>, <q> )
# returns the standard symplectic space. See SymplecticSpace method above.
##
InstallMethod(SymplecticSpace, 
	"for an integer and an integer",
	[ IsPosInt, IsPosInt ],
	function( d, q ) 
		return SymplecticSpace(d, GF(q));
	end );

# CHECKED 21/09/11 jdb
# Cmat adapted 20/3/14
#############################################################################
#O  ParabolicQuadric( <d>, <f> )
# returns the standard parabolic quadric. See CanonicalQuadraticForm and CanonicalGramMatrix
# for details on the standard.
##
InstallMethod( ParabolicQuadric, 
	"for an integer and a field",	
	[ IsPosInt, IsField ],
	function( d, f )
		local pq,m,types,max,reps,q,form,creps;
		if IsOddInt(d) then
			Error("dimension must be even");
			return;
		fi;
		q := Size(f);
		if IsEvenInt(q) then
			m := CanonicalQuadraticForm("parabolic", d+1, f);
			form := QuadraticFormByMatrix(m, f);
		else 
			m := CanonicalGramMatrix("parabolic", d+1, f); 
			form := BilinearFormByMatrix(m, f);   
		fi; 
        pq := PolarSpaceStandard( form, true );
		SetRankAttr( pq, d/2 );
		types := TypesOfElementsOfIncidenceStructure( AmbientSpace(pq) );
		SetTypesOfElementsOfIncidenceStructure(pq, types{[1..d/2]});
		SetIsParabolicQuadric(pq, true);
		SetIsCanonicalPolarSpace(pq, true);
		SetPolarSpaceType(pq, "parabolic");
		if RankAttr(pq) = 2 then
			SetOrder( pq, [q, q]);
		fi;
		max := CanonicalOrbitRepresentativeForSubspaces("parabolic", d+1, f)[1];

    ## Here we take the maximal totally isotropic subspace rep
    ## max and make representatives for lower dimensional subspaces

		reps := [ [ max[1] ] ]; #small change
		Append(reps, List([2..d/2], j -> max{[1..j]}));  

    ## We would like the representative to be stored as
    ## compressed matrices. Then they will be more efficient
    ## to work with.
		creps := List(reps,x->NewMatrix(IsCMatRep, f, d+1, x));
		creps[1] := creps[1][1]; #max is not empty, first element should always be a 1xn matrix->cvec.
		
		#for m in reps do
		#	if IsMatrix(m) then
		#		ConvertToMatrixRep(m,f);
		#	else 
		#		ConvertToVectorRep(m,f);
		#	fi;
		#od;   

    ## Wrap 'em up
		reps := List([1..d/2], j -> Wrap(pq, j, creps[j]) ); #one more small change :-)
		SetRepresentativesOfElements(pq, reps);
        SetClassicalGroupInfo( pq, rec( degree := (q^(d/2)-1)/(q-1)*(q^((d+2)/2-1)+1) ) );   
		return pq;
	end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  ParabolicQuadric( <d>, <q> )
# returns the standard parabolic quadric. See ParabolicQuadric method above.
##
InstallMethod( ParabolicQuadric,
	"for two integers",
	[ IsPosInt, IsPosInt ],
	function( d, q ) 
		return ParabolicQuadric(d, GF(q));
	end );

# CHECKED 21/09/11 jdb
# Cmat adapted 20/3/14
#############################################################################
#O  HyperbolicQuadric( <d>, <f> )
# returns the standard hyperbolic quadric. See CanonicalQuadraticForm and CanonicalGramMatrix
# for details on the standard.
##
InstallMethod( HyperbolicQuadric, 
	"for an integer and a field",	
	[ IsPosInt, IsField ],
	function( d, f )
		local hq,m,types,max,reps,q,form,creps;
		q := Size(f);
		if IsEvenInt(d) then
			Error("dimension must be odd");
			return;
		fi;
		if IsEvenInt(q) then
			m := CanonicalQuadraticForm("hyperbolic", d+1, f);
			form := QuadraticFormByMatrix(m, f);
		else 
			m := CanonicalGramMatrix("hyperbolic", d+1, f);    
			form := BilinearFormByMatrix(m, f);
		fi; 
		hq := PolarSpaceStandard( form, true );
		SetRankAttr( hq, (d+1)/2 );
		types := TypesOfElementsOfIncidenceStructure( AmbientSpace(hq) );
		SetTypesOfElementsOfIncidenceStructure(hq, types{[1..(d+1)/2]});
		SetIsCanonicalPolarSpace(hq, true);
		SetIsHyperbolicQuadric(hq, true);
		SetPolarSpaceType(hq, "hyperbolic");
		if RankAttr(hq) = 2 then
			SetOrder( hq, [q, 1]);
		fi;
		max := CanonicalOrbitRepresentativeForSubspaces("hyperbolic", d+1, f)[1];
  
    ## Here we take the maximal totally isotropic subspace rep
    ## max and make representatives for lower dimensional subspaces

		reps := [ [ max[1] ] ]; #small change
		Append(reps, List([2..(d+1)/2], j -> max{[1..j]}));  

    ## We would like the representative to be stored as
    ## compressed matrices. Then they will be more efficient
    ## to work with.

		creps := List(reps,x->NewMatrix(IsCMatRep, f, d+1, x));
		creps[1] := creps[1][1]; #max is not empty, first element should always be a 1xn matrix->cvec.
  
		#for m in reps do
		#	if IsMatrix(m) then
		#		ConvertToMatrixRep(m,f);
		#	else 
		#		ConvertToVectorRep(m,f);
		#	fi;
		#od;   

    ## Wrap 'em up
		reps := List([1..(d+1)/2], j -> Wrap(hq, j, creps[j]) ); #one more small change :-)
		SetRepresentativesOfElements(hq, reps);
        SetClassicalGroupInfo( hq, rec( degree := (q^((d+1)/2)-1)/(q-1)*(q^((d+1)/2-1)+1)) );
		return hq;
	end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  HyperbolicQuadric( <d>, <q> )
# returns the standard hyperbolic quadric. See HyperbolicQuadric method above.
##
InstallMethod(HyperbolicQuadric, 
	"for a two integers",
	[ IsPosInt, IsPosInt ],
	function( d, q ) 
		return HyperbolicQuadric(d, GF(q));
	end );

# CHECKED 21/09/11 jdb
# Cmat adapted 20/3/14
#############################################################################
#O  HermitianPolarSpace( <d>, <f> )
# returns the standard hermitian variety. See CanonicalGramMatrix
# for details on the standard.
##
InstallMethod( HermitianPolarSpace, 
	"for an integer and a field",	
	[ IsPosInt, IsField ],
	function( d, f )
		local h,m,types,max,reps,q,creps,degree;
		if IsOddInt(DegreeOverPrimeField(f)) then
			Error("field order must be a square");
			return;
		fi;
		q := Sqrt(Size(f));
		m := CanonicalGramMatrix("hermitian", d+1, f);   
		h := PolarSpaceStandard( HermitianFormByMatrix(m, f), true );
		if d mod 2 = 0 then  
			SetRankAttr( h, d/2 );
		else
			SetRankAttr( h, (d+1)/2 );    
		fi;    
		types := TypesOfElementsOfIncidenceStructure( AmbientSpace(h) );
		SetTypesOfElementsOfIncidenceStructure(h, types{[1..RankAttr(h)]});
		SetIsHermitianPolarSpace(h, true);
		SetIsCanonicalPolarSpace(h, true);
		SetPolarSpaceType(h, "hermitian");
		if RankAttr(h) = 2 then
			if d = 3 then SetOrder( h, [q^2, q]); fi;
			if d = 4 then SetOrder( h, [q^2, q^3]); fi;
		fi;
		max := CanonicalOrbitRepresentativeForSubspaces("hermitian", d+1, f)[1];

    ## Here we take the maximal totally isotropic subspace rep
    ## max and make representatives for lower dimensional subspaces
		reps := [ [ max[1] ] ]; #small change
		Append(reps, List([2..RankAttr(h)], j -> max{[1..j]}));  

    ## We would like the representative to be stored as
    ## compressed matrices. Then they will be more efficient
    ## to work with.

		creps := List(reps,x->NewMatrix(IsCMatRep, f, d+1, x));
		creps[1] := creps[1][1]; #max is not empty, first element should always be a 1xn matrix->cvec.

		#for m in reps do
		#	if IsMatrix(m) then
		#		ConvertToMatrixRep(m,f);
		#	else
		#		ConvertToVectorRep(m,f);  
		#	fi;
		#od;   

    ## Wrap 'em up
		reps := List([1..RankAttr(h)], j -> Wrap(h, j, creps[j]) ); #one more small change :-)
		SetRepresentativesOfElements(h, reps);
		if d = 3 then
            degree := (q^3+1)*(q+1);
        else
            degree := (q^d-(-1)^d)*(q^(d+1)-(-1)^(d+1))/(q^2-1);
        fi;
        #SetClassicalGroupInfo( h, rec(  degree := (q^d-(-1)^d)*(q^(d+1)-(-1)^(d+1))/(q^2-1)) );
        SetClassicalGroupInfo( h, rec(  degree := degree ));
		return h;
	end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  HermitianPolarSpace( <d>, <q> )
# returns the standard hermitian polar space. See HermitianPolarSpace method above.
##
InstallMethod(HermitianPolarSpace,
	"for an two integers",
	[ IsPosInt, IsPosInt ],
	function( d, q ) 
		return HermitianPolarSpace(d, GF(q));
	end );

# ADDED 7/11/12 jdb
#############################################################################
#O  StandardPolarSpace( <ps> )
# returns the standard polar space isomorphic with <ps>.
##
InstallMethod( StandardPolarSpace, 
	"for a polar space",
	[ IsClassicalPolarSpace ],
	function( ps )
		local type, standard,d,f;
		d := ps!.dimension;
		f := ps!.basefield;
		type := PolarSpaceType( ps );
		if type = "hermitian" then
			standard := HermitianPolarSpace(d, f);               
			SetIsHermitianPolarSpace(ps, true);            
		elif type = "symplectic" then
			standard := SymplecticSpace(d, f);         
			SetIsSymplecticSpace(ps, true);
		elif type = "elliptic" then 
			standard := EllipticQuadric(d, f);         
			SetIsEllipticQuadric(ps, true);
		elif type = "parabolic" then
			standard := ParabolicQuadric(d, f);         
			SetIsParabolicQuadric(ps, true);
		elif type = "hyperbolic" then
			standard := HyperbolicQuadric(d, f);         
			SetIsHyperbolicQuadric(ps, true);
		fi;
		return standard;
	end );

# ADDED 30/3/14 jdb
#############################################################################
#O  IsCanonicalPolarSpace( <ps> )
# returns true if <ps> is canonical. Remind that canonical means similar to
# standard: in geometrical terms: the same geometry as a standard polar space
# with an underlying form that differs a factor with a standard form.
# Calling this operation also makes sure the ClassicalGroupInfo is set and
# the correct property. This mechanism makes sure that when a user constructs
# his favorite-non-standard-but-canonical polar space, it is recognised by
# FinInG, and only once a base change computation by forms will be done.
##
InstallMethod( IsCanonicalPolarSpace,
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
	local type, form, d, bf, st, gram, c1, c2, order, q, result;
	type := PolarSpaceType(ps); #a base change is computed in forms.
	d := ps!.dimension;
	bf := ps!.basefield;
	if HasQuadraticForm(ps) then
		form := QuadraticForm(ps);
		gram := GramMatrix(form);
		st := CanonicalQuadraticForm(type, d+1, bf); #returns a matrix.
	else
		form := SesquilinearForm(ps);
		gram := GramMatrix(form);
		st := CanonicalGramMatrix(type, d+1, bf);
	fi;
	q := Size(bf);
	if type = "elliptic" then
		SetIsEllipticQuadric(ps,true);
		order := (q^((d+1)/2)+1)*(q^((d+1)/2-1)-1)/(q-1);
	elif type = "parabolic" then
		SetIsParabolicQuadric(ps,true);
		order := (q^(d/2)-1)/(q-1)*(q^((d+2)/2-1)+1);
	elif type = "hyperbolic" then
		SetIsHyperbolicQuadric(ps,true);
		order := (q^((d+1)/2)-1)/(q-1)*(q^((d+1)/2-1)+1);
	elif type = "symplectic" then
		SetIsSymplecticSpace(ps,true);
		order := (q^(d+1)-1)/(q-1);
	elif type = "hermitian" then
		SetIsHermitianPolarSpace(ps,true);
		q := Sqrt(q);
        if d=3 then
            order := (q^3+1)*(q+1);
        else
            order := (q^d-(-1)^d)*(q^(d+1)-(-1)^(d+1))/(q^2-1);
        fi;
	fi;
	c1 := First(gram[1],x->not IsZero(x));
	if c1 = fail then
		return false;
	fi;
	c2 := First(st[1],x->not IsZero(x));
	result :=  c1*st = c2*gram;
	if result then
		SetClassicalGroupInfo( ps, rec(  degree := order ) );
	fi;
	return result;
end );

# ADDED 7/11/12 jdb
# Cmat adapted 20/3/14
#############################################################################
#O  CanonicalPolarSpace( <ps> )
# assume that f is the form determining <ps>, then this operation returns a 
# polar space <geo> determined by a form isometric with f, so <geo> is canonical 
# but not necessarily standard.
##
InstallMethod( CanonicalPolarSpace, 
	"for a polar space",
	[ IsClassicalPolarSpace ],
	function( ps )
		local type, canonicalps,d,f,form1,form2,isometric,b,mat1,mat2,canonicalmatrix,canonicalform, pq,types,max,reps,m,q,gram,creps;
		d := ps!.dimension;
		f := ps!.basefield;
		q := Size(f);
		type := PolarSpaceType( ps );
		if type = "hermitian" then
			canonicalps := HermitianPolarSpace(d, f);               
			SetIsHermitianPolarSpace(ps, true);            
		elif type = "symplectic" then
			canonicalps := SymplecticSpace(d, f);         
			SetIsSymplecticSpace(ps, true);
		elif type = "elliptic" then 
			canonicalps := EllipticQuadric(d, f);         
			SetIsEllipticQuadric(ps, true);
		elif type = "parabolic" then
			canonicalps := ParabolicQuadric(d, f);         
			SetIsParabolicQuadric(ps, true);
		elif type = "hyperbolic" then
			canonicalps := HyperbolicQuadric(d, f);         
			SetIsHyperbolicQuadric(ps, true);
		fi;
		if IsOddInt(Size(f)) and type in ["parabolic"] then
			form1 := QuadraticForm( ps );
			isometric := IsometricCanonicalForm(form1); #be careful with the notion Canonical in Forms package, its meaning is different than canonical in FinInG :-)
			gram := GramMatrix(isometric);
			canonicalmatrix := gram[1,1]*CanonicalGramMatrix("parabolic", d+1, f); 
			canonicalform := BilinearFormByMatrix(canonicalmatrix, f);
			canonicalps := PolarSpaceStandard( canonicalform, false );
			SetRankAttr( canonicalps, d/2 );
			types := TypesOfElementsOfIncidenceStructure( AmbientSpace(canonicalps) );
			SetTypesOfElementsOfIncidenceStructure(canonicalps, types{[1..d/2]});
			SetIsParabolicQuadric(canonicalps, true);
			SetIsCanonicalPolarSpace(canonicalps, true);
			SetIsStandardPolarSpace(canonicalps, false);
			SetPolarSpaceType(canonicalps, "parabolic");
			if RankAttr(canonicalps) = 2 then
				SetOrder( canonicalps, [q, q]);
			fi;
			max := CanonicalOrbitRepresentativeForSubspaces("parabolic", d+1, f)[1];

    ## Here we take the maximal totally isotropic subspace rep
    ## max and make representatives for lower dimensional subspaces

			reps := [ [ max[1] ] ]; #small change
			Append(reps, List([2..d/2], j -> max{[1..j]}));  

    ## We would like the representative to be stored as
    ## compressed matrices. Then they will be more efficient
    ## to work with.
  
			creps := List(reps,x->NewMatrix(IsCMatRep, f, d+1, x));
			creps[1] := creps[1][1]; #max is not empty, first element should always be a 1xn matrix->cvec.

			#for m in reps do
			#	if IsMatrix(m) then
			#		ConvertToMatrixRep(m,f);
			#	else 
			#		ConvertToVectorRep(m,f);
			#	fi;
			#od;   

    ## Wrap 'em up
			reps := List([1..d/2], j -> Wrap(canonicalps, j, creps[j]) ); #one more small change :-)
			SetRepresentativesOfElements(canonicalps, reps);
			SetClassicalGroupInfo( canonicalps, rec( degree := (q^(d/2)-1)/(q-1)*(q^((d+2)/2-1)+1) ) );
		fi;
		return canonicalps;
end );			


#############################################################################
# methods for some attributes.
#############################################################################
	
# CHECKED 25/09/11 jdb
#############################################################################
#A  QuadraticForm( <ps> )
# returns the quadratic form of which the polar space <ps> is the geometry.
# this is usefull since there are generic methods (q odd and even) thinkable 
# for orthogonal polar spaces. In such a case, we must use the quadratic form
# in the even case, and we can use it in the odd case. A typical example are 
# the enumerators. 
# IMPORTANT: this method will only be used when q is odd (and <ps> is orthogonal
# of course). In the q even case, the attribute will be set upon creation of the 
# polar space.
##
InstallMethod( QuadraticForm, 
	"for a polar space",
	[ IsClassicalPolarSpace ],
	function(ps)
		local form;
		form := SesquilinearForm(ps);
		if form!.type <> "orthogonal" then
			Error( "No quadratic form can be associated to this polar space" );
		fi;
		return QuadraticFormByBilinearForm(SesquilinearForm(ps));
	end );


# CHECKED 20/09/11 jdb
#############################################################################
#A  PolarSpaceType( <ps> )
# type of a polar space: see manual and conventions for orthogonal polar spaces.
##
InstallMethod( PolarSpaceType, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local ty, form, sesq;
		if HasQuadraticForm( ps ) then 
			form := QuadraticForm( ps );
		else 
			form := SesquilinearForm( ps );
		fi;
		ty := form!.type;
		if ty = "orthogonal" or ty = "quadratic" then
			if IsParabolicForm( form ) then
				ty := "parabolic";
			elif IsEllipticForm( form ) then
				ty := "elliptic";
			else
				ty := "hyperbolic";
			fi;
		fi;
		return ty;
	end );

# CHECKED 20/09/11 jdb
#############################################################################
#A  CompanionAutomorphism( <ps> )
# companion automorphism of polar space, i.e. the companion automorphism of 
# the sesquilinear form determining the polar space. If the form is quadratic, 
# or not hermitian, then the trivial automorphism is returned.
##
InstallMethod( CompanionAutomorphism, 
	"for a polar space",
	[ IsClassicalPolarSpace ],
	function( ps )
		local aut, type;
		type := PolarSpaceType( ps );
		aut := FrobeniusAutomorphism(ps!.basefield);
		if type = "hermitian" then
			aut := aut ^ (Order(aut)/2);
		else
			aut := aut ^ 0;
		fi;
		return aut;
	end );

#############################################################################
# jdb and ml will change ViewObj/PrintObj/Display methods now.`
# The ViewString is used e.g. in the ViewObj for flags.
#############################################################################

InstallMethod( ViewObj, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( p )
		Print("<polar space in ",AmbientSpace(p),": ",EquationForPolarSpace(p),"=0 >");
	end );

InstallMethod( ViewString, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( p )
		return Concatenation("<polar space in ",ViewString(AmbientSpace(p)),": ",String(EquationForPolarSpace(p)),"=0 >");
	end );

InstallMethod( ViewObj,
	"for an elliptic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsEllipticQuadric],
	function( p )
		Print("Q-(",p!.dimension,", ",Size(p!.basefield),"): ",EquationForPolarSpace(p),"=0");
	end );

InstallMethod( ViewObj,
	"for a standard elliptic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsEllipticQuadric and IsStandardPolarSpace],
	function( p )
		Print("Q-(",p!.dimension,", ",Size(p!.basefield),")");
	end );
	
InstallMethod( ViewString, 
	"for a standard elliptic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsEllipticQuadric and IsStandardPolarSpace],
	function( p )
		return Concatenation("Q-(",String(p!.dimension),", ",String(Size(p!.basefield)),")");
	end );

InstallMethod( ViewString, 
	"for an elliptic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsEllipticQuadric],
	function( p )
		return Concatenation("Q-(",String(p!.dimension),", ",
				String(Size(p!.basefield)),"): ",String(EquationForPolarSpace(p)),"=0");
	end );

InstallMethod( ViewObj,
	"for a symplectic space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsSymplecticSpace],
    function( p )
		Print("W(",p!.dimension,", ",Size(p!.basefield),"): ",EquationForPolarSpace(p),"=0");
	end );

InstallMethod( ViewObj,
	"for a standard symplectic space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsSymplecticSpace and IsStandardPolarSpace],
    function( p )
		Print("W(",p!.dimension,", ",Size(p!.basefield),")");
	end );

InstallMethod( ViewString, 
	"for a standard symplectic space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsSymplecticSpace and IsStandardPolarSpace],
	function( p )
		return Concatenation("W(",String(p!.dimension),", ",String(Size(p!.basefield)),")");
	end );

InstallMethod( ViewString, 
	"for a symplectic space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsSymplecticSpace],
	function( p )
		return Concatenation("W(",String(p!.dimension),", ",String(Size(p!.basefield)),"): ",
				EquationForPolarSpace(p),"=0");
	end );

InstallMethod( ViewObj,
	"for a parabolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric ],
    function( p )
		Print("Q(",p!.dimension,", ",Size(p!.basefield),"): ",EquationForPolarSpace(p),"=0");
	end);

InstallMethod( ViewObj,
	"for a standard parabolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric and IsStandardPolarSpace ],
    function( p )
		Print("Q(",p!.dimension,", ",Size(p!.basefield),")");
	end);

InstallMethod( ViewString, 
	"for a standard parabolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric and IsStandardPolarSpace ],
	function( p )
		return Concatenation("Q(",String(p!.dimension),", ",String(Size(p!.basefield)),")");
	end );

InstallMethod( ViewString, 
	"for a standard parabolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric ],
	function( p )
		return Concatenation("Q(",String(p!.dimension),", ",String(Size(p!.basefield)),"): ",
			String(EquationForPolarSpace(p)),"=0");
	end );

InstallMethod( ViewObj,
	"for a parabolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric ],
    function( p )
		Print("Q(",p!.dimension,", ",Size(p!.basefield),"): ",EquationForPolarSpace(p),"=0");
	end);

InstallMethod( ViewObj,
	"for a hyperbolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHyperbolicQuadric ],
    function( p )
		Print("Q+(",p!.dimension,", ",Size(p!.basefield),"): ",EquationForPolarSpace(p),"=0");
	end);

InstallMethod( ViewObj,
	"for a standard hyperbolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHyperbolicQuadric and IsStandardPolarSpace],
    function( p )
		Print("Q+(",p!.dimension,", ",Size(p!.basefield),")");
	end);

InstallMethod( ViewString, 
	"for a standard hyperbolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHyperbolicQuadric and IsStandardPolarSpace],
	function( p )
		return Concatenation("Q+(",String(p!.dimension),", ",String(Size(p!.basefield)),")");
	end );

InstallMethod( ViewString, 
	"for a hyperbolic quadric",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHyperbolicQuadric],
	function( p )
		return Concatenation("Q+(",String(p!.dimension),", ",
				String(Size(p!.basefield)),"): ",String(EquationForPolarSpace(p)),"=0");
	end );

InstallMethod( ViewObj,
	"for a hermitian variety",
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianPolarSpace ],
    function( p )
		Print("H(",p!.dimension,", ",Sqrt(Size(p!.basefield)),"^2): ",EquationForPolarSpace(p),"=0");
	end);

InstallMethod( ViewObj,
	"for a standard hermitian variety",
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianPolarSpace and IsStandardPolarSpace],
    function( p )
		Print("H(",p!.dimension,", ",Sqrt(Size(p!.basefield)),"^2)");
    end);

InstallMethod( ViewString, 
	"for a standard hermitian quadric",
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianPolarSpace and IsStandardPolarSpace],
	function( p )
		return Concatenation("H(",String(p!.dimension),", ",String(Sqrt(Size(p!.basefield))),"^2)");
	end );

InstallMethod( ViewString, 
	"for a hermitian quadric",
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianPolarSpace],
	function( p )
		return Concatenation("H(",String(p!.dimension),", ",String(Sqrt(Size(p!.basefield))),"^2): ",
			EquationForPolarSpace(p),"=0");
	end );

InstallMethod( PrintObj, [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
  function( p )
    Print("PolarSpace(\n");
    if HasQuadraticForm(p) then
       Print(QuadraticForm(p));
    else
       Print(SesquilinearForm(p));
    fi;
    Print(", ", p!.basefield, " )");
  end );

InstallMethod( Display, [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
# fixed the output for rank (11/11/14 ml)
  function( p )
    Print("<polar space of rank ",RankAttr(p)," in PG(", p!.dimension, ", ", Size(p!.basefield), ")",">\n");
    if HasQuadraticForm(p) then
       Display(QuadraticForm(p));
    fi;
    Display(SesquilinearForm(p));
    #if HasDiagramOfGeometry( p ) then
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

InstallMethod( PrintObj,
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsEllipticQuadric ],
        function( p )
          Print("EllipticQuadric(",p!.dimension,",",p!.basefield,"): ",EquationForPolarSpace(p),"=0");
        end );

InstallMethod( Display, 
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsEllipticQuadric ],
  function( p )
    Print("Q-(",p!.dimension,", ",Size(p!.basefield),")\n");
    if HasQuadraticForm(p) then
       Display(QuadraticForm(p));
    fi;
    Display(SesquilinearForm(p));
    #if HasDiagramOfGeometry( p ) then
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

InstallMethod( PrintObj,
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsSymplecticSpace ],
        function( p )
          Print("SymplecticSpace(",p!.dimension,",",p!.basefield,"): ",EquationForPolarSpace(p),"=0");
  end);

InstallMethod( Display, 
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsSymplecticSpace ],
  function( p )
    Print("W(",p!.dimension,", ",Size(p!.basefield),")\n");
    Display(SesquilinearForm(p));
    #if HasDiagramOfGeometry( p ) then
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

InstallMethod( PrintObj,
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric ],
        function( p )
          Print("ParabolicQuadric(",p!.dimension,",",p!.basefield,"): ",EquationForPolarSpace(p),"=0");
  end);

InstallMethod( Display, 
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsParabolicQuadric ],
  function( p )
    Print("Q(",p!.dimension,", ",Size(p!.basefield),")\n");
    if HasQuadraticForm(p) then
       Display(QuadraticForm(p));
    fi;
    Display(SesquilinearForm(p));
    #if HasDiagramOfGeometry( p ) then
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

InstallMethod( PrintObj,
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHyperbolicQuadric ],
        function( p )
          Print("HyperbolicQuadric(",p!.dimension,",",p!.basefield,"): ",EquationForPolarSpace(p),"=0");
        end);

InstallMethod( Display, 
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHyperbolicQuadric ],
  function( p )
    Print("Q+(",p!.dimension,", ",Size(p!.basefield),")\n");
    if HasQuadraticForm(p) then
       Display(QuadraticForm(p));
    fi;
    Display(SesquilinearForm(p));
    #if HasDiagramOfGeometry( p ) then
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

InstallMethod( PrintObj,
  [IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianPolarSpace ],
        function( p )
          Print("HermitianPolarSpace(",p!.dimension,",",p!.basefield,"): ",EquationForPolarSpace(p),"=0");
        end);

InstallMethod( Display, 
  [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep and IsHermitianPolarSpace ],
  function( p )
    Print("H(",p!.dimension,", ",Size(p!.basefield),")\n");
    Display(SesquilinearForm(p));
    #if HasDiagramOfGeometry( p ) then
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

#############################################################################
## The following methods are needed for "naked" polar spaces; those
## that on conception are devoid of many of the attributes that
## the "canonical" polar spaces exhibit (see the code for the
## "SymplecticSpace" as an example).
#############################################################################

# CHECKED 21/09/11 jdb
#############################################################################
#O  IsomorphismCanonicalPolarSpace( <ps> )
# This method returns a geometry morphism that is in fact the coordinate transformation
# going FROM the standard polar space TO the given polar space <ps>
##
InstallMethod( IsomorphismCanonicalPolarSpace, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local d, f, type, canonical, iso, info;
		d := ps!.dimension;
		f := ps!.basefield;
		type := PolarSpaceType( ps );
		if type = "hermitian" then
			canonical := HermitianPolarSpace(d, f);               
			SetIsHermitianPolarSpace(ps, true);            
		elif type = "symplectic" then
			canonical := SymplecticSpace(d, f);         
			SetIsSymplecticSpace(ps, true);
		elif type = "elliptic" then 
			canonical := EllipticQuadric(d, f);         
			SetIsEllipticQuadric(ps, true);
		elif type = "parabolic" then
			canonical := ParabolicQuadric(d, f);         
			SetIsParabolicQuadric(ps, true);
		elif type = "hyperbolic" then
			canonical := HyperbolicQuadric(d, f);         
			SetIsHyperbolicQuadric(ps, true);
		fi;
		iso := IsomorphismPolarSpacesNC(canonical, ps, false);      ## jb: no longer computes nice monomorphism here
		info := ClassicalGroupInfo( canonical );
		SetClassicalGroupInfo( ps, rec( degree := info!.degree ) );
		return iso;
	end );

#############################################################################
#O  IsomorphismCanonicalPolarSpaceWithIntertwiner( <ps> )
# This method returns a geometry morphism that is in fact the coordinate transformation
# going FROM the standard polar space TO the given polar space <ps>, with intertwiner
##
InstallMethod( IsomorphismCanonicalPolarSpaceWithIntertwiner, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local d, f, type, canonical, iso, info;
		d := ps!.dimension;
		f := ps!.basefield;
		type := PolarSpaceType( ps );
		if type = "hermitian" then
			canonical := HermitianPolarSpace(d, f);               
			SetIsHermitianPolarSpace(ps, true);            
		elif type = "symplectic" then
			canonical := SymplecticSpace(d, f);         
			SetIsSymplecticSpace(ps, true);
		elif type = "elliptic" then 
			canonical := EllipticQuadric(d, f);         
			SetIsEllipticQuadric(ps, true);
		elif type = "parabolic" then
			canonical := ParabolicQuadric(d, f);         
			SetIsParabolicQuadric(ps, true);
		elif type = "hyperbolic" then
			canonical := HyperbolicQuadric(d, f);         
			SetIsHyperbolicQuadric(ps, true);
		fi;
		iso := IsomorphismPolarSpacesNC(canonical, ps, true); 
		info := ClassicalGroupInfo( canonical );
		SetClassicalGroupInfo( ps, rec( degree := info!.degree ) );
		return iso;
	end );

#############################################################################
#More attributes...
#############################################################################

# CHECKED 21/09/11 jdb
#############################################################################
#O  RankAttr( <ps> )
# returns the rank of the polar space <ps>, which is the Witt index of the
# defining form.
##
InstallMethod( RankAttr,
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local iso;
		iso := IsomorphismCanonicalPolarSpace( ps );
		return RankAttr(Source(iso)!.geometry);
  end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  TypesOfElementsOfIncidenceStructure( <ps> )
# Well known method that returns a list with the names of the elements of 
# the polar space <ps>
##
InstallMethod( TypesOfElementsOfIncidenceStructure, 
	"for a polar space ",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local iso;
		iso := IsomorphismCanonicalPolarSpace( ps );
		return TypesOfElementsOfIncidenceStructure(Source(iso)!.geometry);
	end );

#The next two methods are wrong. THe TypesOfElementsOfIncidenceStructure are determined by the
#rank of the polar space, not the projective dimenion.
#InstallMethod( TypesOfElementsOfIncidenceStructure, "for a polar space", 
#  [IsClassicalPolarSpace],
#  function( ps )
#    local d,i,types;
#    types := ["point"];
#    d := ProjectiveDimension(ps);
#    if d >= 2 then Add(types,"line"); fi;
#    if d >= 3 then Add(types,"plane"); fi;
#    if d >= 4 then Add(types,"solid"); fi;
#    for i in [5..d] do
#        Add(types,Concatenation("proj. subspace of dim. ",String(i-1)));
#    od;
#    return types;
#  end );

#InstallMethod( TypesOfElementsOfIncidenceStructurePlural, "for a polar space",
#  [IsClassicalPolarSpace],
#  function( ps )
#    local d,i,types;
#    types := ["points"];
#    d := ProjectiveDimension(ps);
#    if d >= 2 then Add(types,"lines"); fi;
#    if d >= 3 then Add(types,"planes"); fi;
#    if d >= 4 then Add(types,"solids"); fi;
#    for i in [5..d] do
#        Add(types,Concatenation("proj. subspaces of dim. ",String(i-1)));
#    od;
#    return types;
#  end );

# CHECKED 22/09/11 jdb
#############################################################################
#O  TypesOfElementsOfIncidenceStructurePlural( <ps> )
# Well known method that returns a list with the names of the elements of 
# the polar space <ps>
##
InstallMethod( TypesOfElementsOfIncidenceStructurePlural, 
	"for a polar space",  
	[IsClassicalPolarSpace],
	function( ps )
		local d,i,types;
		types := ["points"];
		d := Rank(ps);
		if d >= 2 then Add(types,"lines"); fi;
		if d >= 3 then Add(types,"planes"); fi;
		if d >= 4 then Add(types,"solids"); fi;
		for i in [5..d] do
			Add(types,Concatenation("proj. subspaces of dim. ",String(i-1)));
		od;
		return types;
	end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  Order( <ps> )
# If <ps> has rank two, then it is a GQ, and we can ask its order.
##
InstallOtherMethod( Order, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		if RankAttr(ps) = 2 then
			return Order(Source(IsomorphismCanonicalPolarSpace(ps) )!.geometry );
		else
			Error("Rank is not 2");
		fi;
	end );
  
# CHECKED 21/09/11 jdb
#############################################################################
#O  RepresentativesOfElements( <ps> )
# returns an element of each type of the polar space <ps>
##
InstallMethod( RepresentativesOfElements, 
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local iso, reps;
		iso := IsomorphismCanonicalPolarSpace( ps );
		reps := RepresentativesOfElements( Source( iso )!.geometry );
		return ImagesSet(iso, reps);
	end );

#############################################################################
# one more operation...
#############################################################################

# CHECKED 21/09/11 jdb
#############################################################################
#O  \/( <ps>, <v> )
# returns the quotien space of the element <v>, which must be an element of the
# polar space <ps>
##
InstallOtherMethod(\/,
	"for a polar space and an element of a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep, IsSubspaceOfClassicalPolarSpace],
	function( ps, v )
		if not v in ps then 
			Error( "Subspace does not belong to the polar space" );
		fi;
		return Range( NaturalProjectionBySubspace( ps, v ) )!.geometry;
	end );

#############################################################################
# Counting the number of elements of the polar spaces
#############################################################################

# CHECKED 21/09/11 jdb
# question: can we not use the NumberOfTotallySingularSubspaces for this?
#############################################################################
#O  Size( <vs> )
# returns the number of elements on <vs>, a collection of elements of a polar
# space of a given type.
##
InstallMethod( Size, 
	"for a collection of elements of a polar space",
	[IsSubspacesOfClassicalPolarSpace],
	function( vs )
		local geom, q, d, m, ovnum, ordominus, numti, gauss, flavour;
		geom := vs!.geometry; 
		q := Size(vs!.geometry!.basefield);
		d := vs!.geometry!.dimension + 1;
		m := vs!.type;
		flavour := PolarSpaceType(geom);
        if flavour = "symplectic" then
			return Product(List([0..(m-1)],i->(q^(d-2*i)-1)/(q^(i+1)-1)));
		fi;
		if flavour = "elliptic" then
			gauss:=Product(List([0..(m-1)],i->(q^(d/2-1)-q^i)/(q^m-q^i)));
			numti:=gauss * Product(List([0..(m-1)],j->q^(d/2-j)+1));
			return numti;
		fi;
		if flavour = "hyperbolic" then
			gauss:=Product(List([0..(m-1)],i->(q^(d/2)-q^i)/(q^m-q^i)));
			numti:=gauss * Product(List([0..(m-1)],j->q^(d/2-1-j)+1));
			return numti;
		fi;
		if flavour = "parabolic" then
			gauss:=Product(List([0..(m-1)],i->(q^((d-1)/2)-q^i)/(q^m-q^i)));
			numti:=gauss * Product(List([0..(m-1)],j->q^( (d+1)/2-1-j)+1));
			return numti;
		fi;
		if flavour = "hermitian" then
			return Product(List([(d+1-2*m)..d],i->(Sqrt(q)^i-(-1)^i)))/
			Product(List([1..m],j->q^j-1 ));
		fi;
		Error("Unknown polar space type!");
	end );

#############################################################################
# Elements -- Subspaces
#############################################################################

## The following needs more work: I think it's ok now. jdb
## jb 19/02/2009: Just changed the compressed matrix methods
##               so that length 1 matrices are allowed.

###cmat methods need to be added here. VectorSpaceToElement checks wheter 
# a vectorspace really makes an element of a polar space by checking 
# whether it is totally isotropic. Therefore we need some forms functionality.
# Since forms is not working with cmat/cvecs, and this will not change, we have
# to unpack a cmat before piping it to forms. Hence it makes sens to create VectorSpaceToElement
# methods the lazy way: just unpack and pipe to an existing VectorSpaceToElement method.
###

# Added 20/3/14 jdb
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. 
##
InstallMethod( VectorSpaceToElement, 
	"for a polar space and a CMat",
	[IsClassicalPolarSpace, IsCMatRep],
	function( geom, v )
		return VectorSpaceToElement(geom,Unpack(v));
	end );
	
# Added 20/3/14 jdb
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. 
##
InstallMethod( VectorSpaceToElement, 
	"for a polar space and a cvec",
	[IsClassicalPolarSpace, IsCVecRep],
	function( geom, v )
		return VectorSpaceToElement(geom,Unpack(v));
	end );

# CHECKED 21/09/11 jdb
# cmat change 20/3/14.
# changed 19/01/16 (jdb): by a change of IsPlistRep, this method gets also
# called when using a row vector, causing a problem with TriangulizeMat.
# a solution was to add IsMatrix.
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
##
# JB: Fixed a strange error in here
# I had Q+(11,3) (with a different form than usual) and tried to wrap
# [ [ 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3), Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ] ]
# It kept on returning 0*Z(3).
# JDB: I kept John's historical note, but the code where the bug was, has disappeared with the cmat adaptation :-)
# Information on NewMatrix us here can be found in projectivespaces.gi, method for VectorSpaceToElement.
#
InstallMethod( VectorSpaceToElement, 
	"for a polar space and a Plist",
	[IsClassicalPolarSpace, IsPlistRep and IsMatrix],
	function( geom, v )
		local  x, n, i, y;
		## when v is empty... 
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;
        if not IsMatrix(v) then #next 3 lines: patch of Max Horn (16/2/16)
            TryNextMethod();
        fi;

		x := MutableCopyMat(v);
		TriangulizeMat(x);
		## dimension should be correct
		if Length(v[1]) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## Remove zero rows. It is possible the the user
		## has inputted a matrix which does not have full rank
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		if i = n then
			return EmptySubspace(geom);
		fi;
		x := x{[1..n-i]};
		#if we check later on the the subspace is an element of the polar space, then
		#the user cannot create the whole polar space like this, so I commented out the
		# next three lines.
		#if Length(x)=ProjectiveDimension(geom)+1 then
		#	return geom;
		#fi;
        ## check here that it is in the polar space. 
		if HasQuadraticForm(geom) then
			if not IsTotallySingularSubspace(QuadraticForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
			fi;
		else
			if not IsTotallyIsotropicSubspace(SesquilinearForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
			fi;
		fi;
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		if Length(y) = 1 then
			return Wrap(geom, 1, y[1]);
		else
			return Wrap(geom, Length(y), y);
		fi;
	end );

# CHECKED 21/09/11
# cmat changed 20/3/14.
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
##
InstallMethod( VectorSpaceToElement,	
	"for a compressed GF(2)-matrix",
	[IsClassicalPolarSpace, IsGF2MatrixRep],
	function( geom, v )
		local  x, n, i, y;
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;
		x := MutableCopyMat(v);
		TriangulizeMat(x);
		## remove zero rows
		n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		if i = n then
			return EmptySubspace(geom);
		fi;
		x := x{[1..n-i]};
		#x := x{[1..n-i]};
		#if we check later on the the subspace is an element of the polar space, then
		#the user cannot create the whole polar space like this, so I commented out the
		# next three lines.
		#if Length(x)=ProjectiveDimension(geom)+1 then
		#	return geom;
		#fi;
        ## check here that it is in the polar space. 
		if HasQuadraticForm(geom) then
			if not IsTotallySingularSubspace(QuadraticForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
			fi;
		else
			if not IsTotallyIsotropicSubspace(SesquilinearForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
			fi;
		fi;
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		if Length(y) = 1 then
			return Wrap(geom, 1, y[1]);
		else
			return Wrap(geom, Length(y), y);
		fi;
  end );
  
# CHECKED 21/09/11
# cmat changed 20/3/14.
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
##
InstallMethod( VectorSpaceToElement, 
	"for a compressed basis of a vector subspace",
	[IsClassicalPolarSpace, Is8BitMatrixRep],
	function( geom, v )
		local  x, n, i,y; 
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;
		x := MutableCopyMat(v);
		TriangulizeMat(x);
	## dimension should be correct
		if Length(v[1]) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;	
	## remove zero rows
		n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		if i = n then
			return EmptySubspace(geom);
		fi;
		x := x{[1..n-i]};
		#if we check later on the the subspace is an element of the polar space, then
		#the user cannot create the whole polar space like this, so I commented out the
		# next three lines.
		#if Length(x)=ProjectiveDimension(geom)+1 then
		#	return geom;
		#fi;
      
		## check here that it is in the polar space. 
		if HasQuadraticForm(geom) then
			if not IsTotallySingularSubspace(QuadraticForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
			fi;
		else
			if not IsTotallyIsotropicSubspace(SesquilinearForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
			fi;
		fi;
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		if Length(y) = 1 then
			return Wrap(geom, 1, y[1]);
		else
			return Wrap(geom, Length(y), y);
		fi;
end );

# CHECKED 21/09/11 jdb
# cvec version 20/3/14
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. Several checks are built in.
##
InstallMethod( VectorSpaceToElement, 
	"for a row vector",
	[IsClassicalPolarSpace, IsRowVector],
	function( geom, v )
		local  x,y;
		## dimension should be correct
		if Length(v) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		x := ShallowCopy(v);
		if IsZero(x) then
			return EmptySubspace(geom);
		else
			if HasQuadraticForm(geom) then
				if not IsSingularVector(QuadraticForm(geom),x) then
					Error("<v> does not generate an element of <geom>");
				fi;
			else
				if not IsIsotropicVector(SesquilinearForm(geom),x) then
					Error("<v> does not generate an element of <geom>");
				fi;
			fi;
			MultVector(x,Inverse( x[PositionNonZero(x)] ));
            y := NewMatrix(IsCMatRep,geom!.basefield,Length(x),[x]);
			return Wrap(geom, 1, y[1]);
		fi;
  end );

# CHECKED 22/09/11 jdb 
# cvec version 20/3/14
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. Several checks are built in.
##
InstallMethod( VectorSpaceToElement, 
	"for a polar space and an 8-bit vector",
	[IsClassicalPolarSpace, Is8BitVectorRep],
	function( geom, v )
		local  x, n, i,y;
		## when v is empty...
		if IsEmpty(v) then
			return EmptySubspace(geom);
		fi;
		x := ShallowCopy(v);
		## dimension should be correct
		if Length(v) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## We must also compress our vector.
		#ConvertToVectorRep(x, geom!.basefield);
		## bad characters, such as jdb, checked this with input zero vector...
		if IsZero(x) then
			return EmptySubspace(geom);
		else
			if HasQuadraticForm(geom) then
				if not IsSingularVector(QuadraticForm(geom),x) then
					Error("<v> does not generate an element of <geom>");
				fi;
			else
				if not IsIsotropicVector(SesquilinearForm(geom),x) then
					Error("<v> does not generate an element of <geom>");
				fi;
			fi;			
			MultVector(x,Inverse( x[PositionNonZero(x)] ));
			y := NewMatrix(IsCMatRep,geom!.basefield,Length(x),[x]);
			return Wrap(geom, 1, y[1]);
		fi;
	end );

# CHECKED 22/09/11 jdb  
# Changed 28/11/11 jdb + pc, according to remark in tiny optimalisations.
# added a comment 30/11/11
# cmat version: Unpack before piping to forms. (20/3/14)
#############################################################################
#O  \in( <w>, <ps> ) true if the element <w> is contained in <ps>
# remarks: should we change this? I mean: this method makes a projective subspace
# suddenly into a polar space subspace, if this thest is true. Can cause weird thing 
# when displaying objects. We changed it, see commented out stuff below.
# 30/11/11: caveat: just doing w!.geo := ps; changes the ambient geometry of x,
# but it does not change the categories of x. So maybe x belongs to the polar space then
# but not to the category IsSubspaceOfClassicalPolarSpace. So some operations might stay
# unapplicable anyway. So changing w!.geo has no advantages. 
##
#I changed this, I now use the built in functions of forms to perform the test.
#jdb 6/1/8
InstallMethod( \in, 
	"for an element of a polar space and a polar space",
	[IsElementOfIncidenceStructure, IsClassicalPolarSpace],
	function( w, ps )
		local form, r, ti;
    # if the vector spaces don't agree we can't go any further
		if w!.geo!.dimension <> ps!.dimension or
			not IsSubset(ps!.basefield, w!.geo!.basefield) then
			return false;
		fi;
		r := Unpack(w!.obj); #here is the cmat change.
		if w!.type = 1 then r := [r]; fi;
    # check if the subspace is totally isotropic/singular
		if HasQuadraticForm(ps) then
			form := QuadraticForm(ps);
			ti := IsTotallySingularSubspace(form,r);
		else
			form := SesquilinearForm(ps);
			ti:= IsTotallyIsotropicSubspace(form,r); 
		fi;
    # if yes, make it an element of the polar space.
	#	if not IsSubspaceOfClassicalPolarSpace(w) and ti then
	#		w!.geo := ps;
	#	fi;
		return ti;
	end );

#ADDED 30/11/2011 jdb.
#############################################################################
#O  Span( <x>, <y>, <b> )
# returns <x,y>, <x> and <y> two subspaces of a polar space and a boolean
##
InstallMethod( Span, 
	"for two subspaces of a polar space",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsBool],
	function( x, y, b )
	## This method is quicker than the old one
	local ux, uy, typx, typy, span, vec, spanel;
	if not b then
	  return Span(x,y);
	fi;
	typx := x!.type;
	typy := y!.type;
	vec := x!.geo!.vectorspace;
	if vec = y!.geo!.vectorspace then
		ux := Unwrap(x);
		uy := Unwrap(y);
		if typx = 1 then ux := [ux]; fi;
		if typy = 1 then uy := [uy]; fi;
		span := SumIntersectionMat(ux, uy)[1];	
		if Length(span) < vec!.DimensionOfVectors then
			spanel := VectorSpaceToElement( AmbientSpace(x!.geo), span);
			if IsIdenticalObj(x!.geo,y!.geo) then
				if spanel in x!.geo then
				  return VectorSpaceToElement( x!.geo, span);
				else
				  return spanel;
				fi;
			else
				return spanel;
			fi;
		else
			return AmbientSpace(x!.geo);
		fi;
	else
		Error("Subspaces belong to different ambient spaces");
	fi;
	end );
	


#ADDED 30/11/2011 jdb.
# cvec note (20/3/14): SumIntersectionMat seems to be incompatible with
# cvecs. So I will Unpack, do what I have to do, and get what I want
# since VectorSpaceToElement is helping here.
#############################################################################
#O  Meet( <x>, <y> )
# returns the intersection of <x> and <y>, two subspaces of a polar space.
##
InstallMethod( Meet,
	"for two subspaces of a polar space",
	[IsSubspaceOfClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace],
	function( x, y )
		local ux, uy, typx, typy, int, f, rk;
		typx := x!.type;
		typy := y!.type;
		if x!.geo!.vectorspace = y!.geo!.vectorspace then 
			ux := Unpack(Unwrap(x)); 
			uy := Unpack(Unwrap(y));
			if typx = 1 then ux := [ux]; fi;
			if typy = 1 then uy := [uy]; fi;
			f := x!.geo!.basefield; 
			int := SumIntersectionMat(ux, uy)[2];
			if not int=[] then 
				if IsIdenticalObj(x!.geo,y!.geo) then
					return VectorSpaceToElement(x!.geo,int);
				else
					return VectorSpaceToElement( AmbientSpace(x), int);
				fi;
			else 
				return EmptySubspace(AmbientSpace(x));
			fi;
		else
			Error("Subspaces belong to different ambient spaces");
		fi;
  end );

#############################################################################
# Collection of elements (of given type)
#############################################################################

# CHECKED 22/09/11 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <ps>, <j> )
# returns the elements of the polar space <ps> of type <j>
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for a polar space and an integer",
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep, IsPosInt],
	function( ps, j )
		local r;
		r := Rank(ps);
		if j > r then
			Error("<geo> has no elements of type <j>");
		else
			#Print("grapje\n");
			return Objectify(
			NewType( ElementsCollFamily, IsSubspacesOfClassicalPolarSpace and
                                     IsSubspacesOfClassicalPolarSpaceRep ),
			rec(
				geometry := ps,
				type := j,
				size := NumberOfTotallySingularSubspaces(ps, j)
				)
			);
		fi;
	end);

# CHECKED 22/09/11 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <ps> )
# returns all the elements of the polar space <ps> 
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for a polar space",
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep],
	function( ps )
		return Objectify(
			NewType( ElementsCollFamily, IsAllElementsOfIncidenceStructure ),
			rec( geometry := ps )
				);
	end);

#the next four operations are not necessary any more, the generic methods for them
# in Lie geometry are used.

#InstallMethod( Points, [IsClassicalPolarSpace],
#  function( ps )
#    return ElementsOfIncidenceStructure(ps, 1);
#  end);

#InstallMethod( Lines, [IsClassicalPolarSpace],
#  function( ps )
#    return ElementsOfIncidenceStructure(ps, 2);
#  end);

#InstallMethod( Planes, [IsClassicalPolarSpace],
#  function( ps )
#    return ElementsOfIncidenceStructure(ps, 3);
#  end);

#InstallMethod( Solids, [IsClassicalPolarSpace],
#  function( ps )
#    return ElementsOfIncidenceStructure(ps, 4);
#  end);


# CHECKED 22/09/11 jdb
# question: can we use this function for Size?
#############################################################################
#O  NumberOfTotallySingularSubspaces( <ps>, <j> )
# returns the number of elements of type <j> of the polar space <ps>
## 
InstallMethod( NumberOfTotallySingularSubspaces, 
	"for a polar space and an integer",
	[IsClassicalPolarSpace, IsPosInt],
	function(ps, j)

  ## In a classical finite polar space of rank r with parameters (q,q^e), 
  ## the number of subspaces of algebraic dimension j is given by
  ## [d, n] Prod_{i=1}^{n} (q^{r+e-i}+1)

		local r, d, type, e, q, qe;
		r := RankAttr(ps);
		d := ps!.dimension;
		type := PolarSpaceType(ps);
		q := Size(ps!.basefield); 
		if type = "elliptic" then e := 2; qe := q^e;
		elif type = "hyperbolic" then e:= 0; qe := q^e;
		elif type = "parabolic" or type = "symplectic" then e:=1; qe := q^e;
		elif type = "hermitian" and IsEvenInt(ps!.dimension) then e:=3; 
			qe := Sqrt(q)^e;
		elif type = "hermitian" and IsOddInt(ps!.dimension) then e:=1; 
			qe := Sqrt(q)^e;
		else Error("Polar space doesn't know its type!");
		fi;

		return Size(Subspaces(GF(q)^r, j)) * Product(List([1..j], i -> q^(r-i) * qe +1));
	end);

# CHECKED 17/03/14 jdb
# cmat version. Same consideration as elsewhere: forms does not use cmats, so unpack before piping to form.
#############################################################################
#O  TypeOfSubspace( <ps>, <w> )
# returns the type of the polar space induced by <ps> in the projective subspace
# <w>
## 
InstallMethod( TypeOfSubspace,
	"for a polar space and a subspace of a projective geometry",
     [ IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],
	function( ps, w )
  
    ## At the moment, this is a helper operation, but perhaps
    ## it could be used by the user. This operation returns
    ## "degenerate", "hermitian", "symplectic", "elliptic",
    ## "hyperbolic", or "parabolic".
    
		local pstype, dim, type, mat, gf, q, form, newform,uw;
		pstype := PolarSpaceType( ps );
		gf := ps!.basefield;             
		q := Size(gf);
		dim := w!.type;

		if pstype in ["elliptic", "parabolic", "hyperbolic"] and IsEvenInt(q) then
			form := QuadraticForm( ps );
			uw := Unpack(w!.obj); #here is an unpack cmat change
			mat := uw * form!.matrix * TransposedMat(uw);
			newform := QuadraticFormByMatrix( mat, gf );

#  jdb makes a change here. it was 'IsDegenerateForm( newform )' but it is clear that we want to test whether
#  the quadratic form is *singular*, according to the definition in forms about degenerecy and singularity

			if IsSingularForm( newform ) then
				return "degenerate";    
			elif IsOddInt(dim) then
				return "parabolic";
			else
				if IsHyperbolicForm( newform ) then
					return "hyperbolic";
				else 
					return "elliptic";
				fi;
			fi;
		else
			form := SesquilinearForm( ps );
			#mat := w!.obj * form!.matrix * TransposedMat(w!.obj); #I will use something more advanced to include hermitian forms.
			uw := Unpack(w!.obj); #here is an unpack cmat change
			mat := [uw,uw]^form; 
			if pstype = "symplectic" then
				newform := BilinearFormByMatrix( mat, gf );
				if IsDegenerateForm( newform ) then
					return "degenerate";
				else
					return "symplectic";
				fi;
			elif pstype = "hermitian" then
				#if IsHermitianMatrix( mat, gf ) then #I think this is a mistake
				newform := HermitianFormByMatrix(mat,gf);
				if IsDegenerateForm( newform ) then
					return "degenerate";
				else
					return "hermitian";
				fi;
			elif pstype in ["elliptic", "parabolic", "hyperbolic"] then
				newform := BilinearFormByMatrix( mat, gf );
				if IsDegenerateForm( newform ) then
					return "degenerate";
				else
					if IsEllipticForm(newform) then
						return "elliptic";
					elif IsHyperbolicForm(newform) then
						return "hyperbolic";
					else
						return "parabolic";
					fi;             
				fi;
			fi;
		fi;
    
		Print("Polar space does not have a recognisable type\n");
		return;
	end );

#############################################################################
# Methods to create flags.
#############################################################################

# ADDED 31/3/2014 jdb
# checks added 17/12/14 jdb
#############################################################################
#O  FlagOfIncidenceStructure( <ps>, <els> )
# returns the flag of the projective space <ps> with elements in <els>.
# the method checks whether the input really determines a flag.
# Compare the checks with the same method for projective space and a list of elements.
# I can't get CategoryCollections work for cps, but on the other hand, it is
# useful to allow els to be a list of subspaces of the ambient space of <ps>.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a projective space and list of subspaces of the projective space",
	[ IsClassicalPolarSpace, IsSubspaceOfProjectiveSpaceCollection ],
	function(ps,els)
		local list,i,test,type,flag;
		list := Set(ShallowCopy(els));
		if Length(list) > Rank(ps) then
		  Error("A flag can contain at most Rank(<ps>) elements");
		fi;
        test := List(list,x->AmbientSpace(x));
        if not ForAll(test,x->x=AmbientSpace(ps)) then
            Error("not all elements have the ambient space of <ps> as ambient space");
        fi;
		test := Set(List(list,x->x in ps));
		if (test <> [ true ] and test <> []) then
		  Error("not all elements in <els> belong to <ps>");
		fi;
		test := Set(List([1..Length(list)-1],i -> IsIncident(list[i],list[i+1])));
		if (test <> [ true ] and test <> []) then
		  Error("<els> does not determine a flag>");
		fi;
		flag := rec(geo := ps, types := List(list,x->x!.type), els := list, vectorspace := ps!.vectorspace );
		ObjectifyWithAttributes(flag, IsFlagOfCPSType, IsEmptyFlag, false, RankAttr, Size(list) );
		return flag;
	end);

# ADDED 31/3/2014 jdb
#############################################################################
#O  FlagOfIncidenceStructure( <ps>, <els> )
# returns the empty flag of the projective space <ps>.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a projective space and an empty list",
	[ IsClassicalPolarSpace, IsList and IsEmpty ],
	function(ps,els)
		local flag;
		flag := rec(geo := ps, types := [], els := [], vectorspace := ps!.vectorspace );
		ObjectifyWithAttributes(flag, IsFlagOfCPSType, IsEmptyFlag, true, RankAttr, 0 );
		return flag;
	end);


#############################################################################
# View/Print/Display methods for flags
#############################################################################

InstallMethod( ViewObj, "for a flag of a classical polar space",
	[ IsFlagOfClassicalPolarSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		Print("<a flag of ",ViewString(flag!.geo)," >");
	end );

InstallMethod( PrintObj, "for a flag of a classical polar space",
	[ IsFlagOfClassicalPolarSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		PrintObj(flag!.els);
	end );

InstallMethod( Display, "for a flag of a classical polar space",
	[ IsFlagOfClassicalPolarSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		if IsEmptyFlag(flag) then
			Print("<empty flag of ",flag!.geo,")>\n");
		else
			Print("<a flag of ",flag!.geo,"with elements of types ",flag!.types,"\n");
			Print("respectively spanned by\n");
			Display(flag!.els);
		fi;
	end );


############################################################################
## Methods for random stuff
## Since it is quick to find a pseudo-random element
## of a group (a random subproduct of the generators),
## we just find a random collineation and take the image
## of the associated element representative (see RepresentativesOfElements).
#############################################################################


# CHECKED 22/09/2011 jdb.
# CAHNGED 20/08/2014 jdb.
#############################################################################
#O  RandomSubspace( <ps>, <d> )
# returns a random subspace of projective dimension <d> in the polar space <ps>
##
InstallMethod( RandomSubspace, 
	"for a polar space and a projective dimension",
	[ IsClassicalPolarSpace, IsPosInt ],                                             
	function( ps, d )
		local x, rep, enum;
		if HasCollineationGroup(ps) then
            x := PseudoRandom( CollineationGroup(ps) );
            rep := RepresentativesOfElements(ps)[d+1];
            return OnProjSubspaces(rep, x);
        else
            enum := Enumerator(ElementsOfIncidenceStructure(ps,d));
            return Random(enum);
        fi;
  end );

# CHECKED 22/09/2011 jdb 
#############################################################################
#O  Random( <subs> )
# returns a random subspace of projective dimension <d> in the polar space <ps>
##
InstallMethod( Random, 
	"for a collection of subspaces of a polar space",
    [ IsSubspacesOfClassicalPolarSpace ],
	function( subs )
		local ps, x, rep, enum;
		ps := subs!.geometry;
		if HasCollineationGroup(ps) then
            x := PseudoRandom( CollineationGroup(ps) );
            rep := RepresentativesOfElements(ps)[subs!.type];
            return OnProjSubspaces(rep, x);
        else
            enum := Enumerator(subs);
            return Random(enum);
        fi;
	end );
  
# CHECKED 16/12/2011 jdb + ml CHECKED and CORRECTED 
#############################################################################
#O just return IteratorList(Enumerator(vs));
InstallMethod(Iterator,  
	"for subspaces of a polar space",
	[IsSubspacesOfClassicalPolarSpace],
	vs -> IteratorList(Enumerator(vs)) );


#############################################################################
#
#   Shadows of elements and flags
#
#############################################################################

# CHECKED 22/09/11 jdb
# cmat notice. in this case just the ConvertToMatrixRep causes trouble.
# added "parentflag" field in creation of collection. This happens
# also for ShadowSubspacesOfProjectiveSpace. Now generic method for \in
# for "an element of a Lie geometry and a collection of shadow elements" works.
#############################################################################
#O ShadowOfElement(<ps>, <v>, <j> ). Recall that for every particular Lie 
# geometry a method for ShadowOfElement  must be installed. 
##
InstallMethod( ShadowOfElement,
	"for a polar space, a subspace of a projective space, and an integer",
	[IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace, IsPosInt],
	function( ps, v, j )
		local localinner, localouter, localfactorspace, pstype, psdim, f, vdim, sz;
		pstype := PolarSpaceType(ps);
		psdim := ps!.dimension;
		f := ps!.basefield;
		vdim := v!.type;  
        if not AmbientSpace(ps) = AmbientSpace(v) then
            Error("<v> is not a subspace of (the ambient space) of <ps>");
        elif not v in ps then
            Error("<v> is not a subspace contained in <ps>");
        fi;
        if j > Rank(ps) then
            Error("<ps> has no elements of type <j>");
		elif j < vdim then
			localinner := [];
			localouter := Unpack(v!.obj);
			if IsVector(localouter) and not IsMatrix(localouter) then
				localouter := [localouter]; 
			fi;
			#ConvertToMatrixRep( localouter, f );
			localfactorspace := Subspace(ps!.vectorspace, localouter);
			sz := Size(Subspaces(localfactorspace, j));
		elif j = vdim then
			localinner := Unpack(v!.obj);
			if IsVector(localinner) and not IsMatrix(localinner) then
				localinner := [localinner]; 
			fi;
			localouter := localinner;
			localfactorspace := TrivialSubspace(ps!.vectorspace);
			sz := 1;
		else  
			localinner := Unpack(v!.obj);
			localouter := Unpack(PolarMap(ps)(v)!.obj); #actually, this is not a polarity when q is even and ps is parabolic.
			#localouter := UnderlyingObject(v^PolarityOfProjectiveSpace(ps));
			if pstype = "symplectic" then
				localfactorspace := SymplecticSpace( psdim- 2*vdim, f );
			elif pstype = "hermitian" then
				localfactorspace := HermitianPolarSpace( psdim-2*vdim, f );
			elif pstype = "elliptic" then 
				localfactorspace := EllipticQuadric( psdim-2*vdim, f );
			elif pstype = "parabolic" then 
				localfactorspace := ParabolicQuadric( psdim-2*vdim, f );
			elif pstype = "hyperbolic" then 
				localfactorspace := HyperbolicQuadric( psdim-2*vdim, f );
			fi;    
			sz := Size(ElementsOfIncidenceStructure(localfactorspace, j - vdim)); 
		fi;
        return Objectify(
		NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsShadowSubspacesOfClassicalPolarSpace and
                                IsShadowSubspacesOfClassicalPolarSpaceRep),
				rec( geometry := ps,
					type := j,
					inner := localinner,
					outer := localouter,
					factorspace := localfactorspace,
                    parentflag := FlagOfIncidenceStructure(ps,[v]), #added 5/4/2018, see remark above
					size := sz
					)
				);
	end );

# CHECKED 18/4/2011 jdb
# 25/3/14 It turns out that there is a problem when we do not Unpack
# cvec/cmat with Size(Subspaces(localfactorspace)). As there is never an action
# on shadow of flag objects, it is not unreasonable to store the matrices as
# GAP matrices rather than cvec/cmats.
#############################################################################
#O  ShadowOfFlag( <ps>, <flag>, <j> )
# returns the shadow elements of <flag>, i.e. the elements of <ps> of type <j>
# incident with all elements of <flag>.
# returns the shadow of a flag as a record containing the projective space (geometry),
# the type j of the elements (type), the flag (parentflag), and some extra information
# useful to compute with the shadows, e.g. iterator
##
InstallMethod( ShadowOfFlag,
    "for a classical polar space, a flag and an integer",
    [IsClassicalPolarSpace, IsFlagOfClassicalPolarSpace, IsPosInt],
    function( ps, flag, j )
    local localinner, localouter, localfactorspace, v, smallertypes, biggertypes, ceiling, floor, pstype, f, vdim, psdim, sz;
    if j > ps!.dimension then
        Error("<ps> has no elements of type <j>");
    fi;
    #empty flag - return all subspaces of the right type
    if IsEmptyFlag(flag) then
      return ElementsOfIncidenceStructure(ps, j);
    fi;

    # find the element in the flag of highest type less than j, and the subspace
    # in the flag of lowest type more than j.

    #listoftypes:=List(flag,x->x!.type);
    smallertypes:=Filtered(flag!.types,t->t <= j);
    biggertypes:=Filtered(flag!.types,t->t >= j);
    if smallertypes=[] then
        localinner := [];
        ceiling:=Minimum(biggertypes);
        localouter:=flag!.els[Position(flag!.types,ceiling)];
    elif biggertypes=[] then
        localouter:=BasisVectors(Basis(ps!.vectorspace));
        floor:=Maximum(smallertypes);
        localinner:=flag!.els[Position(flag!.types,floor)];
        vdim := localinner!.type;
    else
        floor:=Maximum(smallertypes);
        ceiling:=Minimum(biggertypes);
        localinner:=flag!.els[Position(flag!.types,floor)];
        localouter:=flag!.els[Position(flag!.types,ceiling)];
    fi;
    if not smallertypes = [] then
        if localinner!.type = 1 then
            localinner:=[Unpack(localinner!.obj)]; #here is the cmat change
        else
            localinner:=Unpack(localinner!.obj);
        fi;
    fi;
    if not biggertypes = [] then
        if localouter!.type = 1 then
            localouter := [Unpack(localouter!.obj)];
        else
            localouter := Unpack(localouter!.obj);
        fi;
    fi;
    if not biggertypes = [] then
        localfactorspace := Subspace(ps!.vectorspace,
            BaseSteinitzVectors(localouter, localinner).factorspace);
            if Dimension(localfactorspace) = 0 then
                sz := 1;
            else
                sz := Size(Subspaces(localfactorspace,j-Size(localinner)));
            fi;
    else
        pstype := PolarSpaceType(ps);
        psdim := ps!.dimension;
        f := ps!.basefield;
        if pstype = "symplectic" then
            localfactorspace := SymplecticSpace( psdim- 2*vdim, f );
        elif pstype = "hermitian" then
            localfactorspace := HermitianPolarSpace( psdim-2*vdim, f );
        elif pstype = "elliptic" then
            localfactorspace := EllipticQuadric( psdim-2*vdim, f );
        elif pstype = "parabolic" then
            localfactorspace := ParabolicQuadric( psdim-2*vdim, f );
        elif pstype = "hyperbolic" then
            localfactorspace := HyperbolicQuadric( psdim-2*vdim, f );
        fi;
        sz := Size(ElementsOfIncidenceStructure(localfactorspace, j - vdim));
    fi;
    return Objectify(
        NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsShadowSubspacesOfClassicalPolarSpace and
                                IsShadowSubspacesOfClassicalPolarSpaceRep),
        rec(
          geometry := ps,
          type := j,
          inner := localinner,
          outer := localouter,
          factorspace := localfactorspace,
          parentflag := flag,
          size := sz
        )
      );
    end);


# added 3/9/14. This was necessary since there is a generic method now
# for Iterator of shadow elements of a generic incidence structure which 
# is too generic for particular incidence geometries. The method here 
# is based on the standard GAP method that was used before we introduce the
# generic method.
#############################################################################
#O just return IteratorList(Enumerator(vs));
InstallMethod(Iterator,  
	"for shadow subspaces of a polar space",
	[ IsShadowSubspacesOfClassicalPolarSpace ],
	vs -> IteratorList(Enumerator(vs)) );


# CHECKED 22/09/11 jdb
#############################################################################
#O Size( <vs> ) returns the number of elements in a shadow collection
##
InstallMethod( Size,
	"for a collection of shadow elements of a polar space",
	[IsShadowSubspacesOfClassicalPolarSpace and	IsShadowSubspacesOfClassicalPolarSpaceRep ],
	function( vs )
		return vs!.size;
	end);

# checked 7/4/2018 jdb.
#############################################################################
#O IsCollinear( <ps>, <a>, <b> ) returns true if <a> and <b> are collinear in 
# <ps>
##
InstallMethod( IsCollinear, 
	"for a polar space and two points of its ambient space", 
	[IsClassicalPolarSpace and IsClassicalPolarSpaceRep, 
		IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	function( ps, a, b )
		if not a!.type=1 and b!.type=1 then
			Error("<a> and <b> should be points");
		elif not AmbientSpace(a) = AmbientSpace(ps) and AmbientSpace(b) = AmbientSpace(ps) then
			Error("<a> and <b> should belong to the ambientspace of <ps>");
		else
			return (a in ps) and (b in ps) and IsZero( [Unwrap(a),Unwrap(b)]^SesquilinearForm(ps) );   
		fi;
	end );

#############################################################################
# Polarities and polar spaces. 
#############################################################################

# CHECKED 22/09/11 jdb
#############################################################################
#O PolarityOfProjectiveSpace( <ps> )
#the next method returns the polarity associated to a polar space.
#recall that polarspaces and associated polarities are equivalent, except
#when q is even and the polar space is orthogonal, in this case, the associated 
#symplectic polarity is returned, so usable to compute tangent hyperplanes etc.
#When q is even and the projective dimension is even, this associated sesquilinear form
#is degenerate, so not usable to construct a polarity, because we ask a non-degenerate form
#So, the above condition is, due to definitions in Fining, and the definition of
#the "associated sesquilinear form of a polar space, equivalent with cheking
#whether the form returned by SesquilinearForm(ps) is degenerate. If not, we go!
#all necessary algebraic stuff is in the forms package.
##
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

# CHECKED 22/09/11 jdb
#############################################################################
#O PolarSpace( <polarity> ) returns the polar space associated to the polarity.
# returns an error if <polarity> is pseudo. Of course, orthogonal polar spaces
# in even characteristic cannot be reached with this method.
##
InstallMethod( PolarSpace, 
	"from a polarity of a projective space",
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

# CHECKED 22/09/11 jdb
#############################################################################
#O GeometryOfAbsolutePoints( <polarity> ) returns the geometry of absolute 
# points of <polarity>. This is in most cases a polar space, which explains why this
# method is found here.
##
InstallMethod( GeometryOfAbsolutePoints, 
	"for a polarity of a projective space",
	[ IsPolarityOfProjectiveSpace ],
	function( polarity )
		local form, geom, ps, vect, mat, n, sub;
		form := SesquilinearForm(polarity);
		if IsPseudoForm(form) then
			mat := polarity!.mat;
			n := NrRows(mat);
			vect := List([1..n],i->mat[i,i]);
			sub := NullspaceMat(TransposedMat([vect]));
			ps := ProjectiveSpace(n-1,polarity!.fld);
			return VectorSpaceToElement(ps,sub);
		else
			return PolarSpace(form);
		fi;
		return ps;
	end );

# CHECKED 22/09/11 jdb
#############################################################################
#O AbsolutePoints( <polarity> ) returns the collection of points of the 
# geometry of absolute points of <polarity>
##
InstallMethod( AbsolutePoints,
	"for a polarity of a projective space",
	[ IsPolarityOfProjectiveSpace ],
	function( polarity )
		return Points(GeometryOfAbsolutePoints(polarity));
	end );

# added 31/07/2014 jdb
#############################################################################
#O TangentSpace( <polarspace>, <el> ) returns the collection of points of the 
# geometry of absolute points of <polarity>
##
InstallMethod( AbsolutePoints,
	"for a polarity of a projective space",
	[ IsPolarityOfProjectiveSpace ],
	function( polarity )
		return Points(GeometryOfAbsolutePoints(polarity));
	end );



#I think the next method is obsolete. We have a method \in for a subspace of a projective space
# and a polar space, and this does the job. I leave as is, comment out, and if there are no complains, 
# it can be deleted.

#InstallMethod( IsTotallySingular, 
#	"for a elementen variety w.r.t a polarity",  
#              [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep, IsSubspaceOfProjectiveSpace ],
#  function( ps, v )
#    local uv, bi, f, ty, localaut, m, form, 
#          flag, bsum, count1, count2;

## Suppose the polar space is defined by a quadratic form Q. Then
## a subspace of the ambient projective space is totally singular if
## it computes zero under Q. If we have only a sesquilinear form b, 
## then let the map b(v,v) take the role of Q. 
#
#    form := SesquilinearForm(ps);
#    m := form!.matrix;
#    f := form!.basefield;
#    ty := form!.type;
#    uv := v!.obj;
#    if v!.type = 1 then uv := [uv]; fi; 
#    if ty = "hermitian" then 
#       localaut := CompanionAutomorphism(ps);
#       return IsZero( uv * m * (TransposedMat(uv)^localaut) );
#    else

## A subspace with basis B is totally singular w.r.t the quadratic form 
## Q(v) = vMv^T if for all pairs b_i and b_j in B we have Q(b_i+b_j) = 0.

       # check first that everything in uv is t.s.
       
#       flag := ForAll(uv, x -> IsZero(x * m * x));
#       if v!.type > 1 and flag then

         ## just a shorter way to go through combinations
#         count1 := 1; count2 := 2;
#         repeat
#           repeat 
#             bsum := uv[count1]+uv[count2];
#             flag := IsZero(bsum * m * bsum);
#             count2 := count2 + 1;
#           until flag or count2 > Length(uv);
#           count1 := count1 + 1;
#           count2 := count1 + 1;
#         until flag or count1 >= Length(uv);
#       fi;
#       return flag;    
#    fi;
#  end );

# this is obsolete now
#InstallMethod( IsTotallyIsotropic, "for a projective variety w.r.t a polarity", 
#             [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep, IsSubspaceOfProjectiveSpace ],
#  function( ps, v )
#    local perp, perpv;
#    perp := Polarity(ps);
#    perpv := perp(v);
#    return perpv!.type >= v!.type and v in perp(v);
#  end );

#CHECKED 20/3/14
# cmat notice: for some computations it seems necessary to Unpack the cmats.
#############################################################################
#O PolarMap( <ps> ) returns the polar map associated to the polar space <ps>.
# in most cases this polar map is just the polarity. Actually, this operation is
# only for internal use.
InstallMethod( PolarMap,  
	"for a polar space",
	[IsClassicalPolarSpace],
	function( ps )
		local perp, m, ty, f, form, bi;

## If we have only a quadratic form Q, then we must remember
## to use let the map b(v,w) := Q(v+w)-Q(v)-Q(w) be the polarisation of Q,
## and consider the polarity induced from this map. This can be computed using AssociatedBilinearForm (from package forms).

    form := SesquilinearForm(ps);
    m := form!.matrix;
    f := form!.basefield;
    ty := form!.type;

    if ty = "hermitian" then
       perp := function(v)
         local perpv, uv, rk, aut;
         aut := CompanionAutomorphism(ps);
         uv := Unpack(v!.obj); #cmat unpack.
         if v!.type = 1 then uv := [uv]; fi; 
         perpv := MutableCopyMat(TriangulizedNullspaceMat( m * TransposedMat(uv)^aut )); 
         #rk := Rank(perpv);   
         # if rk = 1 then perpv := perpv[1]; fi;
         return VectorSpaceToElement(AmbientSpace(ps), perpv); #cmat notice: now Wrap cannot be used anymore, rk not needed.
       end;
    else
       if IsEvenInt(Size(f)) and 
          (ty = "elliptic" or ty = "hyperbolic" or ty = "parabolic") then
          ## recover bilinear form from quadratic form
          bi := m + TransposedMat(m);
       else 
          bi := m;
       fi;
       
       perp := function(v)
         local perpv, uv, rk;
         uv := Unpack(v!.obj); #cmat unpack.
         if v!.type = 1 then uv := [uv]; fi; 
         perpv := MutableCopyMat(TriangulizedNullspaceMat( bi * TransposedMat(uv) )); 
         #rk := Rank(perpv);   
         #if rk = 1 then
         #   perpv := perpv[1]; 
            #ConvertToVectorRep( perpv, f );
         #else
			#ConvertToMatrixRep( perpv, f );
         #fi;
         return VectorSpaceToElement(AmbientSpace(ps), perpv); #cmat notice: now Wrap cannot be used anymore, rk not needed anymore.
       end;
    fi;
    return perp; ## should we return a correlation here? No.
  end );

#############################################################################
#
#  Groups: (special) isometry groups and similarity group of finite classical
#           polar spaces
#
#############################################################################

#############################################################################
#O CollineationGroup( <ps> ) returns collineation group of <ps>
##
InstallMethod( CollineationGroup,
	"for a polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
		local iso, twiner, g, info, x, points, hom, d, f, coll, type, act;
		d := ps!.dimension + 1; 
		f := ps!.basefield;
		if HasIsCanonicalPolarSpace(ps) and IsCanonicalPolarSpace(ps) then
			type := PolarSpaceType( ps );
			info := ClassicalGroupInfo( ps );
			if type = "symplectic" then
				g := GammaSp(d,f);
			elif type = "elliptic" then
				g := GammaOminus(d,f);  
			elif type = "hyperbolic" then
				g := GammaOplus(d,f);
			elif type = "parabolic" then
				g := GammaO(d,f);
			elif type = "hermitian" then
   			   g := GammaU(d, f);
			fi;
		else    
			iso := IsomorphismCanonicalPolarSpaceWithIntertwiner( ps );
			Info(InfoFinInG, 1, "Computing collineation group of canonical polar space...");
			coll := CollineationGroup( Source(iso)!.geometry );
			info := ClassicalGroupInfo( ps );
			twiner := Intertwiner( iso );           
			g := Image(twiner, coll);
		fi;
        ## Setting up the NiceMonomorphism
		x := RepresentativesOfElements( ps );  
		
		# We need to know in the case that d = 2 whether we have a
		# hermitian polar space. If it is hermitian, then for 2.PGammaU(d, q^2), 
		# the NiceMonomorphism will just use that for PGammaL(d, q)
		 
 	   type := PolarSpaceType( ps );
       if type ="hermitian" and d = 2 and not IsPrime(Sqrt(Size(f)))  then
          Info(InfoFinInG, 1, "No nice monomorphism computed.");
          Info(InfoFinInG, 2, "This really pisses me off, John!");
 	   else
	      if not IsEmpty(x) then
            if type = "hermitian" and d=4 then
                x := x[2];
                act := OnProjSubspacesWithFrob;
            else
                x := x[1];
                act := OnProjPointsWithFrob;
            fi;
		     if FINING.Fast then
	               hom := NiceMonomorphismByOrbit( g, x!.obj, act, info!.degree);
			 else 
		           points := Orbit(g, x, OnProjSubspaces);
		           hom := ActionHomomorphism(g, points, OnProjSubspaces, "surjective");    
		           SetIsBijective(hom, true);
		           SetNiceObject(g, Image(hom) );
	        fi;
            SetNiceMonomorphism(g, hom );
	      fi;
 	   fi;	
        
        # only for making generalised polygons section more generic:
        if IsClassicalGQ(ps) then
            SetCollineationAction(g,OnProjSubspaces);
        fi;
        return g;
	end );

#############################################################################
#O SpecialIsometryGroup( <ps> ) returns the special isometry group of <ps>
##
InstallMethod( SpecialIsometryGroup,
	"for a classical polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
	function( ps )
    local iso, twiner, g, info, coll, d, f, type;

    if HasIsCanonicalPolarSpace(ps) and IsCanonicalPolarSpace(ps) then
       type := PolarSpaceType( ps );
       info := ClassicalGroupInfo( ps );
       coll := CollineationGroup( ps );
       d := ps!.dimension + 1; 
       f := ps!.basefield;
       
       if type = "symplectic" then
          g := Spdesargues(d,f);
       elif type = "elliptic" then
          g := SOdesargues(-1,d,f);
       elif type = "hyperbolic" then
          g := SOdesargues(1,d,f);
       elif type = "parabolic" then
          g := SOdesargues(0,d,f);
       elif type = "hermitian" then
          g := SUdesargues(d, f);
       fi;
      
       SetParent(g, coll);
    else
       iso := IsomorphismCanonicalPolarSpaceWithIntertwiner( ps );
       twiner := Intertwiner( iso );
       g := Image(twiner, SpecialIsometryGroup(Source(iso)!.geometry) );
       SetParent(g, CollineationGroup(ps));
    fi;
    return g;
  end );

#############################################################################
#O IsometryGroup( <ps> ) returns the isometry group of <ps>
##
InstallMethod( IsometryGroup, 
	"for a classical polar space",
	[ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
  function( ps )
    local iso, twiner, g, info, coll, d, f, type;

    if HasIsCanonicalPolarSpace(ps) and IsCanonicalPolarSpace(ps) then
       #Print("yes!");
	   type := PolarSpaceType( ps );
       info := ClassicalGroupInfo( ps );
       coll := CollineationGroup( ps );
       d := ps!.dimension + 1; 
       f := ps!.basefield;
       
       if type = "symplectic" then
          g := Spdesargues(d,f);
       elif type = "elliptic" then
          g := GOdesargues(-1,d,f);
       elif type = "hyperbolic" then
          g := GOdesargues(1,d,f);
       elif type = "parabolic" then
          g := GOdesargues(0,d,f);
       elif type = "hermitian" then
          g := GUdesargues(d, f);
       fi;
      
       SetParent(g, coll);
    else
       
       iso := IsomorphismCanonicalPolarSpaceWithIntertwiner( ps );

       twiner := Intertwiner( iso );
       g := Image(twiner, IsometryGroup(Source(iso)!.geometry) );
       SetParent(g, CollineationGroup(ps));
    fi;
    return g;
  end );

#############################################################################
#O SimilarityGroup( <ps> ) returns the similarity group of <ps>
##
InstallMethod( SimilarityGroup, [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep ],
  function( ps )
    local iso, twiner, g, info, coll, d, f, type;
    if HasIsCanonicalPolarSpace(ps) and IsCanonicalPolarSpace(ps) then
       type := PolarSpaceType( ps );
       info := ClassicalGroupInfo( ps );
       coll := CollineationGroup( ps );
       d := ps!.dimension + 1; 
       f := ps!.basefield;
       
       if type = "symplectic" then
          g := GSpdesargues(d,f);
       elif type = "elliptic" then
          g := DeltaOminus(d,f);
       elif type = "hyperbolic" then
          g := DeltaOplus(d,f);
       elif type = "parabolic" then
          g := GOdesargues(0,d,f);
       elif type = "hermitian" then
          g := GUdesargues(d, f);
       fi;
      
       SetParent(g, coll);
    else
      iso := IsomorphismCanonicalPolarSpaceWithIntertwiner( ps );
      twiner := Intertwiner( iso );
      g := Image(twiner, SimilarityGroup(Source(iso)!.geometry) );
      SetParent(g, CollineationGroup(ps));
    fi;
    return g;
  end );

#############################################################################
		
#############################################################################
#P  IsParabolicQuadric( <ps> )
# returns true if <ps> is a parabolic quadric
##
InstallMethod( IsParabolicQuadric,
	"for a polar space having HasQuadraticForm",
	[IsClassicalPolarSpace],
	1,
	function( ps )
		if HasQuadraticForm(ps) then
			return IsParabolicForm(QuadraticForm(ps));
		else
			TryNextMethod();
		fi;
	end);
	
#############################################################################
#P  IsParabolicQuadric( <ps> )
# returns true if <ps> is a parabolic quadric
##
InstallMethod( IsParabolicQuadric, 
	"for a polar space",
	[IsClassicalPolarSpace],
	function( ps )
		return IsParabolicForm(SesquilinearForm(ps));
	end);

#############################################################################
#P  IsHyperbolicQuadric( <ps> )
# returns true if <ps> is a hyperbolic quadric
##
InstallMethod( IsHyperbolicQuadric, 
	"for a polar space having HasQuadraticForm",
	[IsClassicalPolarSpace],
	1,
	function( ps )
		if HasQuadraticForm(ps) then
			return IsHyperbolicForm(QuadraticForm(ps));
		else
			TryNextMethod();
		fi;
	end);
	
#############################################################################
#P  IsHyperbolicQuadric( <ps> )
# returns true if <ps> is a hyperbolic quadric
##
InstallMethod( IsHyperbolicQuadric, 
	"for a polar space",
	[IsClassicalPolarSpace],
	function( ps )
		return IsHyperbolicForm(SesquilinearForm(ps));
	end);


#############################################################################
#P  IsEllipticQuadric( <ps> )
# returns true if <ps> is an elliptic quadric
##
InstallMethod( IsEllipticQuadric, 
	"for a polar space having HasQuadraticForm",
	[IsClassicalPolarSpace],
	1,
	function( ps )
		if HasQuadraticForm(ps) then
			return IsEllipticForm(QuadraticForm(ps));
		else
			TryNextMethod();
		fi;
	end);
	
#############################################################################
#P  IsEllipticQuadric( <ps> )
# returns true if <ps> is an elliptic quadric
##
InstallMethod( IsEllipticQuadric, 
	"for a polar space",
	[IsClassicalPolarSpace],
	function( ps )
		return IsEllipticForm(SesquilinearForm(ps));
	end);


#############################################################################
#P  IsSymplecticSpace( <ps> )
# returns true if <ps> is an elliptic quadric
##
InstallMethod( IsSymplecticSpace,
	"for a polar space",
	[IsClassicalPolarSpace],
	function( ps )
		return PolarSpaceType(ps)="symplectic";
	end);
    
#############################################################################
#P  IsHermitianPolarSpace( <ps> )
# returns true if <ps> is an elliptic quadric
##
InstallMethod( IsHermitianPolarSpace,
	"for a polar space",
	[IsClassicalPolarSpace],
	function( ps )
		return PolarSpaceType(ps)="hermitian";
	end);

#############################################################################
# A litte extra since we put some quadrics and hermitian varieties in 
# IsProjectiveVariety.
#############################################################################


#############################################################################
#P  DefiningListOfPolynomials( <ps> )
# returns all the elements of the polar space <ps> 
## 
InstallMethod( DefiningListOfPolynomials, 
	"for a polar space",
	[IsProjectiveVariety and IsClassicalPolarSpace and IsClassicalPolarSpaceRep],
	function( ps )
		return [EquationForPolarSpace(ps)];
	end);

#############################################################################
# Elementary analytic geometry.
#############################################################################


#added 26/4/2014 jdb
#############################################################################
#A  NucleusOfParabolicQuadric( <ps> )
# returns the nuclues of a parabolic quadric (even characteristic).
## 
InstallMethod( NucleusOfParabolicQuadric, 
	"for a polar space",
	[ IsClassicalPolarSpace ],
	function( ps )
	if not IsParabolicQuadric(ps) and IsEvenInt(Size(BaseField(ps))) then
		Error(" <ps> has no nucleus" );
	else
		return VectorSpaceToElement( AmbientSpace(ps), RadicalOfFormBaseMat( SesquilinearForm(ps) ) );
	fi;
end);


#added 01/08/2014 jdb
#############################################################################
#A  TangentSpace( <el> )
# returns the tangent space at <el>, <el> must be a subspace of a polar space.
## 
InstallMethod( TangentSpace, 
	"for a subspace of a polar space",
	[ IsSubspaceOfClassicalPolarSpace ],
	function( el )
		local form, vec;
		form := SesquilinearForm(el!.geo);
		if not IsDegenerateForm(form) then
			return el^PolarityOfProjectiveSpace(form);
		else
			vec := Unpack(el!.obj)*form!.matrix;
			if el!.type = 1 then
				return HyperplaneByDualCoordinates(AmbientSpace(el),vec);
			else
				return VectorSpaceToElement(AmbientSpace(el), NullspaceMat(TransposedMat(vec)));
			fi;
		fi;
end);

#added 01/08/2014 jdb
#############################################################################
#A  TangentSpace( <el> )
# returns the tangent space at <el>, <el> must be a subspace of a polar space.
## 
InstallMethod( TangentSpace, 
	"for a subspace of a polar space",
	[ IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],
	function( ps, el )
		local form, vec;
		if not el in ps then
			Error("<el> should lie in <ps>");
		fi;
		form := SesquilinearForm(ps);
		if not IsDegenerateForm(form) then
			return el^PolarityOfProjectiveSpace(form);
		else
			vec := Unpack(el!.obj)*form!.matrix;
			if el!.type = 1 then
				return HyperplaneByDualCoordinates(AmbientSpace(el),vec);
			else
				return VectorSpaceToElement(AmbientSpace(el), NullspaceMat(TransposedMat(vec)));
			fi;
		fi;
end);

#added 01/08/2014 jdb
#############################################################################
#A  Pole(<ps>, <el> )
# returns the pole of <el> with respect to <ps>.
## 
InstallMethod( Pole, 
	"for a subspace of a polar space",
	[ IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],
	function( ps, el )
		local form, vec;
		if not el in AmbientSpace(ps) then
			Error("<el> should lie in the ambient space of <ps>");
		fi;
		form := SesquilinearForm(ps);
		if not IsDegenerateForm(form) then
			return el^PolarityOfProjectiveSpace(form);
		else
			vec := Unpack(el!.obj)*form!.matrix;
			if el!.type = 1 then
				return HyperplaneByDualCoordinates(AmbientSpace(el),vec);
			else
				return VectorSpaceToElement(AmbientSpace(el), NullspaceMat(TransposedMat(vec)));
			fi;
		fi;
end);

#############################################################################
#O  IncidenceGraph( <gp> )
# Note that computing the collineation group of a projective space is zero
# computation time. So useless to print the warning here if the group is not
# yet computed.
###
InstallMethod( IncidenceGraph,
    "for a projective space",
    [ IsClassicalPolarSpace ],
    function( ps )
        local elements, graph, adj, coll, sz;
		if IsBound(ps!.IncidenceGraphAttr) then
            return ps!.IncidenceGraphAttr;
        fi;
        if not HasCollineationGroup(ps) then
            Error("No collineation group computed. Please compute collineation group before computing incidence graph\,n");
        else
			coll := CollineationGroup(ps);
			elements := Concatenation(List([1..Rank(ps)], i -> List(AsList(ElementsOfIncidenceStructure(ps,i)))));
			adj := function(x,y)
				if x!.type <> y!.type then
					return IsIncident(x,y);
				else
					return false;
				fi;
			end;
			graph := Graph(coll,elements,OnProjSubspaces,adj,true);
			Setter( IncidenceGraphAttr )( ps, graph );
			return graph;
		fi;
	end );
