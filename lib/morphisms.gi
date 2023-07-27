#############################################################################
##
##  morphisms.gi              FinInG package
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
##  Implementation stuff for incidence geometry morphisms
##
#############################################################################


########################################
# 20/3/14
# cmat notice: in many operations we will compute
# a matrix to initialize a form. Forms can not handle cmats, so
# Unpacking the cmats will sometimes be necessary.
########################################


############################################################
## Generic constructor operations, not intended for the user.
############################################################

## The three methods for GeometryMorphismByFunction are analogous
## with the MappingByFunction function. It behaves in exactly the same
## way except that we return an IsGeometryMorphism.

# CHECKED 27/09/11 jdb
#############################################################################
#O  GeometryMorphismByFunction( <els1>, <els2>, <fun>, <bool>, <prefun> )
##
InstallMethod( GeometryMorphismByFunction, 
	"for two times a collection of elements of any type, a function, a boolean, and a function",
	[ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
	IsFunction, IsBool, IsFunction ],
	function( els1, els2, fun, bool, prefun )
		local morphism;
		morphism := MappingByFunction(els1, els2, fun, bool, prefun );
		SetFilterObj( morphism, IsGeometryMorphism ); 
		return morphism;
	end );  
  
# CHECKED 27/09/11 jdb
#############################################################################
#O  GeometryMorphismByFunction( <els1>, <els2>, <fun>, <prefun> )
##
InstallMethod( GeometryMorphismByFunction,
	"for two times a collection of elements of any type, and two times a function",
	[ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction, IsFunction ],
	function( els1, els2, fun, inv )
		local morphism;
		morphism := MappingByFunction(els1, els2, fun, inv );
		SetFilterObj( morphism, IsGeometryMorphism ); 
		return morphism;
	end );  

# CHECKED 27/09/11 jdb
#############################################################################
#O  GeometryMorphismByFunction( <els1>, <els2>, <fun> )
##
InstallMethod( GeometryMorphismByFunction,
	"for two times a collection of elements of any type, and a function",
	[ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction ],
	function( els1, els2, fun )
		local morphism;
		morphism := MappingByFunction(els1, els2, fun);
		SetFilterObj( morphism, IsGeometryMorphism ); 
		return morphism;
	end );  

#############################################################################
# Display methods
#############################################################################

InstallMethod( ViewObj, 
	"for a geometry morphism",
	[ IsGeometryMorphism ],
	function( f )
		Print("<geometry morphism from "); 
		ViewObj(Source(f));
		Print( " to " );
		ViewObj(Range(f));
		Print(">");
	end );

InstallMethod( PrintObj,
	"for a geometry morphism",
	[ IsGeometryMorphism ],
	function( f )
		Print("Geometry morphism:\n ", f, "\n");
	end );

InstallMethod( Display, 
	"for a geometry morphism",
	[ IsGeometryMorphism ],
	function( f )
		Print("Geometry morphism: ", Source(f), " -> ", Range(f), "\n");
	end );

InstallMethod( ViewObj,
	"for a geometry morphism",
	[ IsGeometryMorphism and IsMappingByFunctionWithInverseRep ],
	function( f )
		Print("<geometry morphism from "); 
		ViewObj(Source(f));
		Print( " to " );
		ViewObj(Range(f));
		Print(">");
	end );

InstallMethod( ViewObj, 
	"for a geometry morphism",
	[ IsGeometryMorphism and IsMappingByFunctionRep ],
	function( f )
		Print("<geometry morphism from "); 
		ViewObj(Source(f));
		Print( " to " );
		ViewObj(Range(f));
		Print(">");
	end );

InstallMethod( PrintObj, 
	"for a geometry morphism",
	[ IsGeometryMorphism and IsMappingByFunctionRep ],
	function( f )
		Print("Geometry morphism:\n ", f, "\n");
	end );

InstallMethod( Display,
	"for a geometry morphism",
	[ IsGeometryMorphism and IsMappingByFunctionRep ],
	function( f )
		Print("Geometry morphism: ", Source(f), " -> ", Range(f), "\n");
	end );

############################################################
## Generic methods for the Image* operations for IsGeometryMorphism. 
############################################################

# CHECKED 27/09/11 jdb
#############################################################################
#O  ImageElm( <em>, <x> )
##
InstallOtherMethod( ImageElm, 
	"for a geometry morphism and an element of an incidence structure",
	[IsGeometryMorphism, IsElementOfIncidenceStructure],
	function(em, x)
		return em!.fun(x); 
	end );

# CHECKED 27/09/11 jdb
#############################################################################
#O  \^( <x>, <em> )
##
InstallOtherMethod( \^, 
	"for an element of an incidence structure and a geometry morphism",
	[IsElementOfIncidenceStructure, IsGeometryMorphism],
	function(x, em)
		return ImageElm(em,x);
	end );


# CHECKED 27/09/11 jdb
#############################################################################
#O  ImagesSet( <em>, <x> )
##
InstallOtherMethod( ImagesSet,
	"for a geometry morphism and a collection of elements of an incidence structure",
	[IsGeometryMorphism, IsElementOfIncidenceStructureCollection],
	function(em, x)
		return List(x, t -> em!.fun(t));
	end );


# CHECKED 27/09/11 jdb
#############################################################################
#O  PreImageElm( <em>, <x> )
##
InstallOtherMethod( PreImageElm,
	"for a geometry morphism and an element of an incidence structure",
	[IsGeometryMorphism, IsElementOfIncidenceStructure],
	function(em, x)
		if IsInjective(em) then
			return em!.prefun(x); 
		else
			Error("Map is not injective");
		fi;
	end );
  
# CHECKED 27/09/11 jdb
#############################################################################
#O  PreImagesSet( <em>, <x> )
##
InstallOtherMethod( PreImagesSet,
	"for a geometry morphism and an element of an incidence structure",
	[IsGeometryMorphism, IsElementOfIncidenceStructureCollection],
	function(em, x)
		return List(x, t -> em!.prefun(t)); 
	end );

##########################################################
## User methods for the "natural geometry morphisms"
##########################################################

## The specialised operations...

# CHECKED 14/12/11 jdb + ml (and added a check that basefields of <ps1> and <ps2> are equal.)
# cmat changed 21/3/14.
#############################################################################
#O  NaturalEmbeddingBySubspace( <ps1>, <ps2>, <v> ) returns a geometry morphism
# from the projective space <ps1> into <v>, an element of the projective space
# <ps2>. <v> must be of the right type, i.e. same projective dimension as <ps1>
# and <ps1> and <ps2> must be projective spaces over the same field.
##
InstallMethod( NaturalEmbeddingBySubspace, 
	"for a projective space into another, via a specified subspace",  
	[ IsProjectiveSpace, IsProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function( geom1, geom2, v ) 
		local d1, d2, rk, f, invbasis, basis, func, pre, map, morphism, bs;
		rk := v!.type;
		basis := Unpack(v!.obj); #cmat change
		d1 := geom1!.dimension + 1;
		d2 := geom2!.dimension + 1;
		f := geom2!.basefield;
		if not v in geom2 then
			Error("Subspace is not an element of ", geom2);
		fi;
		if d2 < d1 or d1 <> rk then
			Error("Dimensions are incompatible");
		fi;
		if not f = geom1!.basefield then
			Error("Basefields must be the same");
		fi;
    ##    To find the preimage, we first find an invertible matrix C such that
    ##    [a1,..,ad]B = [a1,...,ad,0,...,0]C (where B is our d x e matrix "basis") 
    ##    is our embedding. We simply extend B to a basis for geom2.
		bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
		invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
		#ConvertToMatrixRep(invbasis, f); #useless now.
		func := x -> VectorSpaceToElement(geom2 , Unpack(x!.obj) * basis); #VectorSpaceToElement makes sure the 2nd arg becomes cmat.
		pre := function(y)
			local newy;
			if not y in v then
				Error("Applying preimage to an element which is not in the range");
			fi;
			newy:= Unpack(y!.obj) * invbasis; #cmat change.
			if not IsMatrix(newy) then 
				newy := newy{[1..d1]};
                #ConvertToVectorRepNC(newy, f); #useless now.
			else
				newy := newy{[1..Size(newy)]}{[1..d1]};
				#ConvertToMatrixRepNC(newy, f); #useless now.
			fi;
			return VectorSpaceToElement(geom1, newy); #VectorSpaceToElement etc.
		end;   
		morphism := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                           ElementsOfIncidenceStructure(geom2), 
                                           func, false, pre );
		SetIsInjective( morphism, true );  
		return morphism;
	end );

# CHECKED 27/09/11 jdb + ml
# cmat changed 21/3/14.
#############################################################################
#O  NaturalEmbeddingBySubspaceNC( <ps1>, <ps2>, <v> ) 
## This operation is just like its namesake except that it 
## has no checks
##
InstallMethod( NaturalEmbeddingBySubspaceNC, 
	"for a projective space into another, via a specified subspace",  
	[ IsProjectiveSpace, IsProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function( geom1, geom2, v ) 
		local d1, f, invbasis, basis, func, pre, map, morphism, bs;
		basis := Unpack(v!.obj); #cmat change
		d1 := geom1!.dimension + 1;
		f := geom2!.basefield;
		bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
		invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
		# ConvertToMatrixRep(invbasis, f); #useless now.
		func := x -> VectorSpaceToElement(geom2 , x!.obj * basis); #VectorSpaceToElement etc.
		pre := function(y)
			local newy;
			newy:= Unpack(y!.obj) * invbasis; #cmat change
			if not IsMatrix(newy) then 
				newy := newy{[1..d1]}; 
				#ConvertToVectorRepNC(newy, f); useless now.
			else
				newy := newy{[1..Size(newy)]}{[1..d1]};
				#ConvertToMatrixRepNC(newy, f); useless now.
			fi;
			return VectorSpaceToElement(geom1, newy); #cfr. supra.
		end;   
		morphism := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                           ElementsOfIncidenceStructure(geom2), 
                                           func, false, pre );
		SetIsInjective( morphism, true );  
		return morphism;
	end );

# CHECKED 27/09/11 jdb + ml
# cmat version 20/3/14.
#############################################################################
#O  NaturalEmbeddingBySubspace( <ps1>, <ps2>, <v> ) returns a geometry morphism
# from the polar space <ps1> into <ps2>, a polar space induced as a section of
# <ps1> and <v>, the latter a subspace of the ambient projective space of <ps1>
##
InstallMethod( NaturalEmbeddingBySubspace, 
	"for a polar space into another, via a specified subspace",  
	[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],	
	function( geom1, geom2, v ) 
	local map, d1, d2, rk, f, i, basis, invbasis, bs, c1, c2, orth, tyv, quad1, quad2,
		change, perp2, formonv, ses1, ses2, ty1, ty2, newmat, func, pre, invchange;
		rk := v!.type;
		ty1	:= PolarSpaceType(geom1); 
		ty2 := PolarSpaceType(geom2); 
		f := geom1!.basefield;	
		d1 := geom1!.dimension;
		d2 := geom2!.dimension;
		tyv := TypeOfSubspace( geom2, v );
    ## Check that fields are the same and v is non-degenerate 
		if geom2!.basefield <> f then
			Error("fields of both spaces must be the same");
		fi;
    #27/9/2011. jdb was wondering on the next 7 lines. Is this not just tyv = "degenerate"?
	#if not (ty2 = "parabolic" and IsEvenInt(Size(f))) then
    #  perp2 := Polarity(geom2);
    #  if perp2(v) in geom2 then
    #     Error("subspace is degenerate"); 
    #     return;
    #  fi;
    #fi;
		if rk > d2 or d1 > d2 then
			Error("dimensions are incompatible"); 
		fi;	
		if tyv = "degenerate" then
			Error("subspace is degenerate");
		elif
			tyv <> ty1 then 
			Error("non-degenerate section is not the same type as ", geom1);
		fi;		
		orth := ["parabolic", "elliptic", "hyperbolic"];
		if ty1 = ty2 or (ty1 in orth and ty2 in orth) then 
          
          ## Let B be basis of v. Then the form BM(B^T)^sigma (n.b. sigma 
          ## is a field automorphism), where M is the form for geom2,
          ## will be equivalent to the form for geom1.

			basis := Unpack(v!.obj); #cmat unpack

          ## As usual we must consider two cases, the quadratic form case
          ## and the sesquilinear form case. We then obtain a base change matrix c1.

			if HasQuadraticForm( geom2 ) and HasQuadraticForm( geom1 ) then
				quad1 := QuadraticForm(geom1); #cmat notice: these are never cmats. So keep untouched.
				quad2 := QuadraticForm(geom2);
				newmat := basis * quad2!.matrix * TransposedMat(basis);
				formonv := QuadraticFormByMatrix(newmat, f);
				c1 := BaseChangeToCanonical( quad1 );
			else
				ses1 := SesquilinearForm(geom1);
				ses2 := SesquilinearForm(geom2);
				if ty1 = "hermitian" then 
					newmat := basis * ses2!.matrix * (TransposedMat(basis))^CompanionAutomorphism(geom1);
					formonv := HermitianFormByMatrix(newmat, f);
				else
					newmat := basis * ses2!.matrix * TransposedMat(basis);
					formonv := BilinearFormByMatrix(newmat, f);
				fi;
				c1 := BaseChangeToCanonical( ses1 );
			fi;

       ## Finding isometry from geom1 to polar space defined by formofv:

			c2 := BaseChangeToCanonical( formonv );
			change := c1^-1 * c2;        
			# ConvertToMatrixRep(change, f); #became useless.
			invchange := change^-1;
			bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
			invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
			# ConvertToMatrixRep(invbasis, f); #same here.
			func := x -> VectorSpaceToElement( geom2, Unpack(x!.obj) * change * basis);
			pre := function(y)
				local newy;
				if not y in v then
					Error("Applying preimage to an element which is not in the range");
				fi;
				newy:= Unpack(y!.obj) * invbasis;
				if IsMatrix(newy) then 
					newy := newy{[1..Size(newy)]}{[1..rk]};
					ConvertToMatrixRepNC(newy, f);
				else
					newy := newy{[1..rk]}; 
					ConvertToVectorRepNC(newy, f);
				fi;
				return VectorSpaceToElement(geom1, newy * invchange);
			end; 
			map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                         ElementsOfIncidenceStructure(geom2), 
                                         func, false, pre );
			SetIsInjective( map, true );
		else 
			Error("Polar spaces are not compatible"); 
		fi;
		return map;
	end );
  
# CHECKED 28/09/11 jdb
# cmat comments: see NaturalEmbeddingBySubspace (above).
#############################################################################
#O  NaturalEmbeddingBySubspaceNC( <ps1>, <ps2>, <v> ) 
## This operation is just like its namesake except that it 
## has no checks
##  
InstallMethod( NaturalEmbeddingBySubspaceNC, 
	"for a geometry into another, via a specified subspace, no-check version",  
	[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],
	function( geom1, geom2, v ) 
		local map, rk, f, i, basis, invbasis, bs, c1, c2, orth, tyv, quad1, quad2,
          change, perp2, formonv, ses1, ses2, ty1, ty2, newmat, func, pre, invchange;
		rk := v!.type;
		ty1 := PolarSpaceType(geom1); 
		ty2 := PolarSpaceType(geom2); 
		f := geom1!.basefield;
		tyv := TypeOfSubspace( geom2, v );
		orth := ["parabolic", "elliptic", "hyperbolic"];
		if ty1 = ty2 or (ty1 in orth and ty2 in orth) then 
          
          ## Let B be basis of v. Then the form BM(B^T)^sigma (n.b. sigma 
          ## is a field automorphism), where M is the form for geom2,
          ## will be equivalent to the form for geom1.

		basis := Unpack(v!.obj);

          ## As usual we must consider two cases, the quadratic form case
          ## and the sesquilinear form case. We then obtain a base change matrix c1.

		if HasQuadraticForm( geom2 ) and HasQuadraticForm( geom1 ) then
			quad1 := QuadraticForm(geom1);
			quad2 := QuadraticForm(geom2);
			newmat := basis * quad2!.matrix * TransposedMat(basis);
			formonv := QuadraticFormByMatrix(newmat, f);
			c1 := BaseChangeToCanonical( quad1 );
		else
			ses1 := SesquilinearForm(geom1);
			ses2 := SesquilinearForm(geom2);
			if ty1 = "hermitian" then 
				newmat := basis * ses2!.matrix * (TransposedMat(basis))^CompanionAutomorphism(geom1);
				formonv := HermitianFormByMatrix(newmat, f);
			else
				newmat := basis * ses2!.matrix * TransposedMat(basis);
				formonv := BilinearFormByMatrix(newmat, f);
			fi;
			c1 := BaseChangeToCanonical( ses1 );
		fi;

       ## Finding isometry from geom1 to polar space defined by formofv:

		c2 := BaseChangeToCanonical( formonv );
		change := c1^-1 * c2;        
		#ConvertToMatrixRepNC(change, f);
		invchange := change^-1;
		bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
		invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
		#ConvertToMatrixRepNC(invbasis, f);

		func := x -> VectorSpaceToElement( geom2, Unpack(x!.obj) * change * basis);
		pre := function(y)
			local newy;
			newy:= Unpack(y!.obj) * invbasis;
				if IsMatrix(newy) then 
					newy := newy{[1..Size(newy)]}{[1..rk]};
					ConvertToMatrixRepNC(newy, f);
				else
					newy := newy{[1..rk]}; 
					ConvertToVectorRepNC(newy, f);
				fi;
				return VectorSpaceToElement(geom1, newy * invchange);
			end; 
			map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                         ElementsOfIncidenceStructure(geom2), 
                                         func, false, pre );
		SetIsInjective( map, true );
		else 
			Error("Polar spaces are not compatible"); 
		fi;
		return map;
	end );
  
# CHECKED 28/09/11 jdb (and added a check that basefields of <ps1> and <ps2> are equal.)
#cmat changed 20/3/14.
#############################################################################
#O  IsomorphismPolarSpaces( <ps1>, <ps2>, <bool> ) 
# returns the coordinate transformation from <ps1> to <ps2> (which must be
# polar spaces of the same type of course). If <bool> is true, then an intertwiner 
# is computed.
##
InstallMethod( IsomorphismPolarSpaces, 
    "for two similar polar spaces and a boolean",  
    [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
	function( ps1, ps2, computeintertwiner )
		local form1, form2, f, map, c1, c2, change, invchange, hom, ty1, ty2,
          coll1, coll2, gens1, gens2, x, y, func, inv, twinerfunc, twinerprefun, r1, r2;
		ty1 := PolarSpaceType( ps1 );
		ty2 := PolarSpaceType( ps2 );
		if ty1 = "parabolic" and ty2 = "symplectic" then
            return IsomorphismPolarSpacesProjectionFromNucleus(ps1, ps2, computeintertwiner);
        fi;
        r1 := Rank(ps1);
        r2 := Rank(ps2);
        if ty1 <> ty2 or r1 <> r2 then
			Error("Polar spaces are of different type or of different rank");
		fi;
		f := ps1!.basefield;
		if f <> ps2!.basefield then
			Error( "<ps1> and <ps2> must be polar spaces over the same field");
		fi;
		if IsEvenInt(Size(f)) and ty1 in ["parabolic", "elliptic", "hyperbolic"] then
			form1 := QuadraticForm( ps1 );
			form2 := QuadraticForm( ps2 );
		else
			form1 := SesquilinearForm( ps1 );
			form2 := SesquilinearForm( ps2 );
		fi;
		c1 := BaseChangeToCanonical( form1 );
		c2 := BaseChangeToCanonical( form2 );
		change := NewMatrix(IsCMatRep, ps1!.basefield, ps1!.dimension+1,c1^-1 * c2);  #cmat change here
		#ConvertToMatrixRep(change, f);
		invchange := Inverse(change); 
		if ty1 = "hermitian" then
			change := change^CompanionAutomorphism(ps2);
			invchange := invchange^CompanionAutomorphism(ps1);
		fi; 
		func := function(x)
			return VectorSpaceToElement(ps2, ShallowCopy(x!.obj) * change);
		end;
		inv := function(x)
			return VectorSpaceToElement(ps1, ShallowCopy(x!.obj) * invchange);
		end;
		map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1), 
                                      ElementsOfIncidenceStructure(ps2),
                                      func, inv);               

    # map from gens2 to gens1
		twinerprefun := function( x )
			local y;
				y := MutableCopyMat( x!.mat );
                #return ProjElWithFrob( change * y * invchange^x!.frob, x!.frob, f );  
                return ProjElWithFrob( change * y * invchange^(x!.frob^-1), x!.frob, f );  

			end;  
       
    # map from gens1 to gens2
		twinerfunc := function( y )
			local x;
				x := MutableCopyMat( y!.mat );
                #return ProjElWithFrob( invchange * x * change^y!.frob, y!.frob, f );
                return ProjElWithFrob( invchange * x * change^(y!.frob^-1), y!.frob, f ); 
			end;
		if computeintertwiner then
			if (HasIsCanonicalPolarSpace( ps1 ) and IsCanonicalPolarSpace( ps1 )) or
				HasCollineationGroup( ps1 ) then
				coll1 := CollineationGroup(ps1);
				gens1 := GeneratorsOfGroup( coll1 );  
				gens2 := List(gens1, twinerfunc);  
				coll2 := GroupWithGenerators(gens2);  
				hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun); 
				SetIntertwiner( map, hom );
				UseIsomorphismRelation(coll1,coll2);                
			elif (HasIsCanonicalPolarSpace( ps2 ) and IsCanonicalPolarSpace( ps2 )) or
				HasCollineationGroup( ps2 ) then                                 
				coll2 := CollineationGroup( ps2 );  
				gens2 := GeneratorsOfGroup( coll2 );          
				gens1 := List(gens2, twinerprefun ); 
				coll1 := GroupWithGenerators(gens1);      
				hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun); 
				SetIntertwiner( map, hom );      
				UseIsomorphismRelation(coll1,coll2);         
			else 
				Info(InfoFinInG, 1, "No intertwiner computed. One of the polar spaces must have a collineation group computed");
			fi;
		fi;
		return map;
	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  IsomorphismPolarSpaces( <ps1>, <ps2> ) 
# returns IsomorphismPolarSpaces( <ps1>, <ps2>, true );
##
InstallMethod( IsomorphismPolarSpaces, 
	"for two similar polar spaces",  
	[ IsClassicalPolarSpace, IsClassicalPolarSpace ],
	function( ps1, ps2 )
		return IsomorphismPolarSpaces( ps1, ps2, true );
	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  IsomorphismPolarSpacesNC( <ps1>, <ps2>, <bool> ) 
# This operation is just like its namesake except that it 
## has no checks
InstallMethod( IsomorphismPolarSpacesNC, 
    "for two similar polar spaces and a boolean",  
	[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
	function( ps1, ps2, computeintertwiner )
		local form1, form2, f, map, c1, c2, change, invchange, hom, ty1, ty2, n,
          coll1, coll2, gens1, gens2, func, inv, mono, twinerprefun, twinerfunc;
		ty1 := PolarSpaceType( ps1 );
		ty2 := PolarSpaceType( ps2 );
		f := ps1!.basefield;
		if IsEvenInt(Size(f)) and ty1 in ["parabolic", "elliptic", "hyperbolic"] then
			form1 := QuadraticForm( ps1 );
			form2 := QuadraticForm( ps2 );
		else
			form1 := SesquilinearForm( ps1 );
			form2 := SesquilinearForm( ps2 );
		fi;
		c1 := BaseChangeToCanonical( form1 );
		c2 := BaseChangeToCanonical( form2 );
		change := NewMatrix(IsCMatRep, ps1!.basefield, ps1!.dimension+1,c1^-1 * c2);  #cmat change here
		#ConvertToMatrixRepNC(change, f);
		invchange := Inverse(change);     
		func := function(x)
			return VectorSpaceToElement(ps2, ShallowCopy(x!.obj) * change);
		end;
		inv := function(x)
			return VectorSpaceToElement(ps1, ShallowCopy(x!.obj) * invchange);
		end;
		map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1), 
                                      ElementsOfIncidenceStructure(ps2),
                                      func, inv);               
    ## Now creating intertwiner...
	# map from gens2 to gens1
		twinerprefun := function( x )
			local y;
			y := MutableCopyMat( x!.mat );
                #return ProjElWithFrob( invchange * x * change^y!.frob, y!.frob, f );
                #return ProjElWithFrob( invchange * x * change^(y!.frob^-1), y!.frob, f );
                return ProjElWithFrob( change * y * invchange^(x!.frob^-1), x!.frob, f );
		end;  
       
    # map from gens1 to gens2
		twinerfunc := function( y )
			local x;
            x := MutableCopyMat( y!.mat );
                #return ProjElWithFrob( invchange * x * change^y!.frob, y!.frob, f );
                return ProjElWithFrob( invchange * x * change^(y!.frob^-1), y!.frob, f ); 
		end;
		if computeintertwiner then
			if (HasIsCanonicalPolarSpace( ps1 ) and IsCanonicalPolarSpace( ps1 )) or
				HasCollineationGroup( ps1 ) then
				coll1 := CollineationGroup(ps1);
				gens1 := GeneratorsOfGroup( coll1 );  
				gens2 := List(gens1, twinerfunc);  
				coll2 := GroupWithGenerators(gens2);  
				hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun); 
				SetIntertwiner( map, hom );
				UseIsomorphismRelation(coll1,coll2);                
			elif (HasIsCanonicalPolarSpace( ps2 ) and IsCanonicalPolarSpace( ps2 )) or
				HasCollineationGroup( ps2 ) then                                 
				coll2 := CollineationGroup( ps2 );  
				gens2 := GeneratorsOfGroup( coll2 );          
				gens1 := List(gens2, twinerprefun ); 
				coll1 := GroupWithGenerators(gens1);      
				hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun); 
				SetIntertwiner( map, hom );      
				UseIsomorphismRelation(coll1,coll2);         
			else 
				Info(InfoFinInG, 1, "No intertwiner computed. One of the polar spaces must have a collineation group computed");
			fi;
		fi;
		return map;
	end );
  
# CHECKED 28/09/11 jdb
#############################################################################
#O  IsomorphismPolarSpacesNC( <ps1>, <ps2> ) 
# returns IsomorphismPolarSpacesNC( <ps1>, <ps2>, true );
##
InstallMethod( IsomorphismPolarSpacesNC, 
	"for two similar polar spaces",  
	[ IsClassicalPolarSpace, IsClassicalPolarSpace ],
	function( ps1, ps2 )  
		return IsomorphismPolarSpacesNC( ps1, ps2, true );
	end );

###########################################################
## Field reduction / subfield morphisms. Helper operations.
############################################################

# CHECKED 28/09/11 jdb
#############################################################################
#O  ShrinkMat( <B>, <mat> ) 
# returns the preimage of BlownUpMat
##
InstallMethod( ShrinkMat, 
	"for a basis and a matrix",
	[ IsBasis, IsMatrix ],
	function( B, mat )
  
  ## THERE WAS A BUG IN THIS FUNCTION: (lavrauw 15/10/2010)
  ## NOT ALL MATRICES ARE BLOWNUP MATRICES! Here is a new version.
  ## Here is the explanation that could go into the documentation:
  ## The result of BlownUpMat(B,A) is the matrix, where each entry a=A_ij is replaced by
  ## the dxd matrix M_a, representing the linear map x |-> ax with respect to the basis B of
  ## the field K seen as d-dimensional vectorspace over F. This means that if 
  ## B={b_1,b_2,...,b_d}, then the first row are the coefficients of ab_1 w.r.t. B, and so on.
  ## Concersely, if we want to implement ShrinkMat, we need to check if the input is 
  ## a blown up matrix. 
  ## This can be done as follows. Let A be a dm x dn matrix. For each dxd block, say M, 
  ## we need to check that the set {b_i^(-1)\sum_{j=1}^{d}m_ij b_j: i\in \{1,..,d\}} has size one
  ## (Since this would be the a \in K represented as a linear map by M w.r.t. the basis B)
  
  ## This function is basically the inverse map of
  ## BlownUpMat.
  
	local d,vecs,m,n,bmat,blocks,bl,submat,i,j,ij,checklist,row,newmat;
	d:=Size(B);
	vecs:=BasisVectors(B);
	m:=NrRows(mat);
	n:=NrCols(mat);
  # First we check if m and n are multiples of d
	if not (IsInt(m/d) and IsInt(n/d)) then 
		Error("The matrix does not have the right dimensions");
	fi;
	blocks:=List(Cartesian([0..m/d-1],[0..n/d-1]),ij->mat{[ij[1]*d+1 .. ij[1]*d+d]}{[ij[2]*d+1 ..ij[2]*d+d]});
	newmat:=[];
	for i in [1..m/d] do
		row:=[]; 
		for j in [1..n/d] do
			#submat:=blocks[(i-1)*d+j]; #wrong line?
   			submat:=blocks[(i-1)*(m/d)+j];
			checklist:=List([1..d],i->(vecs[i]^(-1)*(Sum([1..d],j->submat[i][j]*vecs[j]))));
			if Size(AsSet(checklist))<>1 then 
				Error("The matrix is not a blown up matrix");
			fi;
			Add(row,checklist[1]);
		od;
		Add(newmat,row);
	od;
	return newmat;
  # DO WE NEED TO USE ConvertToMatrixRepNC ?
	end);	
		
		
InstallMethod( ShrinkMat, 
	"for a field, a subfield and a matrix",
	[ IsField,IsField, IsMatrix ],
	function( f1,f2,mat )
	# This gives the same result as ShrinkMat(B,mat) with B the natural basis returned by
	# Basis(AsVectorSpace(f2,f1));
	local B;
	B:=Basis(AsVectorSpace(f2,f1));
	return ShrinkMat(B,mat);
end );
	

#############################################################################
#O  ShrinkVec( <f1>, <f2>, <v> ) 
# f2 is a subfield of f1 and v is vector in a vectorspace V2 over f2
# return the vector of length d2/t, where d2=dim(V2), and t=[f1:f2]
# using the natural basis Basis(AsVectorSpace(f2,f1))
##
InstallMethod( ShrinkVec, 
	"for a field, a subfield and a vector",
	[ IsField, IsField, IsVector ],
	function( f1,f2,v )
	local t,d1,d2,basvecs;
	t:=Dimension(AsVectorSpace(f2,f1));
	d1:=Length(v)/t;
	if IsInt(d1) then
		basvecs:=BasisVectors(Basis(AsVectorSpace(f2,f1)));
		return List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
	else
		Error("The length of v is not divisible by the degree of the field extension");
	fi;
end );

#############################################################################
#O ShrinkVec( <f1>, <f2>, <v>, basis )
# The same operation as above, but now with a basis as extra argument
InstallMethod( ShrinkVec, 
	"for a field, a subfield and a vector",
	[ IsField, IsField, IsVector, IsBasis ],
	function( f1,f2,v,basis )
	local t,d1,d2,basvecs;
	t:=Dimension(AsVectorSpace(f2,f1));
	d1:=Length(v)/t;
	if IsInt(d1) then
		basvecs:=BasisVectors(basis);
		return List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
	else
		Error("The length of v is not divisible by the degree of the field extension");
	fi;
end );



#############################################################################
#O  BlownUpSubspaceOfProjectiveSpace( <B>, <pg1> ) 
#   blows up a projective space by field reduction.
##
InstallMethod( BlownUpProjectiveSpace,
	"for a basis and a projective space",
	[ IsBasis, IsProjectiveSpace ],
	function(basis,pg1)
	local q,t,r,pg2,mat1,mat2;
		q:=basis!.q;
		t:=basis!.d;
		if not pg1!.basefield=GF(q^t) then 
			Error("The basis and the projective space are not compatible!");
		fi;
		r:=Dimension(pg1)+1;
		pg2:=PG(r*t-1,q);
		return pg2;
	end );

#############################################################################
#O  BlownUpProjectiveSpaceBySubfield( <subfield>, <pg> ) 
#   blows up a projective space by field reduction w.r.t. the canonical basis
##
InstallMethod( BlownUpProjectiveSpaceBySubfield,
	"for a field and a projective space",
	[ IsField, IsProjectiveSpace ],
	function(subfield,pg)
	local t,r,field;
        field:=LeftActingDomain(UnderlyingVectorSpace(pg));
        if not subfield in Subfields(field) then
        	Error("The first argument is not a subfield of the basefield of the projective space!");
        fi;
        t:=Dimension(AsVectorSpace(subfield,field));
        r:=Dimension(pg)+1;
        return PG(r*t-1,Size(subfield));
  end );

# CHECKED 1/10/11 jdb
#############################################################################
#O  BlownUpSubspaceOfProjectiveSpace( <B>, <subspace> ) 
#  blows up a subspace of a projective space by field reduction
##
InstallMethod( BlownUpSubspaceOfProjectiveSpace,
	"for a basis and a subspace of a projective space",
	[ IsBasis, IsSubspaceOfProjectiveSpace ],
	function(basis,subspace)
	local pg1,q,t,r,pg2,mat1,mat2;
		pg1:=AmbientGeometry(subspace);
		q:=basis!.q;
		t:=basis!.d;
		if not pg1!.basefield=GF(q^t) then 
			Error("The basis and the subspace are not compatible!");
		fi;
		r:=Dimension(pg1)+1;
		pg2:=PG(r*t-1,q);
		mat1:=subspace!.obj;
		if subspace!.type = 1 then 
			mat1 := [subspace!.obj]; 
		fi; 
		mat2:=BlownUpMat(basis,mat1);
		return VectorSpaceToElement(pg2,mat2);
	end );

#############################################################################
#O  BlownUpSubspaceOfProjectiveSpaceBySubfield( <subfield>, <subspace> )
#	blows up a subspace of projective space by field reduction
# This is w.r.t. to the canonical basis of the field over the subfield.
##
InstallMethod( BlownUpSubspaceOfProjectiveSpaceBySubfield,
	"for a field and a subspace of a projective space",
	[ IsField, IsSubspaceOfProjectiveSpace],
	function(subfield,subspace)
	local pg,field,basis;
		pg:=AmbientGeometry(subspace);
		field:=LeftActingDomain(UnderlyingVectorSpace(pg));
		if not subfield in Subfields(field) then
			Error("The first argument is not a subfield of the basefield of the projective space!");
		fi;
		basis:=Basis(AsVectorSpace(subfield,field));
		return BlownUpSubspaceOfProjectiveSpace(basis,subspace);
	end );

#############################################################################
#O  IsDesarguesianSpreadElement( <B>, <subspace> ) 
# checks if a subspace is a blown up point using field reduction, w.r.t. a basis
##
InstallMethod( IsDesarguesianSpreadElement, 
	"for a basis and a subspace of a projective space",
	[ IsBasis, IsSubspaceOfProjectiveSpace ],
	function(basis,subspace)
		local flag,q,t,pg1,pg2,em,basvecs,v,v1,i,mat,rt,r;
		flag:=true;
		q:=basis!.q;
		t:=basis!.d;
		pg2:=AmbientGeometry(subspace);
		rt:=Dimension(pg2)+1;
		if not (Dimension(subspace)+1 = t and IsInt(rt/t)) then 
			flag:=false;
		else
			r:=rt/t;
			pg1:=PG(r-1,q^t);
			basvecs:=BasisVectors(basis);
			v:=Coordinates(Random(Points(subspace)));
			v1:=List([1..r],i->v{[(i-1)*t+1..i*t]}*basvecs);
			mat:=BlownUpMat(basis,[v1]);
			flag:=subspace = VectorSpaceToElement(pg2,mat);
		fi;
		return flag;
	end );
	  
# To be done: check this method! (but it is currently never used).	  
#############################################################################
#O  IsBlownUpSubspaceOfProjectiveSpace( <B>, <mat> ) 
# checks if a subspace is blown up using field reduction, w.r.t. a basis
##
InstallMethod( IsBlownUpSubspaceOfProjectiveSpace, 
	"for a basis and a subspace of a projective space",
	[ IsBasis, IsSubspaceOfProjectiveSpace ],
	# It's important that we include a basis in the arguments, 
	# since for every subspace, provided it has the right dimension, there exists some
	# basis with respect to which the subspace is blown up.
	function(basis,subspace)
		local flag,q,F,t,K,pg2,rt,r,pg1,basvecs,mat1,span,x,v,v1,i;
		flag:=true;
		q:=basis!.q;
		F:=GF(q);
		t:=basis!.d;
		K:=GF(q^t);
		pg2:=AmbientGeometry(subspace);
		rt:=Dimension(pg2)+1;
		if not (IsInt(rt/t) and IsInt((Dimension(subspace)+1)/t)) then 
			flag:=false;
		else
			r:=rt/t;
			pg1:=PG(r-1,q^t);
			basvecs:=BasisVectors(basis);
			mat1:=[];
			span:=[];
			repeat
				repeat x:=Random(Points(subspace)); until not x in span;
					v:=Coordinates(x);
					v1:=List([1..r],i->v{[(i-1)*t+1..i*t]}*basvecs);
					Add(mat1,v1);
					span:=VectorSpaceToElement(pg2,BlownUpMat(basis,mat1));
				until Dimension(span)=Dimension(subspace);
				if not span = subspace then 
					flag:= false;
				fi;
		fi;
		return flag;
 end );	  
  
#############################################################################
#		PROJECTIVE SPACES
#############################################################################

#
#  A recent reference on field reduction of projective spaces is
#  "Field reduction and linear sets in finite geomety" by Lavrauw
#   and Van de Voorde (preprint 2013)

#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <field>, <basis> ) 
# <geom2> is a projective space over a field K, <geom1> is a projective space
# over a field extension L, and considering L as a vector space over K, yields 
# that <geom1> and <geom2> have the same ambient vectorspace over K, then this
# operation returns the natural embedding, i.e. with relation to the given basis
# of L over K.
# Important note: the intertwiner has as source the *Projectivity group* only, not
# the full collineation group!
##
InstallMethod( NaturalEmbeddingByFieldReduction, 
	"for two projective spaces and a basis",
     [ IsProjectiveSpace, IsField, IsBasis ],
	function( pg1, f2, basis )
  
  ## This morphism contains a func and prefunc with built-in check.
 
		local map, f1, d1, d2, t, fun, prefun, g1, gens, newgens, g2, twiner, hom, hominv, q1, q2, pg2;
		f1 := pg1!.basefield; 
		q1 := Size(f1);
		q2 := Size(f2);
		t := LogInt(q1,q2);
		if not q2^t = q1 then
			Error( "<f2> is not a subfield of the base field of <pg1> ");
		fi;
		d1 := pg1!.dimension + 1;
		d2 := d1*t;
		pg2 := ProjectiveSpace(d2-1,q2);
		if not (IsBasis(basis) and f1=GF((basis!.q)^basis!.d) and f2=GF(basis!.q) and d1*(basis!.d)=d2) then
			Error("The basis is not a basis or is not compatible with the basefields of the geometries");
		fi;

		fun := function( subspace ) # This map blows up a subspace of geom1 to a subspace of geom2
            local mat1,mat2;
            #return BlownUpSubspaceOfProjectiveSpace(basis,x);
            mat1 := subspace!.obj;
            if subspace!.type = 1 then
                mat1 := [subspace!.obj];
            fi;
            mat2 := BlownUpMat(basis,mat1);
            return VectorSpaceToElement(pg2,mat2);
		end;

        prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
            local flag,basvecs,mat1,span,x,v,v1,i,vecs;
            flag:=true;
            if not subspace in pg2 then
                Error("The input is not in the range of the field reduction map!");
            fi;
            if not IsInt((Dimension(subspace)+1)/t) then
                flag := false;
            else
                basvecs:=BasisVectors(basis);
                vecs := Unpack(UnderlyingObject(subspace));
                mat1 := List(vecs,x->List([1..d1],i->x{[(i-1)*t+1..i*t]}*basvecs));
                span := VectorSpaceToElement(pg2,BlownUpMat(basis,mat1));
                if not span = subspace then
                    flag := false;
                fi;
            fi;
            if flag= false then
                Error("The input is not in the range of the field reduction map!");
            fi;
            return VectorSpaceToElement(pg1,mat1);
        end;

   		#prefun := function( x ) #I want to find out why this is not correct...
		#	return VectorSpaceToElement(pg1,ShrinkMat(basis,x!.obj));
		#end;
       
		map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(pg1),
                                         ElementsOfIncidenceStructure(pg2),
                                            fun, false, prefun);
		
		SetIsInjective( map, true );
        
        ## Now creating intertwiner
        ## 12/6/14: added check to see whether m is really a projectivity
		
        hom := function( m )
			local image;
            if not IsOne(m!.frob) then
                return fail;
                Info(InfoFinInG, 1, "<el> is not a projectivity");
            fi;
			image := BlownUpMat(basis, m!.mat); 
			#ConvertToMatrixRepNC( image, f2 );
			return CollineationOfProjectiveSpace(image, f2);
		end;

		hominv := function( m )
			local preimage;
            if not IsOne(m!.frob) then
                return fail;
                Info(InfoFinInG, 1, "<el> is not a projectivity");
            fi;
			preimage := ShrinkMat(basis, Unpack(m!.mat));
			#ConvertToMatrixRepNC( preimage, f1 );       
			return CollineationOfProjectiveSpace(preimage, f1);
		end;

		g1 := ProjectivityGroup( pg1 );
		gens := GeneratorsOfGroup( g1 );
		newgens := List(gens, hom);
		g2 := Group( newgens );
		SetSize(g2, Size(g1));
		twiner := GroupHomomorphismByFunction(g1, g2, hom, hominv);
		SetIntertwiner( map, twiner);
		return map;
	end );

#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <field> ) 
# <field> is a field K, <geom1> is a projective space
# over a field extension L, and considering L as a vector space over K, yields 
# that a projective space geom2 that has the same ambient vectorspace over K. This
# operation returns the natural embedding, i.e. with relation to the standard basis of
# L over K.
##
InstallMethod( NaturalEmbeddingByFieldReduction, 
	"for a projective space and a field",
	[ IsProjectiveSpace, IsField ],
	function( pg1, f2 )
		local basis;
		basis:=Basis(AsVectorSpace(f2,pg1!.basefield));
		return NaturalEmbeddingByFieldReduction(pg1,f2,basis);
	end );

#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <geom2>, <basis> ) 
# <geom2> is a projective space over a field K, <geom1> is a projective space
# over a field extension L, and considering L as a vector space over K, yields 
# that <geom1> and <geom2> have the same ambient vectorspace over K, then this
# operation returns the natural embedding, i.e. with relation to the given basis
# of L over K.
##
InstallMethod( NaturalEmbeddingByFieldReduction, 
	"for two projective spaces and a basis",
	[ IsProjectiveSpace, IsProjectiveSpace, IsBasis ],
	function( pg1, pg2, basis )
		return NaturalEmbeddingByFieldReduction(pg1,pg2!.basefield,basis);
	end);

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <geom2> ) 
# <geom2> is a projective space over a field K, <geom1> is a projective space
# over a field extension L, and considering L as a vector space over K, yields 
# that <geom1> and <geom2> have the same ambient vectorspace over K, then this
# operation returns the natural embedding, i.e. with relation to the standard basis of
# L over K.
##
InstallMethod( NaturalEmbeddingByFieldReduction, 
	"for two projective spaces",
	[ IsProjectiveSpace, IsProjectiveSpace ],
	function( pg1, pg2 )
		local basis;
		basis:=Basis(AsVectorSpace(pg2!.basefield,pg1!.basefield));
		return NaturalEmbeddingByFieldReduction(pg1,pg2!.basefield,basis);
	end );










############################################################################
#		POLAR SPACES
##############################################################################


#
#  Two references on field reduction of polar spaces:
#  [1] from group theoretic point of view: "Polar spaces and embeddings of classical groups" by Nick Gill
#  (New Zealand J. Math)
#  [2] from finite geometry point of view: "Field reduction and linear sets in finite geomety" by Lavrauw
#   and Van de Voorde (preprint 2013)
#
#  We will use [2] as it is more convenient to us.


####
#
# First the field reduction of bilinear forms, quadratic forms and hermitian forms
#
#####


#############################################################################
#O  BilinearFormFieldReduction( <bil1>, <f2>, <alpha>, <basis> ) 
# <bil1> is a bilinear form over <f1>, <f2> is a subfield of <f1>. 
# <alpha> determines the linear map from f1 to <f2>, f1 the field extension of <f2>
# with basis <basis>. This operation then
# returns the bilinear form L(bil1(.,.)), L(x) = T(alpha*x), T the trace from <f1> to <f2>.
##
# REMARK (ml 31/10/13) The fourth argument is a basis of L over K. This just gives
# extra functionality to the user, and it is used as third argument in the 
# operation ShrinkVec. 
# There is also a version of this field reduction operation, which does not use
# a basis as fourth argument. In this case the standard GAP basis is used.

InstallMethod( BilinearFormFieldReduction,
	"for a bilinear form and a field",
	[ IsBilinearForm, IsField, IsFFE, IsBasis ],
	function(bil1,f2,alpha,basis)
	# f2 is a subfield of the basefield of the bilinear form bil1
	local mat1,f1,d1,t,d2,V2,V1,phi,b2,b2vecs,mat2,i,row,j,bil2;
	mat1:=bil1!.matrix;
	f1:=bil1!.basefield;
	d1:=NrRows(mat1);
	t:=Dimension(AsVectorSpace(f2,f1));
	d2:=d1*t;
	V2:=f2^d2;
	V1:=f1^d1;
	b2:=Basis(V2);
	b2vecs:=BasisVectors(b2);
	mat2:=[];
	for i in [1..d2] do 
		row:=[];
		for j in [1..d2] do
			Add(row,Trace(f1,f2,alpha*(ShrinkVec(f1,f2,b2vecs[i],basis)*mat1*ShrinkVec(f1,f2,b2vecs[j],basis))));
		od;
		Add(mat2,row);
	od;
	bil2:=BilinearFormByMatrix(mat2,f2);
	return bil2;
end );

#############################################################################
#O  BilinearFormFieldReduction( <bil1>, <f2>, <alpha> ) 
# The same as above, but without specified basis. In this case the canonical basis is used.
##
InstallMethod( BilinearFormFieldReduction,
	"for a bilinear form and a field",
	[ IsBilinearForm, IsField, IsFFE ],
	function(bil1,f2,alpha)
	# f2 is a subfield of the basefield of the bilinear form bil1
	return BilinearFormFieldReduction(bil1,f2,alpha,Basis(AsVectorSpace(f2,bil1!.basefield)));
end );

#############################################################################
#O  QuadraticFormFieldReduction( <qf1>, <f2>, <alpha>, <basis> ) 
# <bil1> is a bilinear form over <f1>, <f2> is a subfield of <f1>. This operation
# returns the bilinear form T(alpha*qf1(.,.)), T the trace from <f1> to <f2>.
##
# REMARK (ml 31/10/13) The fourth argument is a basis of L over K. This just gives
# extra functionality to the user, and it is used as third argument in the 
# operation ShrinkVec. 
# There is also a version of this field reduction operation, which does not use
# a basis as fourth argument. In this case the standard GAP basis is used.

InstallMethod( QuadraticFormFieldReduction,
	"for a quadratic form and a field",
	[ IsQuadraticForm, IsField, IsFFE, IsBasis ],
	function(qf1,f2,alpha,basis)
	# f2 is a subfield of the basefield for the quadratic form q1
	local f1,basvecs,d1,t,d2,V2,V1,phi,b2,b2vecs,mat2,i,bil1,j,qf2;
	f1:=qf1!.basefield;
	basvecs:=BasisVectors(basis);
	d1:=NrRows(qf1!.matrix);
	t:=Dimension(AsVectorSpace(f2,f1));
	d2:=d1*t;
	V2:=f2^d2;
	V1:=f1^d1;
	b2:=Basis(V2);
	b2vecs:=BasisVectors(b2);
	mat2:=IdentityMat(d2,f2);
	for i in [1..d2] do
		mat2[i,i]:=Trace(f1,f2,alpha*((ShrinkVec(f1,f2,b2vecs[i],basis))^qf1));
	od;
	for i in [1..d2-1] do
		for j in [i+1..d2] do
			mat2[i,j]:=Trace(f1,f2,alpha*((ShrinkVec(f1,f2,b2vecs[i]+b2vecs[j],basis))^qf1
							-(ShrinkVec(f1,f2,b2vecs[i],basis))^qf1-(ShrinkVec(f1,f2,b2vecs[j],basis))^qf1));
			#mat2[j,i]:=mat2[i,j]; THESE entries need to be zero
		od;
	od;
	qf2:=QuadraticFormByMatrix(mat2,f2);
	return qf2;
end );

#############################################################################
# #O  QuadraticFormFieldReduction( <qf1>, <f2>, <alpha> )
# The same as above, but without specified basis. In this case the canonical basis is used.
##
InstallMethod( QuadraticFormFieldReduction,
	"for a quadratic form and a field",
	[ IsQuadraticForm, IsField, IsFFE ],
	function(qf1,f2,alpha)
	local basis,qf2;
	# f2 is a subfield of the basefield for the quadratic form q1
	basis:=Basis(AsVectorSpace(f2,qf1!.basefield));
	qf2:=QuadraticFormFieldReduction(qf1,f2,alpha,basis);
	return qf2;
end );


#############################################################################
#O  HermitianFormFieldReduction( <hf1>, <f2>, <alpha>, <basis> ) 
# <hf1> is a hermitian form over <f1>, <f2> is a subfield of <f1>. This operation
# returns the form T(alpha*bil1(.,.)). Depending on the degree of the field extension, this
# yields either a bilinear form or a hermitian form.
##
# REMARK (ml 31/10/13) The fourth argument is a basis of L over K. This just gives
# extra functionality to the user, and it is used as third argument in the 
# operation ShrinkVec. 
# There is also a version of this field reduction operation, which does not use
# a basis as fourth argument. In this case the standard GAP basis is used.

InstallMethod( HermitianFormFieldReduction,
	"for a hermitian form and a field",
	[ IsHermitianForm, IsField, IsFFE, IsBasis ],
	function(hf1,f2,alpha,basis)
	# f2 is a subfield of the basefield for the hermitian form hf1
	# here the basefield is always a square
	local f1,d1,t,d2,V2,V1,phi,b2,b2vecs,mat2,i,row,j,hf2;
	f1:=hf1!.basefield;
	d1:=NrRows(hf1!.matrix);
	t:=Dimension(AsVectorSpace(f2,f1));
	d2:=d1*t;
	V2:=f2^d2;
	V1:=f1^d1;
	b2:=Basis(V2);
	b2vecs:=BasisVectors(b2);
	mat2:=[];
	for i in [1..d2] do 
		row:=[];
		for j in [1..d2] do
			Add(row,Trace(f1,f2,alpha*([ShrinkVec(f1,f2,b2vecs[i],basis),ShrinkVec(f1,f2,b2vecs[j],basis)]^hf1)));
		od;
		Add(mat2,row);
	od;
	# checking parity of t is indeed sufficient to decide, see table in manual.
	if IsOddInt(t) then 
		hf2:=HermitianFormByMatrix(mat2,f2);
	else
		hf2:=BilinearFormByMatrix(mat2,f2);
	fi;
	return hf2;
end );


#############################################################################
# #O  HermitianFormFieldReduction( <hf11>, <f2>, <alpha> ) 
# The same as above, but without specified basis. In this case the canonical basis is used.
##

InstallMethod( HermitianFormFieldReduction,
	"for a hermitian form and a field",
	[ IsHermitianForm, IsField, IsFFE ],
	function(hf1,f2,alpha)
	# f2 is a subfield of the basefield for the hermitian form hf1
	# here the basefield is always a square
	local basis;
	
	basis:=Basis(AsVectorSpace(f2,hf1!.basefield));
	return HermitianFormFieldReduction(hf1,f2,alpha,basis);
end );



##########################################################
#
# Next the embeddings for polar spaces based on the field reduction of the forms above
# 
#############################################################################

# master version for the user: handle all parameters.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <ps1>, <f2>, <alpha>, <basis>, <bool> ) 
# returns a geometry morphism, described below. If <bool> is true, then an intertwiner
# is computed.
##
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space, a field, a basis, a finite field element, and a boolean",
	[IsClassicalPolarSpace, IsField, IsFFE, IsBasis, IsBool],
	function(ps1,f2,alpha,basis,computeintertwiner)
	# f2 must be a subfield of the basefield of the classical polar space
	local type1,qf1,qf2,ps2,bil1,bil2,hf1,hf2,fun,prefun,f1,
					map,hom,hominv,g1,gens,newgens,g2,twiner, t,q1,q2,d1,d2;
	type1 := PolarSpaceType(ps1);
	f1:=ps1!.basefield;
	q1 := Size(f1);
	q2 := Size(f2);
	t := LogInt(q1,q2);
	d1 := ps1!.dimension + 1;
	d2 := d1*t;

	# 1. the polar space is of orthogonal type
	if type1 in ["hyperbolic","elliptic","parabolic"] then
		qf1:=QuadraticForm(ps1);
		qf2:=QuadraticFormFieldReduction(qf1,f2,alpha,basis);
		
		if IsSingularForm(qf2) then 
			Error("The field reduction does not yield a natural embedding");
		else ps2:=PolarSpace(qf2);
		fi;
	# 2. the polar space is of symplectic type
	elif type1 in ["symplectic"] then
		bil1:=SesquilinearForm(ps1);
		bil2:=BilinearFormFieldReduction(bil1,f2,alpha,basis);
		ps2:=PolarSpace(bil2);
	# 3. the polar space is of hermitian type	
	elif type1 in ["hermitian"] then
		hf1:=SesquilinearForm(ps1);
		hf2:=HermitianFormFieldReduction(hf1,f2,alpha,basis);
		ps2:=PolarSpace(hf2);
	fi;
	
	#em:=NaturalEmbeddingByFieldReduction(AmbientSpace(ps1),AmbientSpace(ps2));
	# this is the field reduction for projective spaces PG(r-1,q^t) -> PG(rt-1,q)
	
	#fun:=function(x)
	#	local projfun;
	#	projfun:=em!.fun;
	#	return VectorSpaceToElement(ps2,projfun(x)!.obj);
	#end;
	
    fun := function( subspace )
        local mat1,mat2;
        mat1 := subspace!.obj;
        if subspace!.type = 1 then
            mat1 := [subspace!.obj];
        fi;
        mat2 := BlownUpMat(basis,mat1);
        return VectorSpaceToElement(ps2,mat2);
    end;


	#prefun:=function(x)
	#	local projprefun;
	#	projprefun:=em!.prefun;
	#	return VectorSpaceToElement(ps1,projprefun(x)!.obj);
	#end;

    # new version (2/7/14, much faster since we avoid the repeat loops and Random calls.
    prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
        local flag,basvecs,mat1,span,x,v,v1,i,vecs;
        flag:=true;
        if not subspace in ps2 then
            Error("The input is not in the range of the field reduction map!");
        fi;
        if not IsInt((Dimension(subspace)+1)/t) then
            flag:=false;
        else
            basvecs:=BasisVectors(basis);
            mat1:=[];
            span:=[];
            vecs := Unpack(UnderlyingObject(subspace));
            mat1 := List(vecs,x->List([1..d1],i->x{[(i-1)*t+1..i*t]}*basvecs));
            span := VectorSpaceToElement(AmbientSpace(ps2),BlownUpMat(basis,mat1)); 
                #the ambientspace makes sure that there is no error message here yet.
            if not span=subspace then
                flag := false;
            fi;
        fi;
        if flag= false then
            Error("The input is not in the range of the field reduction map!");
        fi;
        return VectorSpaceToElement(ps1,mat1);
    end;

	map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1), ElementsOfIncidenceStructure(ps2),
                                           fun, false, prefun);
		
	SetIsInjective( map, true );
	
	## Now creating intertwiner
	
	if computeintertwiner then
        if (HasIsCanonicalPolarSpace( ps1 ) and IsCanonicalPolarSpace( ps1 )) or
            HasCollineationGroup( ps1 ) then
            
            hom := function( m )
                local image;
                image := BlownUpMat(basis, m!.mat);
                ConvertToMatrixRepNC( image, f2 );
                return CollineationOfProjectiveSpace(image, f2);
            end;
            hominv := function( m )
                local preimage;
                preimage := ShrinkMat(basis, Unpack(m!.mat));
                ConvertToMatrixRepNC( preimage, f1 );
                return CollineationOfProjectiveSpace(preimage, f1);
            end;
            g1 := IsometryGroup( ps1 );
            gens := GeneratorsOfGroup( g1 );
            newgens := List(gens, hom);
            g2 := Group( newgens );
            SetSize(g2, Size(g1));
            twiner := GroupHomomorphismByFunction(g1, g2, hom, hominv);
            SetIntertwiner( map, twiner);
        fi;
	fi;

    return map;
	end );


#first version: user wants a particular alpha, and basis, agrees with bool=true.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis> )
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis>, true )
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space and a field, a finite field element, and a basis",
	[IsClassicalPolarSpace, IsField, IsFFE, IsBasis],
	function(ps1,f2,alpha,basis)
		return NaturalEmbeddingByFieldReduction(ps1,f2,alpha,basis,true);
	end );

#second particular version: user wants a particular alpha, and bool, agrees with basis.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <bool> )
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis>, true )
# where <basis> is the canonical basis of BaseField(geom1) over <f2>.
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space and a field, a finite field element, and a boolean",
	[IsClassicalPolarSpace, IsField, IsFFE, IsBool],
	function(ps1,f2,alpha,bool)
		return NaturalEmbeddingByFieldReduction(ps1,f2,alpha,Basis(AsVectorSpace(f2,BaseField(ps1))),bool);
	end );

#third particular version: user wants a particular alpha, agrees with basis and bool.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha> )
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis>, true )
# where <basis> is the canonical basis of BaseField(geom1) over <f2>.
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space, a field, and a finite field element",
	[IsClassicalPolarSpace, IsField, IsFFE],
	function(ps1,f2,alpha)
		return NaturalEmbeddingByFieldReduction(ps1,f2,alpha,Basis(AsVectorSpace(f2,BaseField(ps1))),true);
	end );

# fourth particular version: user agrees but wants to control intertwiner
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <bool> ) 
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <one>, <basis>, <bool> )
# where <basis> is the canonical basis of BaseField(geom1) over <f2>, and
# <one> is One(BaseField(geom1)). This might be usefull if you agree with the defaults, but doesn't 
# want the intertwiner.
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space, a field, and a boolean",
	[IsClassicalPolarSpace, IsField, IsBool],
	function(ps1,f2,bool)
		return NaturalEmbeddingByFieldReduction(ps1,f2,One(f2),Basis(AsVectorSpace(f2,BaseField(ps1))),bool);
	end );

# fifth particular version: user agrees with everything
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2> ) 
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis>, true )
# where <basis> is the canonical basis of BaseField(geom1) over <f2>, and
# <alpha> is One(f2).
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space and a field",
	[IsClassicalPolarSpace, IsField],
	function(ps1,f2)
		return NaturalEmbeddingByFieldReduction(ps1,f2,One(f2),Basis(AsVectorSpace(f2,BaseField(ps1))),true);
	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <geom2> ) 
# returns NaturalEmbeddingByFieldReduction(<geom1>, <geom2> )
##
InstallMethod( NaturalEmbeddingByFieldReduction, 
	"for two polar spaces",
	[ IsClassicalPolarSpace, IsClassicalPolarSpace ],
	function( geom1, geom2 )
		return NaturalEmbeddingByFieldReduction( geom1, geom2, true );
	end );

# ml 31/10/2013
# changed with new prefun 2/7/14 jdb
#####################################################################################
# O  NaturalEmbeddingByFieldReduction
# This NaturalEmbeddingByFieldReduction takes two polar spaces and returns an embedding
# obtained by field reduction if such an embedding exists, otherwise it returns an error.
##
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for two classical polar spaces",
	[IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool],
	function(ps1, ps2, computeintertwiner)

	#####################################################################
	#  List of possible embeddings of ps1 in PG(r-1, q^t) -> ps2 in PG(rt-1, q)
	#
	#  Hermitian
	#  	H -> H (t odd)
	#  	H -> W (t even)
	#  	H -> Q+ (t even, r even)
	#  	H -> Q- (t even, r odd)
	#
	#  Symplectic
	#  	W -> W (always)
	#
	#  Orthogonal	
	# r even			
	#	Q+ -> Q+ (always)
	#	Q- -> Q- (always)
	# r odd
	#	Q -> Q (t odd, q odd)
	#   Q -> Q+ (t even, q odd)
	#   Q -> Q- (t even, q odd)
	#####################################################################

    local t, r, type1, type2, form1, f1, f2, newps, sigma, map,
		  gamma, bil1, q, n, alpha, basis, is_square, iso, form2, fun, prefun,
          c1, c2, cps2, cps2inv, d1, hom, hominv, g1, g2, gens, newgens, twiner;
					
	type1 := PolarSpaceType(ps1);
	type2 := PolarSpaceType(ps2);
	if type1 in ["hyperbolic", "parabolic", "elliptic"] then 
	   form1 := QuadraticForm(ps1);
	else
	   form1 := SesquilinearForm(ps1);
	fi;
    d1 := ps1!.dimension+1;
	f1 := ps1!.basefield;
	f2 := ps2!.basefield;
	q := Size(f2);
	
	if not IsSubset(f1,f2) then
	   Error("Fields are incompatible");
	fi;
	t := Log(Size(f1),q);
	r := ProjectiveDimension(ps1)+1;
	basis := CanonicalBasis(AsVectorSpace(f2,f1));
    is_square := function(x, f) return IsZero(x) or IsEvenInt(LogFFE(x,PrimitiveRoot(f))); end;
      
	if IsSesquilinearForm(form1) then
       	if type1 = "hermitian" and type2 = "hermitian" and IsOddInt(t) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
				# need alpha to be fixed by sigma:
			alpha := One(f1);
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
			form2 := HermitianFormFieldReduction(form1,f2,alpha,basis);
	   	elif type1 = "hermitian" and type2 = "symplectic" and IsEvenInt(t) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			if IsEvenInt(q) then
				# need alpha to be fixed by sigma:
				alpha := One(f1);				
			else
				# need sigma(alpha)=-alpha
				sigma := Sqrt(Size(f1));
				alpha := First(f1, x->not IsZero(x) and x^sigma = -x);				
			fi;	
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := HermitianFormFieldReduction(form1,f2,alpha,basis);
		
	   	elif type1 = "hermitian" and type2 = "hyperbolic" and IsEvenInt(t) and IsEvenInt(r) 
				and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);	
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := HermitianFormFieldReduction(form1,f2,alpha,basis);
						
	   	elif type1 = "hermitian" and type2 = "elliptic" and IsEvenInt(t) and IsOddInt(r) 
				and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);	
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := HermitianFormFieldReduction(form1,f2,alpha,basis);
			
	   	elif type1 = "symplectic" and type2 = "symplectic" then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);				
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := BilinearFormFieldReduction(form1,f2,alpha,basis);
        else
		 	Error("These polar spaces are not suitable for field reduction.");
		fi;
	else 	# form1 is a quadratic form
		if type1 = "hyperbolic" and type2 = "hyperbolic" then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := QuadraticFormFieldReduction(form1,f2,alpha,basis);
		elif type1 = "elliptic" and type2 = "elliptic" then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := QuadraticFormFieldReduction(form1,f2,alpha,basis);
		elif type1 = "parabolic" and type2 = "parabolic" and IsOddInt(t) and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := QuadraticFormFieldReduction(form1,f2,alpha,basis);

		# If ps1 is parabolic and ps2 is hyperbolic or elliptic, 
		# then the choice of alpha that we will use for the embedding
		# depends on the isometry type of ps1. The isometry type is determined by
		# gamma being a square or not, where gamma is
		# (-1)^(r-1)/2 times the determinant of the bilinear form, divided by two.
		# The conditions we use here are taken from [Lavrauw - Van de Voorde: "Field
		# reduction and linear sets in finite geometry", preprint], they are
		# somewhat easier to work with than the conditions in Gill's paper.

		elif type1 = "parabolic" and type2 = "hyperbolic" and IsEvenInt(t) and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction: Parabolic -> Hyperbolic");
			bil1:=AssociatedBilinearForm(form1); # important to use this instead of 
			# BilinearFormByQuadraticForm (see documentation of the Forms package).
			gamma:=((-1)^((r-1)/2)) * Determinant(bil1!.matrix)/2;
			if q^(t/2) mod 4 = 1 then # the product of alpha and gamma must be a nonsquare
 				alpha := First(f1, a -> not IsZero(a) and not is_square( a*gamma, f1 ) );
			else # the product of alpha and gamma must be a square
				alpha := First(f1, a -> not IsZero(a) and is_square( a*gamma, f1 ) );
			fi;
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := QuadraticFormFieldReduction(form1,f2,alpha,basis);

		elif type1 = "parabolic" and type2 = "elliptic" and IsEvenInt(t) and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction: Parabolic -> Elliptic");
			bil1:=AssociatedBilinearForm(form1); # important to use this instead of 
			# BilinearFormByQuadraticForm (see documentation of the Forms package).
			gamma:=((-1)^((r-1)/2)) * Determinant(bil1!.matrix)/2;
			if q^(t/2) mod 4 = 1 then # the product of alpha and gamma must be a square
 				alpha := First(f1, a -> not IsZero(a) and is_square( a*gamma, f1 ) );
			else # the product of alpha and gamma must be a nonsquare
				alpha := First(f1, a -> not IsZero(a) and not is_square( a*gamma, f1 ) );
			fi;
			#map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis );
            form2 := QuadraticFormFieldReduction(form1,f2,alpha,basis);

		else
		 	Error("These polar spaces are not suitable for field reduction.");
     	fi;	
	fi;
	
	c1 := BaseChangeToCanonical( form2 );
    if not IsCanonicalPolarSpace(ps2) then
        if type2 in ["parabolic", "hyperbolic", "elliptic"] then
            c2 := BaseChangeToCanonical(QuadraticForm(ps2));
        else
            c2 := BaseChangeToCanonical(SesquilinearForm(ps2));
        fi;
        cps2 := c2^-1 * c1;
    else
        cps2 := c1;
    fi;
    cps2inv := cps2^-1;

    fun := function( subspace )
        local mat1,mat2;
        mat1 := subspace!.obj;
        if subspace!.type = 1 then
            mat1 := [subspace!.obj];
        fi;
        mat2 := BlownUpMat(basis,mat1);
        return VectorSpaceToElement(ps2,mat2*cps2inv);
    end;

    #this version of prefun is new (2/7/14).

    prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
        local flag,basvecs,mat1,span,x,v,v1,i,vecs;
        flag:=true;
        if not subspace in ps2 then
            Error("The input is not in the range of the field reduction map!");
        fi;
        if not IsInt((Dimension(subspace)+1)/t) then
            flag:=false;
        else
            basvecs:=BasisVectors(basis);
            vecs := Unpack(UnderlyingObject(subspace))*cps2;
            mat1 := List(vecs,x->List([1..d1],i->x{[(i-1)*t+1..i*t]}*basvecs));
            span := VectorSpaceToElement(AmbientSpace(ps2),BlownUpMat(basis,mat1)*cps2inv); #the ambientspace makes sure that there is no error message here yet.
            if not span = subspace then
                flag := false;
            fi;
        fi;
        if flag = false then
            Error("The input is not in the range of the field reduction map!");
        fi;
        return VectorSpaceToElement(ps1,mat1);
    end;

    #iso := IsomorphismPolarSpacesNC(AmbientGeometry(Range(map)), ps2);
	#morphism := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1),
    #                                   ElementsOfIncidenceStructure(ps2),
    #                                    x -> iso!.fun(map!.fun(x)), false,  x -> map!.prefun(iso!.prefun(x)) );

	map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1),
                                         ElementsOfIncidenceStructure(ps2),
                                            fun, false, prefun);
	SetIsInjective( map, true );

    if (HasIsCanonicalPolarSpace( ps1 ) and IsCanonicalPolarSpace( ps1 )) or
            HasCollineationGroup( ps1 ) then
        if computeintertwiner then
            hom := function( m )
                local image;
                image := BlownUpMat(basis, m!.mat);
                ConvertToMatrixRepNC( image, f2 );
                return CollineationOfProjectiveSpace(cps2 * image * cps2inv, f2);
            end;
            hominv := function( m )
                local preimage;
                preimage := ShrinkMat(basis, cps2inv * Unpack(m!.mat) * cps2);
                ConvertToMatrixRepNC( preimage, f1 );
                return CollineationOfProjectiveSpace(preimage, f1);
            end;
            g1 := IsometryGroup( ps1 );
            gens := GeneratorsOfGroup( g1 );
            newgens := List(gens, hom);
            g2 := Group( newgens );
            SetSize(g2, Size(g1));
            twiner := GroupHomomorphismByFunction(g1, g2, hom, hominv);
            SetIntertwiner( map, twiner);
        fi;
    fi;

	SetIntertwiner(map, hom );
		
	return map;
end );

# added 2/7/14 jdb
#####################################################################################
# O  NaturalEmbeddingByFieldReduction
# This NaturalEmbeddingByFieldReduction takes two polar spaces and returns an embedding
# obtained by field reduction if such an embedding exists, otherwise it returns an error.
##
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for two classical polar spaces",
	[IsClassicalPolarSpace, IsClassicalPolarSpace],
	function(ps1, ps2)
        return NaturalEmbeddingByFieldReduction(ps1,ps2,true);
    end );


#############################################################################
#
#
# Embeddings by SUBFIELDS
#
#
#############################################################################


# CHECKED 11/06/14 jdb
# found another cvec/cmat bug thanks to our testuser Geertrui!
#############################################################################
#O  NaturalEmbeddingBySubfield( <geom1>, <geom2> )
# returns the embedding of <geom1> in <geom2>, two projective spaces of the
# same dimension, over the fields K and L respectively, where L is an extension
# of K.
##
InstallMethod( NaturalEmbeddingBySubfield, 
	"for two projective spaces",
	[ IsProjectiveSpace, IsProjectiveSpace ],
	function( geom1, geom2 )
		local map, f1, f2, func, prefun, g1, g2,
          gens1, gens2, hom, twinerfunc, twinerprefun;
		f1 := geom1!.basefield; 
		f2 := geom2!.basefield; 
		if geom1!.dimension  <> geom2!.dimension or not IsSubset(f2,f1) then  
			Error("Dimensions and/or field sizes are incompatible"); return;
		fi;
		
        func :=  x -> VectorSpaceToElement(geom2, Unpack(x!.obj)); #here an unpack is needed to avoid field incompatibilites.
		
        prefun := function( x )
            local xmat;
			if not ForAll( Flat(x!.obj), i -> i in f1 ) then
				Error("Element does not have all of its coordinates in ", f1);
			fi;
            xmat := Unpack(x!.obj);
            if x!.type = 1 then
                ConvertToVectorRep(xmat);
            else
                ConvertToMatrixRep(xmat);
            fi;
			return VectorSpaceToElement(geom1, xmat ); # the same here.
		end;
		
        map := GeometryMorphismByFunction( ElementsOfIncidenceStructure(geom1),
                                       ElementsOfIncidenceStructure(geom2),
                                       func, false, prefun ); 
		SetIsInjective( map, true );

    ## Intertwiner...
    
		twinerfunc := function( x )
            local xmat;
            if not IsOne(x!.frob) then
				Info(InfoFinInG, 1, "<el> is not in the source of the intertwiner");
                return fail;
            fi;
            xmat := x!.mat;
            return CollineationOfProjectiveSpace( Unpack(x!.mat), f2 ); #here was an unpack needed!
        end;

		twinerprefun := function( y )
            local ymat,q;
            if not IsOne(y!.frob) then
				Info(InfoFinInG, 1, "<el> is not in the range of the intertwiner");
                return fail;
            fi;
            ymat := Unpack(y!.mat); #here too!
            q := ConvertToMatrixRep(ymat);
            if not q = Size(f1) then
   				Info(InfoFinInG, 1, "<el> is not in the range of the intertwiner");
                return fail;
            fi;
            return CollineationOfProjectiveSpace(ymat,f1);
        end;
        
        g1 := ProjectivityGroup( geom1 );
		gens1 := GeneratorsOfGroup( g1 );
		gens2 := List(gens1, twinerfunc );
		g2 := Group(gens2);
		SetSize(g2, Size(g1));
		hom := GroupHomomorphismByFunction(g1, g2, twinerfunc, twinerprefun);    
		SetIntertwiner(map, hom);
		return map;
	end );
  
# CHECKED 25/7/14 jdb. With new (and correct now) prefun. 
# cmat changes 21/3/14 and 24/3/2014, after a nice cultural weekend :-)
#############################################################################
#O  NaturalEmbeddingBySubfield( <geom1>, <geom2>, <bool> )
# returns the embedding of <geom1> in <geom2>, two polar spaces of the
# same dimension, over the fields K and L respectively, where L is an extension
# of K. An Intertwiner is computed if <bool> is true.
# The prefun principle is taken from NaturalDualityHermitian.
##
InstallMethod( NaturalEmbeddingBySubfield, 
	"for two polar spaces and a boolean",
	[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
	function( geom1, geom2, computeintertwiner )
		local map, f1, f2, q1, q2, gamma, func, prefun, g1, g2, gens1, gens2, f,
          r, type1, type2, iso, gram, newgram, form, ps, hom, twinerfunc, twinerprefun,
		  change, invchange, basis, d, n, bmat, one, i, c1, c2, form2;
        
    #
    #  Check Kleidman and Liebeck Table 4.5.A
    #  There, the conditions on the polarity types can be found.
    #
    #  All possible cases:
    #  W -> W
    #  W -> H (quadratic)
    #  O^eps -> H (q odd, quadratic)
    #  H -> H (odd degree)
    #  O^eps -> O^eps' (eps = eps'^r) 
    #

		f1 := geom1!.basefield; q1 := Size(f1); 
		f2 := geom2!.basefield; q2 := Size(f2);       
		type1 := PolarSpaceType(geom1);
		type2 := PolarSpaceType(geom2);
		form2 := SesquilinearForm( geom2 ); #will be changed in last case only
		if geom1!.dimension = geom2!.dimension and IsSubset(f2, f1) then  
			r := LogInt(q2, q1);
			gram := GramMatrix( SesquilinearForm(geom1) );
			if [type1, type2] = ["symplectic", "hermitian" ] and r = 2 then                   
				gamma := First( f2, t -> not IsZero(t) and IsZero(t^q1+t)); 
				newgram := gamma * gram;
				form := HermitianFormByMatrix(newgram, f2);
			elif [type1, type2] = ["symplectic", "symplectic"] then
				form := BilinearFormByMatrix(gram, f2);
			elif [type1, type2] = ["hermitian", "hermitian"] and IsOddInt(r) then
				form := HermitianFormByMatrix(gram, f2);
			elif type1 in ["elliptic", "parabolic", "hyperbolic"] and 
				type2 = "hermitian" and r=2 and IsOddInt(q2) then
				form := HermitianFormByMatrix(gram, f2);
			elif (IsOddInt(r) and type2 in ["elliptic", "parabolic", "hyperbolic"] and 
				type1 = type2) or (IsEvenInt(r) and [type1, type2] in [["elliptic","hyperbolic"], 
				["hyperbolic","hyperbolic"], ["parabolic","parabolic"]]) then
				gram := GramMatrix( QuadraticForm(geom1) );
				form := QuadraticFormByMatrix(gram, f2);
				form2 := QuadraticForm(geom2);           
			else
				Error("Not possible for these geometries\n");
			fi;   
		else
			Error("Dimensions and/or field sizes are incompatible"); return;
		fi;
		
		c1 := BaseChangeToCanonical( form );
		if not IsCanonicalPolarSpace( geom2 ) then
			c2 := BaseChangeToCanonical( form2 );
			change := c2^-1 * c1;
		else
			change := c1;
		fi;
		invchange := change^-1;

		d := geom1!.dimension+1;
		basis := Basis(AsVectorSpace(f1,f2));
		n := Length(basis);
		bmat := NullMat(d,d*n,f1);
		one := One(f1);
		for i in [1..d] do
			bmat[i,1+(i-1)*n] := one;
		od;

		func := function( el )
			return VectorSpaceToElement(geom2,Unpack(el!.obj) * invchange);
		end; 

		prefun := function( el )
			local vec,nvec,list,bgen;
			vec := Unpack(el!.obj) * change;
			if el!.type = 1 then
				nvec := vec / First(vec,x->not IsZero(x));
				if not ForAll( nvec, i -> i in f1 ) then
					Error("Element is not in the range of the geometry morphism");
				fi;	
				ConvertToVectorRepNC(nvec,f1); #necessary, also if appearantly vec is already over f1.
			else
				bgen := BlownUpMat(basis,vec);
				list := SumIntersectionMat(bgen, bmat)[2]; #hard coded Meet operation :-)
				nvec := List(list,x->ShrinkVec(f2,f1,x));
				if not (IsMatrix(nvec) and Length(nvec) = el!.type) then
					#return fail;
					Error("Element is not in the range of the geometry morphism");
				fi;
				ConvertToMatrixRepNC(nvec,f1);
			fi;
			return VectorSpaceToElement(geom1,nvec);
		end;
		
		map := GeometryMorphismByFunction( ElementsOfIncidenceStructure(geom1), 
                                       ElementsOfIncidenceStructure(geom2),
                                       func, false, prefun ); 
		SetIsInjective( map, true );
 
   		twinerfunc := function( x )
            local xmat;
            if not IsOne(x!.frob) then
				Info(InfoFinInG, 1, "<el> is not in the source of the intertwiner");
                return fail;
            fi;
            xmat := x!.mat;
            return CollineationOfProjectiveSpace( change * Unpack(x!.mat) * invchange, f2 ); #here was an unpack needed!
        end;

		twinerprefun := function( y )
            local ymat,q;
            if not IsOne(y!.frob) then
				Info(InfoFinInG, 1, "<el> is not in the range of the intertwiner");
                return fail;
            fi;
            ymat := invchange * Unpack(y!.mat) * change; #here too!
            ymat := ymat/First(ymat[1],x->not IsZero(x));
			if not ForAll( Flat(ymat), i -> i in f1 ) then
				Info(InfoFinInG, 1, "<el> is not in the range of the intertwiner");
				return fail;
			else
				ConvertToMatrixRepNC(ymat,f1);
                return CollineationOfProjectiveSpace(ymat,f1);
            fi;
        end;
        
        if HasCollineationGroup( geom1 ) then
			g1 := SimilarityGroup( geom1 );
			gens1 := GeneratorsOfGroup( g1 );
			gens2 := List(gens1, twinerfunc );
			g2 := Group(gens2);
			SetSize(g2, Size(g1));
			hom := GroupHomomorphismByFunction(g1, g2, twinerfunc, twinerprefun);    
			SetIntertwiner(map, hom);
		else
			Info(InfoFinInG, 1, "No intertwiner computed. <geom1> must have a collineation group computed");
		fi;
		
		return map;

	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalEmbeddingBySubfield( <geom1>, <geom2> )
# returns NaturalEmbeddingBySubfield( <geom1>, <geom2>, true )
##
InstallMethod( NaturalEmbeddingBySubfield, 
	"for two polar spaces",
	[ IsClassicalPolarSpace, IsClassicalPolarSpace ],
	function( geom1, geom2 )
		return NaturalEmbeddingBySubfield( geom1, geom2, true );
	end );


#############################################################################
#
# PROJECTIONS
#
#############################################################################

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalProjectionBySubspace( <ps>, <v> )
# returns the morphism from the projective space to the quotient space of the 
# subspace <v>. It is checked if <v> is a subspace of <ps>.
##
InstallMethod( NaturalProjectionBySubspace, 
	"for a projective space and a subspace of the projective space",
	[ IsProjectiveSpace, IsSubspaceOfProjectiveSpace ], 
	function(ps, v)

  ## map from geometry of elements incident with v
  ## to the projective space of lesser dimension 

    local psdim, b, vdim, vs, sub, func, pre,
          Wvectors, mb, compl, gen, bas, img, 
          canbas, zero, basimgs, ps2, map, hom, f;
    if not v in ps then
       Error("<v> is not an element of <ps>");
    fi;
    psdim := ps!.dimension;
    f := ps!.basefield;
    b := Unpack(v!.obj); #cmat unpack necessary for infra.
    vdim := v!.type;  
    if vdim = 1 then b:=[b]; fi;
    vs := ps!.vectorspace;
    sub := Subspace(vs, b);  
   
    ## if v has dimension j, then the new geometry 
    ## has dimension psdim - vdim

    ps2 := ProjectiveSpace( psdim - vdim, f );

      ## This code was adapted from Thomas Breuer's 
      ## "NaturalHomomorphismBySubspace" code
   
    Wvectors:= b;
    mb:= MutableBasis( f, Wvectors );
    compl:= [];
    for gen in BasisVectors( Basis( vs ) ) do
        if not IsContainedInSpan( mb, gen ) then
          Add( compl, gen );
          CloseMutableBasis( mb, gen );
        fi;
    od;
    bas := BasisNC( vs, Concatenation( Wvectors, compl ) );

        # Compute the linear mapping by images.
    img:= FullRowModule( f, Length( compl ) );
    canbas:= CanonicalBasis( img );
    zero:= Zero( img );
        # Matrix of the linear transformation we want
    basimgs:= Concatenation( ListWithIdenticalEntries( Size(Wvectors), zero ),
                             BasisVectors( canbas ) );

    func := function( x )
		local y;         
        if not v in x then
			Error("Subspace is not incident with the subspace of projection");
		fi;
        y := List(Unpack(Unwrap(x)),i-> Coefficients(bas,i))*basimgs;  #was x^_, now UnWrap(x), cmat unpack
        if not IsEmpty(y) then 
           ## Note: TriangulizeMat does not return a matrix of full
           ##       rank, whereas SemiEchelonMat does!
     		y := SemiEchelonMat(y)!.vectors;  ## JB: 3/11/2012. Moved this line from the "else" to here so it also applies to the first case.

			if x!.type - vdim = 1 then 
				y := y[1]; 
				#ConvertToVectorRep(y, f);
			fi;
			#else
			#	ConvertToMatrixRepNC(y, f);
			#fi;
        fi;
		if not IsEmpty(y) then  
			#return Wrap(ps2, x!.type - vdim, y);
            return VectorSpaceToElement(ps2,y);
			else  
				return EmptySubspace(ps2);
			fi;
		end;
		pre := function( y )
			local x;          
            x := Unpack(y!.obj);  #cmat unpack
            if y!.type = 1 then x := [x]; fi;
            x :=  Concatenation(x * compl, b); 
            #ConvertToMatrixRepNC(x, f);
            return VectorSpaceToElement(ps, x);
		end;
		map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps), 
                                      ElementsOfIncidenceStructure(ps2), func, pre );
		return map;
	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalProjectionBySubspaceNC( <ps>, <v> )
## This operation is just like its namesake except that it 
## has no checks
##
InstallMethod( NaturalProjectionBySubspaceNC, 
	"for a projective space and a singular subspace",
	[ IsProjectiveSpace, IsSubspaceOfProjectiveSpace ], 
	function(ps, v)

  ## map from geometry of elements incident with v
  ## to the projective space of lesser dimension 

    local psdim, b, vdim, vs, sub, func, pre,
          Wvectors, mb, compl, gen, bas, img, 
          canbas, zero, basimgs, ps2, map, hom, f;
  
    psdim := ps!.dimension;
    f := ps!.basefield;
    b := v!.obj;
    vdim := v!.type;  
    if vdim = 1 then b:=[b]; fi;
    vs := ps!.vectorspace;
    sub := Subspace(vs, b);  
   
    ## if v has dimension j, then the new geometry 
    ## has dimension psdim - vdim

    ps2 := ProjectiveSpace( psdim - vdim, f );

      ## This code was adapted from Thomas Breuer's 
      ## "NaturalHomomorphismBySubspace" code
   
    Wvectors:= b;
    mb:= MutableBasis( f, Wvectors );
    compl:= [];
    for gen in BasisVectors( Basis( vs ) ) do
        if not IsContainedInSpan( mb, gen ) then
          Add( compl, gen );
          CloseMutableBasis( mb, gen );
        fi;
    od;
    bas := BasisNC( vs, Concatenation( Wvectors, compl ) );

        # Compute the linear mapping by images.
    img:= FullRowModule( f, Length( compl ) );
    canbas:= CanonicalBasis( img );
    zero:= Zero( img );
        # Matrix of the linear transformation we want
    basimgs:= Concatenation( ListWithIdenticalEntries( Size(Wvectors), zero ),
                             BasisVectors( canbas ) );

    func := function( x )
              local y;         
              y := List(x!.obj,i-> Coefficients(bas,i))*basimgs;   
              y := SemiEchelonMat(y)!.vectors;     
              if x!.type - vdim = 1 then 
                 y := y[1]; 
                 ConvertToVectorRep(y, f);
              else
                 ConvertToMatrixRepNC(y, f);
              fi;
			  if not IsEmpty(y) then  
			 	 return Wrap(ps2, x!.type - vdim, y);
	          else  
			     return EmptySubspace(ps2);
			  fi;
			end;

    pre := function( y )
              local x;          
              x := y!.obj; 
              if y!.type = 1 then x := [x]; fi;
              x :=  Concatenation(x * compl, b); 
              ConvertToMatrixRepNC(x, f);
              return VectorSpaceToElement(ps, x);
           end;

    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps), 
                                      ElementsOfIncidenceStructure(ps2), func, pre );
    return map;
  end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  \/( <ps>, <v> )
# returns the quotient space of the subspace <v> of the projective space <ps>
# it is checked if <v> is a subspace of <ps>
##
InstallOtherMethod(\/,
	"for a projective space and a subspace",
	[ IsProjectiveSpace and IsProjectiveSpaceRep, IsSubspaceOfProjectiveSpace],
	function( ps, v )
		if not v in ps then 
			Error( "Subspace does not belong to the projective space" );
		fi;
		return Range( NaturalProjectionBySubspace( ps, v ) )!.geometry;
	end );

# CHECKED 28/09/11 jdb
#cmat change 21/3/14
#############################################################################
#O  NaturalProjectionBySubspace( <ps>, <v> )
# returns the morphism from the polar space to the quotient space of the 
# subspace <v>. It is checked if <v> is a subspace of <ps>.
##
InstallMethod( NaturalProjectionBySubspace, 
    "for a polar space and a subspace",
    [ IsClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace ], 
	function(ps, v)

  ## map from geometry of elements incident with v
  ## to the polar space of the same type but of lesser dimension 
    local pstype, psdim, b, vdim, vs, func, pre,
          Wvectors, mb, compl, gen, bas, img, canbas, zero, basimgs,
          ps2, map, hom, a, m, newform, f, perp;
    if not v in ps then
		Error("<v> is not an element of <ps>");
    fi;
    pstype := PolarSpaceType(ps); 
    psdim := ps!.dimension;
    f := ps!.basefield;
    b := Unpack(v!.obj); #cmat unpack necessary for infra.
    vdim := v!.type;  
    if vdim = 1 then b:=[b]; fi; 
    perp := PolarMap(ps);
    vs := VectorSpace(f, Unpack(perp(v)!.obj)); #PolarMap maps to elements, containing a cmat.

    ## if v has dimension j, then the new geometry 
    ## has dimension psdim - 2*vardim

    Wvectors:= b;
    mb:= MutableBasis( f, Wvectors );
    compl:= [];
    for gen in BasisVectors( Basis( vs ) ) do
        if not IsContainedInSpan( mb, gen ) then
          Add( compl, gen );
          CloseMutableBasis( mb, gen );
        fi;
    od;
    bas := BasisNC( vs, Concatenation( Wvectors, compl ) );

    # Compute the linear mapping by images.
    img:= FullRowModule( f, Length( compl ) );
    canbas:= CanonicalBasis( img );
    zero:= Zero( img );
    # Matrix of the linear transformation we want
    basimgs:= Concatenation( List( Wvectors, v -> zero ),
                             BasisVectors( canbas ) );
    #ConvertToMatrixRep(basimgs, f); #useles now.
    a := compl;

    if HasQuadraticForm( ps ) then
       m := QuadraticForm(ps)!.matrix;   
       newform := QuadraticFormByMatrix(a * m * TransposedMat(a), f);
    else
       m := SesquilinearForm(ps)!.matrix; 
       if pstype = "hermitian" then   
          #newform := HermitianFormByMatrix(a^CompanionAutomorphism(ps) * m * TransposedMat(a), f); #finally found the bug, jdb gets thirrrrrrsty now.
		  newform := HermitianFormByMatrix(a * m * TransposedMat(a^CompanionAutomorphism(ps)), f);

       else
          newform := BilinearFormByMatrix(a * m * TransposedMat(a), f);
       fi;
    fi;
    ps2 := PolarSpace( newform );  
  
  
    func := function( x )
              local y;         
              if not v in x then
                 Error("Subspace is not incident with the subspace of projection");
              fi;
              if not x in ps then
                 Error("Subspace is not an element of the polar space");
              fi;
              if v = x then return
                 EmptySubspace(ps2); 
              else
                 y := List(Unpack(x!.obj),i-> Coefficients(bas,i))*basimgs;  #cmat unpack here
                 y := SemiEchelonMat(y)!.vectors;   
                 if x!.type - vdim = 1 then 
                    y := y[1]; 
                    #ConvertToVectorRep(y, f);
                 #else
                 #   ConvertToMatrixRepNC(y, f);
                 fi;
                 return VectorSpaceToElement(ps2, y);
              fi;
           end;

    pre := function( y )
              local x;   
              if not y in ps2 then
                 Error("Subspace is not an element of the polar space");
              fi;
              x := Unpack(y!.obj); #cmat unpack here.
              if y!.type = 1 then x := [x]; fi;
              x := Concatenation(x * compl, StructuralCopy( b )); 
              #ConvertToMatrixRepNC(x, f); #useless now.
              return VectorSpaceToElement(ps, x);
            end;
    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps), 
                                      ElementsOfIncidenceStructure(ps2), func, false, pre );
    return map;
  end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalProjectionBySubspaceNC( <ps>, <v> )
## This operation is just like its namesake except that it 
## has no checks
##
InstallMethod( NaturalProjectionBySubspaceNC, 
      "for a polar space and a singular subspace",
      [ IsClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace ], 
  function(ps, v)

  ## map from geometry of elements incident with v
  ## to the polar space of the same type but of lesser dimension 

    local pstype, psdim, b, vdim, vs, func, pre,
          Wvectors, mb, compl, gen, bas, img, canbas, zero, basimgs,
          ps2, map, hom, a, m, newform, f, perp;
  
    pstype := PolarSpaceType(ps); 
    psdim := ps!.dimension;
    f := ps!.basefield;
    b := v!.obj;
    vdim := v!.type;  
    if vdim = 1 then b:=[b]; fi; 
    perp := PolarMap(ps);
    vs := VectorSpace(f, perp(v)!.obj);

    ## if v has dimension j, then the new geometry 
    ## has dimension psdim - 2*vdim

    Wvectors:= b;
    mb:= MutableBasis( f, Wvectors );
    compl:= [];
    for gen in BasisVectors( Basis( vs ) ) do
        if not IsContainedInSpan( mb, gen ) then
          Add( compl, gen );
          CloseMutableBasis( mb, gen );
        fi;
    od;
    bas := BasisNC( vs, Concatenation( Wvectors, compl ) );

    # Compute the linear mapping by images.
    img:= FullRowModule( f, Length( compl ) );
    canbas:= CanonicalBasis( img );
    zero:= Zero( img );
    # Matrix of the linear transformation we want
    basimgs:= Concatenation( List( Wvectors, v -> zero ),
                             BasisVectors( canbas ) );
    ConvertToMatrixRep(basimgs, f);
    a := compl;

    if HasQuadraticForm( ps ) then
       m := QuadraticForm(ps)!.matrix;   
       newform := QuadraticFormByMatrix(a * m * TransposedMat(a), f);
    else
       m := SesquilinearForm(ps)!.matrix; 
       if pstype = "hermitian" then   
          newform := HermitianFormByMatrix(a * m * TransposedMat(a^CompanionAutomorphism(ps)), f);
       else
          newform := BilinearFormByMatrix(a * m * TransposedMat(a), f);
       fi;
    fi;
    ps2 := PolarSpace( newform );  
  
    func := function( x )
              local y;         
              y := List(x!.obj,i-> Coefficients(bas,i))*basimgs;
              y := SemiEchelonMat(y)!.vectors;  
              if x!.type - vdim = 1 then 
                 y := y[1]; 
                 ConvertToVectorRepNC(y, f);
              else
                 ConvertToMatrixRepNC(y, f);
              fi;
              return VectorSpaceToElement(ps2, y);
            end;

    pre := function( y )
              local x;          
              x := y!.obj; 
              if y!.type = 1 then x := [x]; fi;
              x := Concatenation(x * compl, StructuralCopy( b )); 
              ConvertToMatrixRepNC(x, f);
              return VectorSpaceToElement(ps, x);
            end;
    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps), 
                                      ElementsOfIncidenceStructure(ps2), func, false, pre );
    return map;
  end );

#############################################################################
#
# Klein correspondence
#
#############################################################################

#############################################################################
# two helper operations. They can be used by the users, but on the other hand
# the form of the Klein quadric is fixed, so this is against the philiosphy of
# FinInG, at least for user functions. Also, there are no checks. So we will 
# describe these in the technical section of the documentation.
#############################################################################

# CHECKED 28/09/11 jdb
# changed 28/3/14 jdb
#############################################################################
#O  PluckerCoordinates( <lobj> )
# returns the Plucker coordinates of the line represented by <lobj>. We accept 
# a matrix representing the line. No check on whether this is a line of PG(3,q),`
# so use with care. cmat note: whether l is a cmat or not, coords will be a plain 
# list (so no cvec). This is the internal version accepting a matrix.
##
InstallMethod( PluckerCoordinates, 
	"for a matrix representing a line of PG(3,q)",
    [ IsMatrix ],
	function( lobj )
		local pij, u, v, coords;
		pij := function(u,v,i,j)
			return u[i]*v[j] - u[j] * v[i];
		end;
		u := lobj[1];
		v := lobj[2];
		coords := [pij(u,v,1,2),pij(u,v,1,3),pij(u,v,1,4),
			pij(u,v,2,3),pij(u,v,4,2),pij(u,v,3,4)];
		return coords;
	end );

# CHECKED 28/09/11 jdb
# changed 28/3/14 jdb
#############################################################################
#O  PluckerCoordinates( <line> )
# returns the Plucker coordinates of the line represented by <lobj>. We accept 
# a matrix representing the line. No check on whether this is a line of PG(3,q),`
# so use with care. cmat note: whether l is a cmat or not, coords will be a plain 
# list (so no cvec). This is the user version accepting a subspace of a projective
# space.
##
#InstallMethod( PluckerCoordinates,
#	"for a matrix representing a line of PG(3,q)",
#    [ IsSubspaceOfProjectiveSpace ],
#	function( l )
#		local pij, u, v, coords,lobj;
#		if l!.type <> 2 or Dimension(l!.geo) <> 3 then
#           Error(" <l> must be a line of a PG(3,q)");
#        fi;
#        lobj := l!.obj;
#        pij := function(u,v,i,j)
#			return u[i]*v[j] - u[j] * v[i];
#		end;
#		u := lobj[1];
#		v := lobj[2];
#		coords := [pij(u,v,1,2),pij(u,v,1,3),pij(u,v,1,4),
#			pij(u,v,2,3),pij(u,v,4,2),pij(u,v,3,4)];
#		return coords;
#	end );


# CHECKED 28/09/11 jdb
# changed 28/3/14 jdb
#############################################################################
#O  InversePluckerCoordinates( <var> )
# returns a list of two vectors spanning the line with Plucker coordinates <var>
# no check on whether this is really a line of the particular Q+(5,q):X0X5+X1X4+X2X3
# caution: the list l is not Triangulized!
# cmat note: l will be an ordinary matrix.
##
InstallMethod( InversePluckerCoordinates, 
	"for a vector repr. a point of Q+(5,q): X0X5+X1X4+X2X3=0",
    [ IsVector ],

   ## The point must satisfy the Pluecker relation x1x6+x2x5+x3x4=0.
	function( x )
		local pairs, i, pair, l, f, zero;
		pairs := [[1,2],[1,3],[1,4],[2,3],[4,2],[3,4]];
		i := PositionNonZero(x);
		pair := pairs[i];
		zero := Zero(x[1]);
        l := [];
		if 1 in pair then
			Add(l, [ zero, x[1], x[2], x[3] ]);
		fi;
		if 2 in pair then
			Add(l, [ x[1], zero, -x[4], x[5] ]);
		fi;
		if 3 in pair then
			Add(l, [ x[2], x[4], zero, -x[6] ]);
		fi;
		if 4 in pair then
			Add(l, [ x[3], -x[5], x[6], zero ]);
		fi;
		return l;
	end );

#############################################################################
# Klein correspondence: the user operations.
#############################################################################

# added may 2014 jdb
#############################################################################
#O  PluckerCoordinates( <l> )
# returns the Plucker coordinates of the line <l>. The is the user variant
# of the operation. It accepts a line of PG(3,q), does some checking, and
# uses the helper operation.
##
InstallMethod( PluckerCoordinates, 
	"for a line of PG(3,q)",
    [ IsSubspaceOfProjectiveSpace ],
	function( l )
		if Dimension(AmbientSpace(l)) <> 3 then
			Error("<l> is not a line of a three dimensional projective space");
		elif l!.type <> 2 then
			Error("<l> is not a line");
		fi;
		return PluckerCoordinates(l!.obj);
	end );

# added may 2014 jdb
#############################################################################
#O  KleinCorrespondence( <f>, <computeintertwiner> )
# returns the well known morphism from the lines of PG(3,f), f a finite field
# to the points of Q+(5,q): x0x5+x1x4+x2x3 = 0. This is the bare essential 
# of some geometry morphisms that follow. For didactical reasons, it looks 
# useful to me to have this version that maps to a fixed hyperbolic quadric. 
# Of course a flexible verion of this operation is also present.
##
# The interwiner: - the frobenius automorphism commutes with taking PluckerCoordinates
#					so we only have to deal with the matrices in the twiner/pretwiner
#					funcs.
#				  - A projectivity is known if its image of a frame is known. So:
#					going from PGL(4,q) -> PGO+(6,q): the standard basepoints of PG(5,q)
#					are points of the quadric, compute the lines (InversePlucker), their image,
#					, computer the point (Plucker), this gives the matrix of the element
#					of PGO+(5,q) where each row is determined up to a factor. It turns out
#					using order as in twinerfunc, the needed factors are always the same:
#					[1,1,1,1,1,-1]. This explains the '-' at newmat[6] line.
#					going from PGO+(5,q) -> PGL(4,q): cave canem: projectivities swapping
#					greek and latin planes do not induce a projectivity of PG(3,q)!
#					The principle is the same as above, just more work is needed to compute
#					the factors here (they seem not to be independant of the group element now).
#					Checking whether greek/latins are swapped is done by mapping a point
#					to a generator, computing the image, mapping back, and see whether
#					the three lines span PG(3,q). If so, we have the point, if not,
#					the image of the point is a plane and the projectivity of Q+(5,q)
#					swaps the greeks/latins and induces in fact a correlation of PG(3,q).
##
InstallMethod( KleinCorrespondence, 
	"for a finite field and a boolean",
    [ IsField, IsBool ],
	function( f, computeintertwiner )
		local i, form, map, pg, mat, pre, plucker, ps, inv, one, twinerfunc, 
		twinerprefun, coll1, gens1, coll2, gens2, hom, id;
		
		one := One(f);

    ## The form for the resulting Plucker coordinates is 
    ## x1x6+x2x5+x3x4 = 0
	
		mat := NullMat(6, 6, f);
		for i in [1..3] do
			mat[i,7-i] := one;
		od;
		form := QuadraticFormByMatrix(mat, f);
		ps := PolarSpace( form );
		pg := ProjectiveSpace(3,f);
		
		plucker := 
           function( l )
             local pt1;
             pt1 := PluckerCoordinates( l!.obj );
             return VectorSpaceToElement(ps, pt1);
           end;
		
		inv := 
           function( x )
             local l;
             l := InversePluckerCoordinates( x!.obj );
             return VectorSpaceToElement(pg, l);
           end;
		
		map := GeometryMorphismByFunction(Lines(pg), Points(ps), plucker, inv);
		SetIsBijective(map, true);
		
		twinerfunc := function(g)
			local mat,newmat;
			mat := Unpack(g!.mat);
			newmat := [];
			newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
			newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
			newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
			newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
			newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
			newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);
			return ProjElWithFrob(newmat,g!.frob,f);
		end;
		
		id := IdentityMat(4, f);
		
		twinerprefun := function( g )
			local mat, newmat, lines, pts, ipts, ilines, e, x, ept,
				ielines, ie, cs, iept;
			mat := Unpack(g!.mat);
			lines:= [];
			lines[1] := [[id[1],id[2]],[id[1],id[3]],[id[1],id[4]]];
			lines[2] := [[id[2],id[3]],[id[2],id[4]]];
			lines[3] := [[id[3],id[4]],[id[3],id[1]]];
			lines[4] := [[id[4],id[1]],[id[4],id[2]]];
			pts := List(lines,x->List(x,y->PluckerCoordinates(y)));
			ipts := List(pts,x->x*mat);
			ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
			if Rank(Union(ilines[1])) = 3 then
				Info(InfoFinInG, 1, "<el> is not inducing a collineation of PG(3,q)");
				return fail;
			fi;
			ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
			newmat := List(ipts,x->x[1]);
			e := [1,1,1,1]*one;
			ept := List([[e,id[1]],[e,id[2]]],y->PluckerCoordinates(y));
			iept := List(ept,x->x*mat);
			ielines := List(iept,x->InversePluckerCoordinates(x));
			ie := SumIntersectionMat(ielines[1],ielines[2])[2];
			cs := SolutionMat(newmat,ie[1]);
			for i in [1..4] do
				newmat[i] := newmat[i]*cs[i];
			od;
			return ProjElWithFrob(newmat,g!.frob,f);
		end;

		if computeintertwiner then
			coll1 := CollineationGroup(PG(3,f));
			gens1 := GeneratorsOfGroup(coll1);
			gens2 := List(gens1, twinerfunc);
			coll2 := GroupWithGenerators(gens2);
			hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun);
			SetIntertwiner( map, hom );
		fi;
		
		return map;
	end );

#############################################################################
#O  KleinCorrespondence( <q> )
# returns KleinCorrespondence( GF(q), true )
##
InstallMethod( KleinCorrespondence, 
	"for a prime power",
    [ IsPosInt ],
	q -> KleinCorrespondence(GF(q),true)
	);
	
#############################################################################
#O  KleinCorrespondence( <q> )
# returns KleinCorrespondence( GF(q), computeintertwiner )
##
InstallMethod( KleinCorrespondence, 
	"for a prime power and a boolean",
    [ IsPosInt, IsBool ],
	function(q, computeintertwiner )
		return KleinCorrespondence(GF(q),computeintertwiner);
	end );
    
#############################################################################
#O  KleinCorrespondence( <f> )
# returns KleinCorrespondence( GF(q), true )
##
InstallMethod( KleinCorrespondence, 
	"for a finite field",
    [ IsField ],
	f -> KleinCorrespondence(f,true)
	);

	
# CHECKED 28/09/11 jdb
#############################################################################
#O  KleinCorrespondence( <quadric>, <computeintertwiner> )
# returns the well known morphism from the lines of PG(3,q) to the points
# of Q+(5,q). Of course, the bilinear form determining the latter, can be chosen
# by the users. This method is exactly the same as the method for 
#  KleinCorrespondence( <f>, <computeintertwiner> ), but adds base changes.
##
InstallMethod( KleinCorrespondence, 
	"for a hyperbolic quadric and a boolean",
	[ IsClassicalPolarSpace, IsBool ],
	function( quadric, computeintertwiner )
		local f, i, form, map, pg, mat, twinerfunc, twinerprefun,
			pre, func, ps, iso, one, c, c1, c2, id, cinv, coll1, gens1,
			coll2, gens2, hom;
		if ProjectiveDimension(quadric) <> 5 then
			Error("<ps> is not Klein's quadric");
		fi;
		if not IsHyperbolicQuadric(quadric) then
   			Error("<ps> is not Klein's quadric");
		fi;
		f := quadric!.basefield;
		one := One(f);

    ## The form for the resulting Plucker coordinates is 
    ## x1x6+x2x5+x3x4 = 0
	
		mat := NullMat(6, 6, f);
		for i in [1..3] do
			mat[i,7-i] := one;
		od;
		form := QuadraticFormByMatrix(mat, f);
		ps := PolarSpace( form );
		c1 := BaseChangeToCanonical( form );
		if not IsCanonicalPolarSpace(quadric) then
			c2 := BaseChangeToCanonical(QuadraticForm(quadric));
			c := c2^-1 * c1;
		else
			c := c1;
		fi;
		cinv := c^-1;
		pg := ProjectiveSpace(3,f);
		
		func := function( el )
			local pt1;
			if el!.type <> 2 then
				Error("<el> is not a line ");
			fi;
			pt1 := PluckerCoordinates( el!.obj );
			return VectorSpaceToElement(quadric, pt1 * cinv ); #base change is here
        end;
		
		pre := function( el )
			local l;
			if el!.type <> 1 then
				Error("<el> is not a point ");
			fi;	
			l := InversePluckerCoordinates( el!.obj * c ); #base change is here
			return VectorSpaceToElement(pg, l);
        end;
		
		map := GeometryMorphismByFunction(Lines(pg), Points(quadric), func, pre);
		SetIsBijective(map, true);
		
		twinerfunc := function(g)
			local mat,newmat,frob;
			mat := Unpack(g!.mat);
			frob := g!.frob;
			newmat := [];
			newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
			newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
			newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
			newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
			newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
			newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);
			return ProjElWithFrob(c * newmat * cinv^frob,frob,f); #base change is here.
		end;
		
		id := IdentityMat(4, f);
		
		twinerprefun := function( g )
			local mat, newmat, lines, pts, ipts, ilines, e, x, ept,
				ielines, ie, cs, iept, frob;
			frob := g!.frob;
			mat := cinv * Unpack(g!.mat) * c^frob;      #base change is here.
			lines:= [];
			lines[1] := [[id[1],id[2]],[id[1],id[3]],[id[1],id[4]]];
			lines[2] := [[id[2],id[3]],[id[2],id[4]]];
			lines[3] := [[id[3],id[4]],[id[3],id[1]]];
			lines[4] := [[id[4],id[1]],[id[4],id[2]]];
			pts := List(lines,x->List(x,y->PluckerCoordinates(y)));
			ipts := List(pts,x->x*mat);
			ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
			if Rank(Union(ilines[1])) = 3 then
				Info(InfoFinInG, 1, "<el> is not inducing a collineation of PG(3,q)");
				return fail;
			fi;
			ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
			newmat := List(ipts,x->x[1]);
			e := [1,1,1,1]*one;
			ept := List([[e,id[1]],[e,id[2]]],y->PluckerCoordinates(y));
			iept := List(ept,x->x*mat);
			ielines := List(iept,x->InversePluckerCoordinates(x));
			ie := SumIntersectionMat(ielines[1],ielines[2])[2];
			cs := SolutionMat(newmat,ie[1]);
			for i in [1..4] do
				newmat[i] := newmat[i]*cs[i];
			od;
			return ProjElWithFrob(newmat,g!.frob,f);
		end;

		if computeintertwiner then
			coll1 := CollineationGroup(PG(3,f));
			gens1 := GeneratorsOfGroup(coll1);
			gens2 := List(gens1, twinerfunc);
			coll2 := GroupWithGenerators(gens2);
			hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun);
			SetIntertwiner( map, hom );
		fi;

		return map;

	end );

#############################################################################
#O  KleinCorrespondence( <quadric> )
# returns KleinCorrespondence( <quadric>, true )
##
InstallMethod( KleinCorrespondence, 
	"for a hyperbolic quadric",
    [ IsClassicalPolarSpace ],
	quadric -> KleinCorrespondence(quadric, true)
	);

# added may 2014 jdb.
#############################################################################
#O  KleinCorrespondenceExtended( <f>, <computeintertwiner> )
# I had really a lot of fun with the above methods and the mathematics behind.
# Therefore I decided to include an "extension" which also computes the image
# of points and planes of PG(3,q) under the Klein correspondence. This morphism
# becomes an isomorphism from PG(3,q) as incidence geometry to the incidence
# geometry consisting of the points and the two systems of generators of Q+(5,q).
# All mathematics is explained in the KleinCorrespondence variants above. For the 
# moment I recycle the interwiner from the appropriate KleinCorrespondence's.
# When I will have more time, I'll maybe implement the interwiner from the
# CorrelationCollineationGroup(PG(3,q)) to PGO+(6,q). 
##
InstallMethod( KleinCorrespondenceExtended, 
	"for a field and a boolean",
    [ IsField, IsBool ],
	function( f, computeintertwiner )
		local i, form, map, pg, mat, pre, func, ps, one, twinerfunc, 
		twinerprefun, coll1, gens1, coll2, gens2, hom, em;
		
		one := One(f);

    ## The form for the resulting Plucker coordinates is 
    ## x1x6+x2x5+x3x4 = 0
	
		mat := NullMat(6, 6, f);
		for i in [1..3] do
			mat[i,7-i] := one;
		od;
		form := QuadraticFormByMatrix(mat, f);
		ps := PolarSpace( form );
		pg := ProjectiveSpace(3,f);
		
		func := function( el )
			local t, vec, newvec, ptvec, elvec;
			t := el!.type;
			elvec := Unpack(el!.obj);
			if t = 1 then
				vec := BasisVectors(Basis(Lines(el)!.factorspace));
				newvec := [PluckerCoordinates([elvec,vec[1]]), PluckerCoordinates([elvec,vec[2]]), PluckerCoordinates([elvec,vec[3]])];
				return VectorSpaceToElement(ps,newvec);
			elif t = 2 then
				return VectorSpaceToElement(ps,PluckerCoordinates(elvec));
			elif t = 3 then
				newvec := [PluckerCoordinates( elvec{[1,2]}), PluckerCoordinates( elvec{[2,3]} ), PluckerCoordinates( elvec{[1,3]} )];
				return VectorSpaceToElement(ps,newvec);
			fi;
		end;
		
		pre := function( el )
			local elvec,t,newvec,lines,space;
			t := el!.type;
			elvec := Unpack(el!.obj);
			if t = 1 then
				newvec := InversePluckerCoordinates( elvec );
				return VectorSpaceToElement(pg, newvec);
			elif t = 3 then
				newvec := List(elvec,x->InversePluckerCoordinates( x ));
				lines := List(newvec,x->VectorSpaceToElement(pg,x));
				space := Meet(lines);
				if IsEmptySubspace(space) then
					return Span(lines);
				else
					return space;
				fi;
			elif t = 2 then
				Error("Lines of Klein quadric have no preimage");
			fi;
		end;
		
		map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(pg), ElementsOfIncidenceStructure(ps), func, pre);
		SetIsBijective(map, true);
		
		if computeintertwiner then
			em := KleinCorrespondence(f,computeintertwiner);
			hom := Intertwiner(em);
			SetIntertwiner( map, hom );
		fi;
		
		return map;
	end );

#############################################################################
#O  KleinCorrespondenceExtended( <q> )
# returns KleinCorrespondenceExtended( GF(q), true )
##
InstallMethod( KleinCorrespondenceExtended, 
	"for a prime power",
    [ IsPosInt ],
	q -> KleinCorrespondenceExtended(GF(q),true)
	);

# added may 2014 jdb.
#############################################################################
#O  KleinCorrespondenceExtended( <quadric>, <computeintertwiner> )
# The same as KleinCorrespondenceExtended, but accepting a user defined
# hyperbolic quadric
##
InstallMethod( KleinCorrespondenceExtended, 
	"for a field and a boolean",
    [ IsClassicalPolarSpace, IsBool ],
		function( quadric, computeintertwiner )
		local i, form, map, pg, mat, pre, func, ps, one, twinerfunc, f,
		twinerprefun, coll1, gens1, coll2, gens2, hom, em, c, c1, c2, cinv;

		f := quadric!.basefield;
		one := One(f);

    ## The form for the resulting Plucker coordinates is 
    ## x1x6+x2x5+x3x4 = 0
	
		mat := NullMat(6, 6, f);
		for i in [1..3] do
			mat[i,7-i] := one;
		od;
		form := QuadraticFormByMatrix(mat, f);
		ps := PolarSpace( form );
		pg := ProjectiveSpace(3,f);

		c1 := BaseChangeToCanonical( form );
		if not IsCanonicalPolarSpace(quadric) then
			c2 := BaseChangeToCanonical(QuadraticForm(quadric));
			c := c2^-1 * c1;
		else
			c := c1;
		fi;
		cinv := c^-1;
		
		func := function( el )
			local t, vec, newvec, ptvec, elvec;
			t := el!.type;
			elvec := Unpack(el!.obj);
			if t = 1 then
				vec := BasisVectors(Basis(Lines(el)!.factorspace));
				newvec := [PluckerCoordinates([elvec,vec[1]]), PluckerCoordinates([elvec,vec[2]]), PluckerCoordinates([elvec,vec[3]])];
				return VectorSpaceToElement(quadric,newvec * cinv); #base change is here...
			elif t = 2 then
				return VectorSpaceToElement(quadric,PluckerCoordinates(elvec) * cinv); # ... and here ...
			elif t = 3 then
				newvec := [PluckerCoordinates( elvec{[1,2]}), PluckerCoordinates( elvec{[2,3]} ), PluckerCoordinates( elvec{[1,3]} )];
				return VectorSpaceToElement(quadric,newvec * cinv); #... and here.
			fi;
		end;
		
		pre := function( el )
			local elvec,t,newvec,lines,space;
			t := el!.type;
			elvec := Unpack(el!.obj) * c; # base change is here.
			if t = 1 then
				newvec := InversePluckerCoordinates( elvec );
				return VectorSpaceToElement(pg, newvec);
			elif t = 3 then
				newvec := List(elvec,x->InversePluckerCoordinates( x ));
				lines := List(newvec,x->VectorSpaceToElement(pg,x));
				space := Meet(lines);
				if IsEmptySubspace(space) then
					return Span(lines);
				else
					return space;
				fi;
			elif t = 2 then
				Error("Lines of Klein's quadric have no preimage in the source of <em>");
			fi;
		end;
		
		map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(pg), ElementsOfIncidenceStructure(quadric), func, pre);
		SetIsBijective(map, true);
		
		if computeintertwiner then
			em := KleinCorrespondence(quadric,true);
			hom := Intertwiner(em);
			SetIntertwiner( map, hom );
		fi;
		
		return map;
	end );

#############################################################################
#O  KleinCorrespondenceExtended( <quadric> )
# returns KleinCorrespondenceExtended( quadric, true )
##
InstallMethod( KleinCorrespondenceExtended, 
	"for a prime power",
    [ IsClassicalPolarSpace ],
	quadric -> KleinCorrespondenceExtended(quadric,true)
	);


#############################################################################
#
# DUALITIES between W(q) and Q(4,q) and H(3,q^2) and Q-(5,q)
#
#############################################################################

#############################################################################
# two helper operations. These were derived from the previous version of 
# NaturalDuality. The new NaturalDuality will interface these.
#############################################################################

# Added april 2014. jdb
#############################################################################
#O  NaturalDualitySymplectic( <w>, <q4q>, <computeintertwiner>, <reverse> )
# returns the well known isomorphism between W(3,q) and Q(4,q). 
# Both polar spaces may be user defined. 
# Setup: - Plucker Coordinates map lines of PG(3,q) on points of Q+(5,q): X0X5+X1X4+X2X3 = 0
#        - restricting to lines of W(3,q), the range is the points of Q(4,q): X0^2 = X1X4+X2X3, which is the above 
#          intersecting with the hyperplane X0+X5 = 0.
#        - When using PluckerCoordinates, we only need the first 5 coordinates. When using InversePluckerCoordinates,
#          we make the sixth coordinate equal to minus the first.
#        - points of W(3,q): take its pole under the polarity associated with W(3,q), this defines a plane.
#          From this plane's underlying object, we have three lines -> three points of Q+(5,q) spanning a generator
#		   then intersect this generator with X0+X5=0 to find the line of Q(4,q).
#        - lines of Q(4,q): take the two spanning points -> two lines of W -> Meet of these lines is the point
#          we are looking for.
#        - some base changes are necessary.
#        - all the linear algebra is hard coded.
#        - the intertwiners: inspired by the intertwiners of the Klein Correspondence.
#		 - except for the filters, there is not real check whether the input the correct GQ.
#############################################################################
##
InstallMethod( NaturalDualitySymplectic,
	"for a symplectic GQ and a parabolic quadric",
	[ IsClassicalGQ, IsClassicalGQ, IsBool, IsBool ],
	function( w, q4q, computeintertwiner, reverse )
    local f, one, mat, form_quadric, quadric, data, form_w, cq4q, cw, can,
        func, pre, formw, c1, c2, delta, hyp, coll1, coll2, gens1, gens2, hom,
		twinerfunc, twinerprefun, cq4qinv, cwinv, id;
    f := w!.basefield;
    one := One(f);
    mat := NullMat(5, 5, f);
    mat[1,1] := one;
    mat[2,5] := -one;
    mat[3,4] := -one;
    form_quadric := QuadraticFormByMatrix(mat, f);
	c1 := BaseChangeToCanonical( form_quadric );
    if not IsCanonicalPolarSpace(q4q) then
        c2 := BaseChangeToCanonical(QuadraticForm(q4q));
        cq4q := c2^-1 * c1;
    else
        cq4q := c1;
    fi;
    cq4qinv := cq4q^-1;
	formw := SesquilinearForm( w );
    delta := PolarityOfProjectiveSpace(w);
    hyp := IdentityMat(6,f){[1..5]}; #this and next are in fact HyperplaneByDualCoorindates([1,0,0,0,0,1]..)
    hyp[1,6] := -one;
   	if IsCanonicalPolarSpace( w ) then
        func := function( el )
            local list,plane,vec;
            if el!.type = 2 then #dealing with a line
                return VectorSpaceToElement(q4q, PluckerCoordinates(el!.obj){[1..5]} * cq4q^-1 );
            else
                plane := el^delta;
                vec := Unpack(plane!.obj); #we have the plane, next line: use three lines in it.
                list := [ PluckerCoordinates(vec{[1,2]}), PluckerCoordinates(vec{[2,3]}), PluckerCoordinates(vec{[1,3]}) ];
       			plane := SumIntersectionMat(list, hyp)[2]; #hard coded Meet operation :-)
                list := List(plane,x->x{[1..5]}*cq4q^-1); #remember that we only need the first five coordinates, and do not forget base change
                return VectorSpaceToElement(q4q, list);
            fi;
        end;
        pre := function( el )
            local vec;
            if el!.type = 1 then #dealing with a point
                vec := Unpack(el!.obj)*cq4q;
                vec[6] := -vec[1];
                return VectorSpaceToElement(w, InversePluckerCoordinates(vec) );
            else
                vec := Unpack(el!.obj)*cq4q;
                vec[1][6] := -vec[1][1];
                vec[2][6] := -vec[2][1];
                return VectorSpaceToElement(w, SumIntersectionMat(InversePluckerCoordinates(vec[1]), InversePluckerCoordinates(vec[2]))[2] );
            fi;
        end;
    else
		cw := BaseChangeToCanonical( formw );
        func := function( el )
            local list,plane,vec;
                if el!.type = 2 then #dealing with a line
                return VectorSpaceToElement(q4q, PluckerCoordinates(Unpack(el!.obj)*cw^-1){[1..5]} * cq4q^-1);
            else
                plane := el^delta;
                vec := Unpack(plane!.obj)*cw^-1;
                list := [ PluckerCoordinates(vec{[1,2]}), PluckerCoordinates(vec{[2,3]}), PluckerCoordinates(vec{[1,3]}) ];
       			plane := SumIntersectionMat(list, hyp)[2];                 
                list := List(plane,x->x{[1..5]}*cq4q^-1);
                return VectorSpaceToElement(q4q, list);
            fi;
        end;
        pre := function( el )
            local vec;
            if el!.type = 1 then #dealing with a point
                vec := Unpack(el!.obj)*cq4q;
                vec[6] := -vec[1];
                return VectorSpaceToElement(w, InversePluckerCoordinates(vec) * cw);
            else;
                vec := Unpack(el!.obj)*cq4q;
                vec[1][6] := -vec[1][1];
                vec[2][6] := -vec[2][1];
                return Meet( VectorSpaceToElement(w, InversePluckerCoordinates(vec[1]) * cw ),
                                VectorSpaceToElement(w, InversePluckerCoordinates(vec[2]) * cw ) );
            fi;
        end;
    fi;
     
	if not reverse then
		data := rec( func := func, pre := pre);
	else
		data := rec( func := pre, pre := func);
	fi;

   	if computeintertwiner then

		id := IdentityMat(4, f);
	
		if IsCanonicalPolarSpace( w ) then
    
			twinerfunc := function(g)
				local mat,newmat,frob;
				mat := Unpack(g!.mat);
				frob := g!.frob;
				newmat := [];
				newmat[2] := PluckerCoordinates([mat[3],mat[1]]){[1..5]};
				newmat[3] := PluckerCoordinates([mat[4],mat[1]]){[1..5]};
				newmat[4] := -PluckerCoordinates([-mat[3],mat[2]]){[1..5]};
				newmat[5] := PluckerCoordinates([mat[4],-mat[2]]){[1..5]};
				newmat[1] := PluckerCoordinates([mat[2]+mat[3],mat[1]+mat[4]]){[1..5]} - newmat[2] - newmat[5];
				return ProjElWithFrob(cq4q * newmat * cq4qinv^(frob^-1),frob,f); #base change is here.
				return newmat;
			end;
	
			twinerprefun := function( g )
				local mat, newmat, lines, pts, ipts, ilines, e, x, ept,
					ielines, ie, cs, iept, frob, i, j;
				frob := g!.frob;
				mat := cq4qinv * Unpack(g!.mat) * cq4q^(frob^-1);
				lines:= [];
				lines[1] := [[id[1],id[3]],[id[1],id[4]]];
				lines[2] := [[id[2],id[3]],[id[2],id[4]]];
				lines[3] := [[id[3],id[1]],[id[3],id[2]]];
				lines[4] := [[id[4],id[1]],[id[4],id[2]]];
				pts := List(lines,x->List(x,y->PluckerCoordinates(y){[1..5]}));
				ipts := List(pts,x->x*mat);
				for i in [1..Length(ipts)] do
					for j in [1..2] do
						ipts[i][j][6] := -ipts[i][j][1];
					od;
				od;
				ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
				ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
				newmat := List(ipts,x->x[1]);
				e := [1,1,1,1]*one;
				ept := List([[e,id[1]+id[4]],[e,id[1]+id[2]]],y->PluckerCoordinates(y){[1..5]});
				iept := List(ept,x->x*mat);
				iept[1][6] := -iept[1][1];
				iept[2][6] := -iept[2][1];

				ielines := List(iept,x->InversePluckerCoordinates(x));
				ie := SumIntersectionMat(ielines[1],ielines[2])[2];
				cs := SolutionMat(newmat,ie[1]);
				for i in [1..4] do
					newmat[i] := newmat[i]*cs[i];
				od;
				return ProjElWithFrob(newmat,g!.frob,f);
			end;

		else

			cw := BaseChangeToCanonical( SesquilinearForm(w) );
			cwinv := cw^-1;

			twinerfunc := function(g)
				local mat,newmat,frob;
				frob := g!.frob;
				mat := cw * Unpack(g!.mat) * cwinv^(frob^-1);
				newmat := [];
				newmat[2] := PluckerCoordinates([mat[3],mat[1]]){[1..5]};
				newmat[3] := PluckerCoordinates([mat[4],mat[1]]){[1..5]};
				newmat[4] := -PluckerCoordinates([-mat[3],mat[2]]){[1..5]};
				newmat[5] := PluckerCoordinates([mat[4],-mat[2]]){[1..5]};
				newmat[1] := PluckerCoordinates([mat[2]+mat[3],mat[1]+mat[4]]){[1..5]} - newmat[2] - newmat[5];
				return ProjElWithFrob(cq4q * newmat * cq4qinv^(frob^-1),frob,f); #base change is here.
				#return newmat;
			end;

			twinerprefun := function( g )
				local mat, newmat, lines, pts, ipts, ilines, e, x, ept,
					ielines, ie, cs, iept, frob, i, j;
				frob := g!.frob;
				mat := cq4qinv * Unpack(g!.mat) * cq4q^(frob^-1);
				lines:= [];
				lines[1] := [[id[1],id[3]],[id[1],id[4]]];
				lines[2] := [[id[2],id[3]],[id[2],id[4]]];
				lines[3] := [[id[3],id[1]],[id[3],id[2]]];
				lines[4] := [[id[4],id[1]],[id[4],id[2]]];
				pts := List(lines,x->List(x,y->PluckerCoordinates(y){[1..5]}));
				ipts := List(pts,x->x*mat);
				for i in [1..Length(ipts)] do
					for j in [1..2] do
						ipts[i][j][6] := -ipts[i][j][1];
					od;
				od;
				ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
				ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
				newmat := List(ipts,x->x[1]);
				e := [1,1,1,1]*one;
				ept := List([[e,id[1]+id[4]],[e,id[1]+id[2]]],y->PluckerCoordinates(y){[1..5]});
				iept := List(ept,x->x*mat);
				iept[1][6] := -iept[1][1];
				iept[2][6] := -iept[2][1];
	
				ielines := List(iept,x->InversePluckerCoordinates(x));
				ie := SumIntersectionMat(ielines[1],ielines[2])[2];
				cs := SolutionMat(newmat,ie[1]);
				for i in [1..4] do
					newmat[i] := newmat[i]*cs[i];
				od;
				return ProjElWithFrob(cwinv * newmat * cw^(frob^-1),g!.frob,f);
			end;
		fi;
		if not reverse then		
			data.twinerfunc := twinerfunc;
			data.twinerprefun := twinerprefun;
		else
			data.twinerfunc := twinerprefun;
			data.twinerprefun := twinerfunc;
		fi;
	fi;
	
    return data;
 end );

# Added april 2014. jdb
#############################################################################
#O  NaturalDualityHermitian( <h>, <q5q>, <computeintertwiner>, <reverse> )
# returns the well known isomorphism between H(3,q^2) and Q-(5,q). 
# Both polar spaces may be user defined. 
# Setup: - Plucker Coordinates map lines of PG(3,q^2) on points of Q+(5,q^2): X0X5+X1X4+X2X3 = 0
#        - restricting to lines of H(3,q^2), the range is the points of Q-(5,q). The details (thanks jb for pointing these out!)
#			1. Suppose l is a line of H(3,q^2), pl its Plucker coordinates. 
#			then up to a scalar, pl is of the form (x,y,z,z^q,y^q,x^q). We first make sure that
#			our vector is really in this form (up to no scalar!).
#           2. The projectivity of PG(5,q^2) which maps the Plucker coordinates of the lines of H(3,q^2)
#           to PG(5,q). See "Finite Projective Spaces of Three Dimensions" by Hirschfeld and Thas (section 19.2).
#			is defined.
#			3. Applying this collineation on plucker coordinates (in the right form, see above), makes
#			sure this is a point of the Elliptic Quadric Q-(5,q): x1^2+x2^2+x3^2+x4^2+x5^2+x6^2 - e(x1x6+x2x5+x3x4)  
#			4. The result pl*x might be over GF(q) modulo a GF(q^2)
#			factor, then this causes problems in the VectorSpaceToElement
#			call, since using cvec/cmat, now requires this to be a vector
#			over GF(q). So we have to normalize first, then do the base change
#			then use VectorSpaceToElement.
#		 - the inverse is easier: the combination of InversePluckerCoordinates and xinv.
#		 - points of H(3,q^2): take its pole under the polarity associated with H(3,q^2), this defines a plane.
#          From this plane's underlying object, we have three lines -> three points of Q+(5,q^2) spanning a generator.
#		   Apply x on this generator and the intersection with the standard PG(5,q) yields a line of Q^-(5,q).
#		   To compute this intersection we first blow up, use the hard coded Meet, and then Shrink.
#        - lines of Q^-(5,q): take the two spanning points -> two lines of H -> Meet of these lines is the point
#          we are looking for.
#        - some base changes are necessary.
#        - all the linear algebra is hard coded.
#		 - except for the filters, there is not real check whether the input the correct GQ.
#        - the intertwiners: I can tell you: this is pain in the ass!
#        - twinerfun: from PGammaU(4,q^2) to PGammaO-(6,q):
#           1. part one is taken from the Klein correspondence. This gives an element of
#               PGammaO+(6,q^2), stabilizing some pointset that is actually a Q-(5,q).
#               We know the collineation x, and use it to make this a collineation of the Q-(5,q)
#           2. The frobenius automorphism of this collineation is just the acion of frob on the 
#               subfield.
#        - twinerprefun: 
#           1. part one goes from Q-(5,q) to Q+(5,q^2). Note that we need to compute frob2 from 
#            the given frob, since although frob acts also on the big field, its inverse is not
#            the same, and this is really important!
#           2. part two starts a bit like the Klein correspondence. If the greeks/latins are
#            swapped by the computed collineation of Q+, we apply the special collineation.
#            this swaps back the greeks/latins, and stabilizes the Q-(5,q). Now we can use the
#            same code as in the Klein correspondence, modulo the switch field automorphism when
#              necessary.
#############################################################################
##
InstallMethod( NaturalDualityHermitian,
	"for H(3,q^2) and Q-(5,q)",
	[ IsClassicalGQ,  IsClassicalGQ, IsBool, IsBool ],
	function( h, q5q, computeintertwiner, reverse )
    ## The way this works is that we map the lines of h to the canonical H(3,q^2),
    ## which Klein corresponds to points of Q+(5,q^2) using the usual plucker map.
    ## This subgeometry is then mapped to PG(5,q), where we can associate it to
    ## Q-(5,q).

    #local f, frob, q, one, iso, eq, pg5, alpha, mat1, mat2, x, xinv,
    #      func, pre, i, map, e, ps, form, iso2;
    
    local f, frob, q, one, alpha, e, mat1, mat2, i, form_quadric, c1, c2, cq5q,
            func, pre, data, x, xinv, formh, ch, chinv, bmat, delta, basis, twinerfunc,
            twinerprefun, coll1,coll2, gens1, gens2, cq5qinv, hom, id, id6, special, theta;

    f := h!.basefield;
    frob := FrobeniusAutomorphism(f);
    q := Sqrt(Size(f));
    one := One(f);   
    
    #first set up the elliptic quadric we get if we start from the standard hermitian polar space.
    
    alpha := First(f, a -> (a^q)^2 <> a^2);
    e := alpha^(q-1) + alpha^(1-q);

    ## The form for the Elliptic Quadric that we get is
    ## x1^2+x2^2+x3^2+x4^2+x5^2+x6^2 - e(x1x6+x2x5+x3x4)  

    mat1 := IdentityMat(6, GF(q));
    mat2 := NullMat(6, 6, f);
    for i in [1..3] do
       mat2[2*i-1,8-2*i] := one;
    od; 
    mat1 := mat1 - e * mat2;
    form_quadric := QuadraticFormByMatrix(mat1, GF(q));

    # do the necessary base changes.

	c1 := BaseChangeToCanonical( form_quadric );
    if not IsCanonicalPolarSpace(q5q) then
        c2 := BaseChangeToCanonical(QuadraticForm(q5q));
        cq5q := c2^-1 * c1;
    else
        cq5q := c1;
    fi;
    cq5qinv := cq5q^-1;

	#see comments in header which collineation x represents.
    
	mat1 := IdentityMat(6, f) * alpha;
    mat2 := NullMat(6, 6, f);
    for i in [1..6] do
       mat2[i,7-i] := one;
    od;
    x := mat1 + mat2 * alpha^q;
    x := List(x,y->y);
    xinv := x^-1;

	formh := SesquilinearForm( h );

    delta := PolarityOfProjectiveSpace(h);
	bmat := NullMat(6,12,GF(q));
	for i in [1..6] do
		bmat[i,2*i-1] := one;
	od;
	basis := Basis(AsVectorSpace(GF(q),GF(q^2)));

    if IsCanonicalPolarSpace( h ) then
       func := function( el )
            local pl, mu, pos, vec, plane, bgen, list;
            
			if el!.type = 2 then #dealing with a line
				pl := PluckerCoordinates( Unpack(el!.obj) ); #the Unpack might be unnecessary, but harmless anyway.
				#see the comments in the header for what we do here.
				pos := PositionNonZero( pl );
				mu := First( AsList(f), m -> m^(q-1) = pl[pos] / pl[7-pos]);
				pl := mu * pl;
				vec := pl*x;
				MultVector(vec,Inverse( vec[PositionNonZero(vec)] ));
				return VectorSpaceToElement(q5q, vec * cq5q^-1);
			else
				plane := el^delta;
                vec := Unpack(plane!.obj); #we have the plane, next line: use three lines in it.
                list := [ PluckerCoordinates(vec{[1,2]}), PluckerCoordinates(vec{[2,3]}), PluckerCoordinates(vec{[1,3]}) ];
				bgen := BlownUpMat(basis,list*x);
				list := SumIntersectionMat(bgen, bmat)[2]; #hard coded Meet operation :-)
				vec := List(list,x->ShrinkVec(GF(q^2),GF(q),x));
                return VectorSpaceToElement(q5q, vec * cq5q^-1);				
			fi;
		end;
 
        pre := function( el )
			local vec ;
            if el!.type = 1 then #dealing with a point
				vec := Unpack(el!.obj)*cq5q;
				return VectorSpaceToElement(h, InversePluckerCoordinates(vec * xinv) );
			else
				vec := (Unpack(el!.obj)*cq5q)*xinv;
                return VectorSpaceToElement(h, SumIntersectionMat(InversePluckerCoordinates(vec[1]), InversePluckerCoordinates(vec[2]))[2] );
			fi;
		end;
    else
   		ch := BaseChangeToCanonical( formh );
        func := function( el )
            local pl, mu, pos, vec, plane, bgen, list;
   			if el!.type = 2 then #dealing with a line
				pl := PluckerCoordinates( Unpack(el!.obj)*ch^-1 );
				pos := PositionNonZero( pl );
				mu := First( AsList(f), m -> m^(q-1) = pl[pos] / pl[7-pos]);
				pl := mu * pl;
				vec := pl*x;
				MultVector(vec,Inverse( vec[PositionNonZero(vec)] ));
				return VectorSpaceToElement(q5q, vec * cq5q^-1);
			else
				plane := el^delta;
                vec := Unpack(plane!.obj)*ch^-1; #we have the plane, next line: use three lines in it.
                list := [ PluckerCoordinates(vec{[1,2]}), PluckerCoordinates(vec{[2,3]}), PluckerCoordinates(vec{[1,3]}) ];
				bgen := BlownUpMat(basis,list*x);
				list := SumIntersectionMat(bgen, bmat)[2]; #hard coded Meet operation :-)
				vec := List(list,x->ShrinkVec(GF(q^2),GF(q),x));
                return VectorSpaceToElement(q5q, vec * cq5q^-1);				
			fi;
		end;
        
		pre := function( el )
            local vec;
            if el!.type = 1 then #dealing with a point
				vec := Unpack(el!.obj)*cq5q;
				return VectorSpaceToElement(h, InversePluckerCoordinates(vec * xinv) * ch );
			else
				vec := (Unpack(el!.obj)*cq5q)*xinv;
                return VectorSpaceToElement(h, SumIntersectionMat(InversePluckerCoordinates(vec[1]), InversePluckerCoordinates(vec[2]))[2] * ch );
			fi;
		end;

    fi;

    #map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(h), ElementsOfIncidenceStructure(q5q), func, pre);
    #SetIsBijective( map, true );

	if not reverse then
		data := rec( func := func, pre := pre);
	else
		data := rec( func := pre, pre := func);
	fi;
   	
	if computeintertwiner then
		id := IdentityMat(4,f);
		id6 := IdentityMat(6,f);
		special := List([1..6],i->id6[7-i]);
		i := Log(q,Characteristic(f));
		theta := FrobeniusAutomorphism(f)^i;

		if IsCanonicalPolarSpace( h ) then
			twinerfunc := function(g)
				local mat,newmat,frob,newmat2,n,frob2,j,arg;
				frob := g!.frob;
				mat := Unpack(g!.mat);
				newmat := [];
				newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
				newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
				newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
				newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
				newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
				newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);
				newmat2 :=  xinv*newmat*x^(frob^-1);
				n := First(newmat2[1],x->not IsZero(x));
				newmat2 := newmat2/n;
				if not IsOne(frob) then
					j := Log(frob!.power,Characteristic(f));
				else
					j := 0;
				fi;
				frob2 := FrobeniusAutomorphism(GF(q))^(j mod q);
				arg := cq5q * newmat2 * cq5qinv^(frob2^-1);
				ConvertToMatrixRep(arg);
				return ProjElWithFrob(arg,frob2,GF(q));
			end;

			twinerprefun := function( g )
				local mat, newmat, lines, pts, ipts, ilines, e, ept,
					ielines, ie, cs, iept, frob, frob2, j, n, switch;
					frob := g!.frob;
				#first setup the frobenius automorphism over GF(q^2)=f.
				if not IsOne(frob) then
					j := Log(frob!.power,Characteristic(f));
				else
					j := 0;
				fi;
				frob2 := FrobeniusAutomorphism(f)^j;
				#base change. Note that frob is used to change from different Q-(5,q)'s, frob2 for Q-(5,q) -> Q+(5,q^2)
				mat := x * (cq5qinv * Unpack(g!.mat) * cq5q^(frob^-1) ) * xinv^(frob2^-1);      #base change is here.
				#now from Q+(5,q^2) to H(3,q^2).
				lines:= [];
				pts := [];
				ipts := [];
				ilines := [];
				lines[1] := [[id[1],id[2]],[id[1],id[3]],[id[1],id[4]]];
				pts[1] := List(lines[1],y->PluckerCoordinates(y));
				ipts[1] := (pts[1]*mat);
				ilines[1] := List(ipts[1],y->InversePluckerCoordinates(y));
				switch := theta^0;
				if Rank(Union(ilines[1])) = 3 then
					#Print("switch\n");
					mat := (mat^theta) * special;
					frob2 := frob2 * theta;
					switch := theta;
				fi;
				n := First(mat[1],x->not IsZero(x));
				mat := mat/n;
				lines[1] := [[id[1],id[2]],[id[1],id[3]]];
				lines[2] := [[id[2],id[3]],[id[2],id[4]]];
				lines[3] := [[id[3],id[4]],[id[3],id[1]]];
				lines[4] := [[id[4],id[1]],[id[4],id[2]]];
				pts := List(lines,x->List(x,y->PluckerCoordinates(y)));
				ipts := List(pts,x->(x*mat)^switch);
				#ipts := List(pts,x->(x*mat));
				ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
				ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
				newmat := List(ipts,x->x[1]);
				e := [1,1,1,1]*one;
				ept := List([[e,id[1]],[e,id[2]]],y->PluckerCoordinates(y));
				iept := List(ept,x->(x*mat)^switch);
				#iept := List(ept,x->(x*mat));
				ielines := List(iept,x->InversePluckerCoordinates(x));
				ie := SumIntersectionMat(ielines[1],ielines[2])[2];
				cs := SolutionMat(newmat,ie[1]);
				for i in [1..4] do
					newmat[i] := newmat[i]*cs[i];
				od;
				return ProjElWithFrob(newmat,frob2,f);
			end;

		else

			ch := BaseChangeToCanonical( SesquilinearForm(h) );
			chinv := ch^-1;
        
			twinerfunc := function(g)
				local mat,newmat,frob,newmat2,n,frob2,j,arg;
				frob := g!.frob;
				mat := ch * Unpack(g!.mat) * chinv^(frob^-1);
				newmat := [];
				newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
				newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
				newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
				newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
				newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
				newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);
				newmat2 :=  xinv*newmat*x^(frob^-1);
				n := First(newmat2[1],x->not IsZero(x));
				newmat2 := newmat2/n;
				if not IsOne(frob) then
					j := Log(frob!.power,Characteristic(f));
				else
					j := 0;
				fi;
				frob2 := FrobeniusAutomorphism(GF(q))^(j mod q);
				arg := ShallowCopy(cq5q * newmat2 * cq5qinv^(frob2^-1));
				ConvertToMatrixRep(arg);
				return ProjElWithFrob(arg,frob2,GF(q));
			end;

			twinerprefun := function( g )
				local mat, newmat, lines, pts, ipts, ilines, e, ept,
					ielines, ie, cs, iept, frob, frob2, j, n, switch;
				frob := g!.frob;
				#first setup the frobenius automorphism over GF(q^2)=f.
				if not IsOne(frob) then
					j := Log(frob!.power,Characteristic(f));
				else
					j := 0;
				fi;
				frob2 := FrobeniusAutomorphism(f)^j;
				#base change. Note that frob is used to change from different Q-(5,q)'s, frob2 for Q-(5,q) -> Q+(5,q^2)
				mat := x * (cq5qinv * Unpack(g!.mat) * cq5q^(frob^-1) ) * xinv^(frob2^-1);      #base change is here.
				#now from Q+(5,q^2) to H(3,q^2).
				lines:= [];
				pts := [];
				ipts := [];
				ilines := [];
				lines[1] := [[id[1],id[2]],[id[1],id[3]],[id[1],id[4]]];
				pts[1] := List(lines[1],y->PluckerCoordinates(y));
				ipts[1] := (pts[1]*mat);
				ilines[1] := List(ipts[1],y->InversePluckerCoordinates(y));
				switch := theta^0;
				if Rank(Union(ilines[1])) = 3 then
					#Print("switch\n");
					mat := (mat^theta) * special;
					frob2 := frob2 * theta;
					switch := theta;
				fi;
				n := First(mat[1],x->not IsZero(x));
				mat := mat/n;
				lines[1] := [[id[1],id[2]],[id[1],id[3]]];
				lines[2] := [[id[2],id[3]],[id[2],id[4]]];
				lines[3] := [[id[3],id[4]],[id[3],id[1]]];
				lines[4] := [[id[4],id[1]],[id[4],id[2]]];
				pts := List(lines,x->List(x,y->PluckerCoordinates(y)));
				ipts := List(pts,x->(x*mat)^switch);
				#ipts := List(pts,x->(x*mat));
				ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
				ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
				newmat := List(ipts,x->x[1]);
				e := [1,1,1,1]*one;
				ept := List([[e,id[1]],[e,id[2]]],y->PluckerCoordinates(y));
				iept := List(ept,x->(x*mat)^switch);
				#iept := List(ept,x->(x*mat));
				ielines := List(iept,x->InversePluckerCoordinates(x));
				ie := SumIntersectionMat(ielines[1],ielines[2])[2];
				cs := SolutionMat(newmat,ie[1]);
				for i in [1..4] do
					newmat[i] := newmat[i]*cs[i];
				od;
				return ProjElWithFrob(chinv * newmat * ch^(frob2^-1),frob2,f);
			end;

		fi;
		if not reverse then		
			data.twinerfunc := twinerfunc;
			data.twinerprefun := twinerprefun;
		else
			data.twinerfunc := twinerprefun;
			data.twinerprefun := twinerfunc;
		fi;
    fi;

	return data;
	
end );

# Added april 2014 jdb.
#############################################################################
#O  NaturalDuality( <gq1>, <gq2>, <bool> )
# This is the interface to the helper functions. It simply checks the input 
# and decides which NaturalDuality... to use.
##
InstallMethod( NaturalDuality,
	"for a GQ and a GQ",
	[ IsClassicalGQ, IsClassicalGQ, IsBool ],
	function( gq1, gq2, computeintertwiner )
	local bf1,bf2,data, coll1,coll2,gens1,gens2, map, hom;
	bf1 := gq1!.basefield;
	bf2 := gq2!.basefield;
    if IsSymplecticSpace( gq1 ) and IsParabolicQuadric( gq2 ) then
        if bf1 <> bf2 then
			Error("<gq1> and <gq2> have a different base field");
		fi;
		data := NaturalDualitySymplectic( gq1, gq2, computeintertwiner, false);
	elif (IsHermitianPolarSpace( gq1 ) and Dimension( gq1 ) = 3) and IsEllipticQuadric( gq2) then
		if Size(bf1) <> Size(bf2)^2 then
			Error("base field of <gq1> must have order the square of the base field of <gq2>");
		fi;
		data := NaturalDualityHermitian( gq1, gq2, computeintertwiner, false);
    elif IsParabolicQuadric( gq1 ) and IsSymplecticSpace( gq2 ) then
        if bf1 <> bf2 then
			Error("<gq1> and <gq2> have a different base field");
		fi;
		data := NaturalDualitySymplectic( gq2, gq1, computeintertwiner, true);
	elif IsEllipticQuadric( gq1 ) and (IsHermitianPolarSpace( gq2 ) and Dimension( gq2 ) = 3) then
		if Size(bf1^2) <> Size(bf2) then
			Error("base field of <gq2> must have order the square of the base field of <gq1>");
		fi;
		data := NaturalDualityHermitian( gq2, gq1, computeintertwiner, true);
	else
        Error("no duality possible between <gq1> and <gq2>");
    fi;
	map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(gq1), ElementsOfIncidenceStructure(gq2),
					data.func, data.pre);
	SetIsBijective( map, true );
    if computeintertwiner then
        if (HasIsCanonicalPolarSpace( gq1 ) and IsCanonicalPolarSpace( gq1 )) or
            HasCollineationGroup( gq1 ) then
			coll1 := CollineationGroup(gq1);
			gens1 := GeneratorsOfGroup( coll1 );
			gens2 := List(gens1, data.twinerfunc);
			coll2 := GroupWithGenerators(gens2);
			hom := GroupHomomorphismByFunction(coll1, coll2, data.twinerfunc, data.twinerprefun);
			SetIntertwiner( map, hom );
			UseIsomorphismRelation(coll1,coll2);
        elif (HasIsCanonicalPolarSpace( gq2 ) and IsCanonicalPolarSpace( gq2 )) or
			HasCollineationGroup( gq2 ) then
			coll2 := CollineationGroup( gq2 );
			gens2 := GeneratorsOfGroup( coll2 );
			gens1 := List(gens2, data.twinerprefun );
			coll1 := GroupWithGenerators(gens1);
			hom := GroupHomomorphismByFunction(coll1, coll2, data.twinerfunc, data.twinerprefun);
			SetIntertwiner( map, hom );
			UseIsomorphismRelation(coll1,coll2);
		else
			Info(InfoFinInG, 1, "No intertwiner computed. One of the polar spaces must have a collineation group computed");
		fi;
    fi;

	return map;
end );

# Added may 2014 jdb.
#############################################################################
#O  NaturalDuality( <gq1>, <gq2> )
# returns NaturalDuality( <gq1>, <gq2>, true)
##
InstallMethod( NaturalDuality,
	"for a GQ and a GQ",
	[ IsClassicalGQ, IsClassicalGQ ],
	function(gq1,gq2)
		return NaturalDuality(gq1,gq2,true);
	end);

# Added may 2014 jdb.
#############################################################################
#O  NaturalDuality( <gq1>, <computeintertwiner> )
# This is the interface to the helper functions. It simply checks the input 
# and decides which NaturalDuality... to use.
##
InstallMethod( NaturalDuality,
	"for a GQ and a GQ",
	[ IsClassicalGQ, IsBool ],
	function( gq1, computeintertwiner )
	local q;
    if IsSymplecticSpace( gq1 ) then
        return NaturalDuality( gq1, ParabolicQuadric(4, BaseField(gq1)), computeintertwiner ) ;
    elif (IsHermitianPolarSpace( gq1 ) and Dimension( gq1 ) = 3) then
        q := Sqrt(Size(BaseField(gq1)));
		return NaturalDuality( gq1, EllipticQuadric(5,GF(q)), computeintertwiner );
    elif IsParabolicQuadric( gq1 ) then
		return NaturalDuality( gq1, SymplecticSpace(3, BaseField(gq1) ),  computeintertwiner );
	elif IsEllipticQuadric( gq1 ) then
		q := Size(BaseField(gq1));
		return NaturalDuality( gq1, HermitianPolarSpace(3, q^2), computeintertwiner);
	else
        Error("no duality possible on <gq1>");
    fi;
    end );

# Added may 2014 jdb.
#############################################################################
#O  NaturalDuality( <gq1> )
# returns NaturalDuality( <gq1>, true)
##
InstallMethod( NaturalDuality,
	"for a GQ and a GQ",
	[ IsClassicalGQ ],
	function(gq1)
		return NaturalDuality(gq1,true);
	end);

#############################################################################
#
# Q(2n,q) -> W(2n-1,q) (q even).
#
#############################################################################

# Added april 2014 jdb.
#############################################################################
#O  IsomorphismPolarSpacesProjectionFromNucleus( <quadric>, <w>, <bool> )
# returns the isomorphism between Q(2n,q) and W(2n-1,q). Both may be user
# defined.
# standard Q(2n,q): X0^2+X1X2+...X2n-1X2n: nucleus: (1,0,0,...,0,0)
# standard W(2n,q): X0Y1+X1Y0+...X2n-2X2n.
# projecting from nucleus yields the automorphism. Going back: consider a point
# of W(2n-1,q): evaluate it under X1X2+...X2n-1X2n: this yields a value z.
# the equation X0^2 = z has exactly one solution -> point of Q(2n,q).
# The interwiner: given an element of PGammaO(2n+1,q): it is sufficient to select
# a submatrix, sinc the projection is equivalent with removing X0
# given an element of PGammaSp: the points (1,0,...0), (0,1,...0), ... (0,0,...0)
# are, with an extra 0 in front, also points of Q(2n,q). Compute the elements of
# the symplectic points, compute the preimage of this image under the projection
# this yields the first column of the matrix of the corresponding element of 
# PGammaO. Since the nucleus is fixed under the elements of PGammaO, the first
# row (except the very first element), is zero. To compute the first element,
# we need the image of the n+2 nd point under the given element. So find a 
# symplectic point that together with the other symplectic points forms part of a 
# frame, make sure the preimage can be computed, compute the image under el, and
# compute its preimage. This gives the very first element of the PGammaO element.
# Note that for q=2 this element is necessarily one. Note that the general code 
# works anyway also for q=2 and n even, which explains the exception in the
# twinerprefun.
# Finally: base changes are included if Q and W are not canonical. This is straight-
# forward.
##
InstallMethod( IsomorphismPolarSpacesProjectionFromNucleus,
	"for two classical polar spaces",
	[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
	function(quadric, w, computeintertwiner)
	local q,f, func, pre, nucleus, n, mat, hyp, map, can_form, cquadric, cquadricinv,
	cw, cwinv, twinerfunc, twinerprefun, coll1, coll2, hom, ones, one, gens1, gens2;
	f := BaseField(quadric);
	q := Size(f);
    one := One(f);
	if not IsEvenInt(q) and f = BaseField(w) then
		Error(" Characteristic of basefields must be even and basfields must be equal");
	elif Rank(quadric) <> Rank(w) then
		Error(" Rank of polar spaces must be equal ");
	fi;
	n := quadric!.dimension + 1;
	mat := IdentityMat(n,f);
	if IsCanonicalPolarSpace(quadric) then
		can_form := QuadraticForm(quadric);
		cquadric := IdentityMat(n,f);
		cquadricinv := cquadric;
	else
		cquadric := BaseChangeToCanonical(QuadraticForm(quadric));
		can_form := QuadraticFormByMatrix( CanonicalQuadraticForm("parabolic", n, f), f);
		cquadricinv := cquadric^-1;
	fi;
	if IsCanonicalPolarSpace(w) then
		cw := IdentityMat(n-1,f);
		cwinv := cw;
	else
		cw := BaseChangeToCanonical(SesquilinearForm(w));
		cwinv := cw^-1;
	fi;
	nucleus := mat[1]; #vector representing the nucleus
	hyp := mat{[2..n]}; #hyperplane on which we will project.
	func := function( el )
		local vec, proj;
		if el!.type = 1 then
			vec := [Unpack(el!.obj) * cquadricinv, nucleus];
		else
			vec := Concatenation(Unpack(el!.obj) * cquadricinv, [nucleus]);
		fi;
		proj := SumIntersectionMat(vec, hyp)[2]; #hard coded Meet operation :-)
		vec := List(proj,x->x{[2..n]}); #make it a GF(q)^(n-1) vector ends the projection.
		return VectorSpaceToElement(w, vec * cw);
	end;
	pre := function(el)
		local vec, i;
		if el!.type = 1 then
			vec := [Unpack(el!.obj) * cwinv];
		else
			vec := Unpack(el!.obj) * cwinv;
		fi;
		vec := List(vec,x->Concatenation([0*Z(q)^0],x));
		for i in [1..Length(vec)] do
			vec[i][1] := (vec[i]^can_form)^(q/2);
		od;
		return VectorSpaceToElement(quadric, vec * cquadric);
	end;
	
    ones := List([1..n],x->one); #all ones for several purposes.

	twinerfunc := function( el )
		local mat,frob;
		frob := el!.frob;
		#mat := Unpack(el!.mat){[2..n]}{[2..n]};
		mat := cquadric * Unpack(el!.mat) * cquadricinv^(frob^-1);
		mat := mat{[2..n]}{[2..n]};
		return ProjElWithFrob(cwinv * mat * cw^(frob^-1),frob,f);
	end;
	
	twinerprefun := function( el )
		local newmat,mat,frob,i,vec,y,newvec,z,zprime;
		frob := el!.frob;
		mat := cw * Unpack(el!.mat) * cwinv^(frob^-1);
		#mat := Unpack(el!.mat);
		newmat := NullMat(n,n,f);
		newmat{[2..n]}{[2..n]} := mat;

        #compute the first column of newmat (except for newmat[1][1].
		for i in [2..n] do
			vec := Concatenation([0*Z(q)^0],mat[i-1])^frob;
			newmat[i,1] := ((vec^can_form)^(q/2))^(frob^-1);
		od;
		#vec := List(TransposedMat(mat),x->Sum(x)^frob); #is the image of (1,1,...,1) under el.
        
        if not (q=2 and IsEvenInt((n-1)/2)) then
            vec := ShallowCopy(ones);
            vec[1] := 0*one;
            vec[2] := Z(q);
            z := (vec^can_form)^(q/2);

            y := vec*newmat;
            y := y[1];
            newvec := (vec * newmat)^frob; #remind that the symplectic part is in [2..n].
            newvec[1] := 0*one;
            zprime := ((newvec^can_form)^(q/2))^(frob^-1);
        
        #image of (alpha,1,1,...,1) under el (symplectic point!)

            newmat[1,1] := (zprime+y)/z;
        else
            newmat[1,1] := one;
        fi;

		return ProjElWithFrob(cquadricinv * newmat * cquadric^(frob^-1), frob, f);
		#return ProjElWithFrob( newmat , frob, f);
	end;

    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(quadric), ElementsOfIncidenceStructure(w), func, pre);
    SetIsBijective( map, true );

    if computeintertwiner then
        if (HasIsCanonicalPolarSpace( quadric ) and IsCanonicalPolarSpace( quadric )) or
            HasCollineationGroup( quadric ) then
			coll1 := CollineationGroup(quadric);
			gens1 := GeneratorsOfGroup( coll1 );
			gens2 := List(gens1, twinerfunc);
			coll2 := GroupWithGenerators(gens2);
			hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun);
			SetIntertwiner( map, hom );
			UseIsomorphismRelation(coll1,coll2);
        elif (HasIsCanonicalPolarSpace( w ) and IsCanonicalPolarSpace( w )) or
			HasCollineationGroup( w ) then
			coll2 := CollineationGroup( w );
			gens2 := GeneratorsOfGroup( coll2 );
			gens1 := List(gens2, twinerprefun );
			coll1 := GroupWithGenerators(gens1);
			hom := GroupHomomorphismByFunction(coll1, coll2, twinerfunc, twinerprefun);
			SetIntertwiner( map, hom );
			UseIsomorphismRelation(coll1,coll2);
		else
			Info(InfoFinInG, 1, "No intertwiner computed. One of the polar spaces must have a collineation group computed");
		fi;
    fi;

    return map;
 end );

# Added july 2014 jdb.
#############################################################################
#O SelfDualitySymplectic
# combines NaturalDualitySymplectic with IsomorphismPolarSpacesProjectionFromNucleus,
# most of the code is recycled from these operations, and is documented there.
# Note that we have less base changes to deal with. Starting from a canonical W(3,q), 
# the obtained Q(4,q) has form X_0^2+X1X4+X2X3=0. The base change X4<->X2 is represented
# by perm5 at the Q(4,q) side and perm4 at the projected W(3,q) side.
# This operation is a helper operation for SelfDuality, there are no checks 
# on the input.
# order: W(3,q) -> Q(4,q) -> W(3,q), by Plucker and then projection.
# -> twinerfunc: 1. PsP(4,q) -> PGO(5,q) by Plucker (twinerfunc of NaturalDualitySymplectic)
#                2. PGO(5,q) -> PsP(4,q) by projection (twinerfunc of Isomorphism...Nucleus)
# -> twinerprefun: 1. PsP(4,q) -> PPGO(5,q) by inverse projection (twinerprefun of Isomorphism...Nucleus)
#                  2. PGO(5,q) -> PsP(4,q) by inverse Plucker (twinerprefun of NaturalDualitySymplectic)
#############################################################################
#
InstallMethod( SelfDualitySymplectic,
	"for a symplectic GQ and a boolean",
	[ IsClassicalGQ, IsBool ],
	function( w, computeintertwiner )
    local f, one, mat, quad_form, quadric, data, form_w, cw, cwinv, q, map,
        func, pre, formw, delta, hyp, coll1, hom, ones, can_form,
		twinerfunc, twinerprefun, cq4qinv, id, hyp2, nucleus, perm4, gram, perm5;
    f := w!.basefield;
    one := One(f);
	formw := SesquilinearForm( w );
    delta := PolarityOfProjectiveSpace(w);
    hyp := IdentityMat(6,f){[1..5]}; #this and next are in fact HyperplaneByDualCoorindates([1,0,0,0,0,1]..)
    hyp[1,6] := -one;
    nucleus := [1,0,0,0,0]*one;
	mat := IdentityMat(5,f);
    hyp2 := mat{[2..5]};
    perm4 := IdentityMat(4,f){[1,4,3,2]};
    perm5 := IdentityMat(5,f){[1,2,5,4,3]};
    gram := [[1,0,0,0,0],[0,0,0,0,1],[0,0,0,1,0],[0,0,0,0,0],[0,0,0,0,0]]*one;
    quad_form := QuadraticFormByMatrix(gram,f);
    q := Size(f);
    cw := IdentityMat(4,f);
    if not IsCanonicalPolarSpace( w ) then
        cw := BaseChangeToCanonical( formw );
    fi;
    cwinv := cw^-1;
    can_form := QuadraticFormByMatrix( CanonicalQuadraticForm("parabolic", 5, f), f);
    ones := List([1..5],x->one); #all ones for several purposes.
    id := IdentityMat(4, f);

    func := function( el )
        local list,plane,vec,proj;
        if el!.type = 2 then #dealing with a line
            vec := [ PluckerCoordinates(Unpack(el!.obj) * cwinv){[1..5]}, nucleus];
            proj := SumIntersectionMat(vec, hyp2)[2];
            vec := List(proj,x->x{[2..5]});
            return VectorSpaceToElement(w, ( vec * perm4 ) * cw);
        else
            plane := el^delta;
            vec := Unpack(plane!.obj) * cwinv;
            list := [ PluckerCoordinates(vec{[1,2]}), PluckerCoordinates(vec{[2,3]}), PluckerCoordinates(vec{[1,3]}) ];
            plane := SumIntersectionMat(list, hyp)[2]; #hard coded Meet operation :-)
            list := List(plane,x->x{[1..5]});
            vec := Concatenation(list, [nucleus]);
            proj := SumIntersectionMat(vec, hyp2)[2];
            vec := List(proj,x->x{[2..5]});
            return VectorSpaceToElement(w, ( vec * perm4 ) * cw);
        fi;
    end;

    pre := function( el )
        local vec,i;
        if el!.type = 1 then #dealing with a point
            vec := [Unpack(el!.obj) * cwinv] * perm4;
        else
            vec := (Unpack(el!.obj) * cwinv )* perm4;
        fi;
        vec := List(vec,x->Concatenation([0*Z(q)^0],x));
        for i in [1..Length(vec)] do
            vec[i][1] := (vec[i]^quad_form)^(q/2);
        od;
        if el!.type = 1 then #dealing with a point
            vec[1][6] := -vec[1][1];
            return VectorSpaceToElement(w, InversePluckerCoordinates(vec[1]) * cw );
        else
            vec[1][6] := -vec[1][1];
            vec[2][6] := -vec[2][1];
            return VectorSpaceToElement(w, (SumIntersectionMat(InversePluckerCoordinates(vec[1]), InversePluckerCoordinates(vec[2]))[2]) * cw );
        fi;
    end;
    
    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(w), ElementsOfIncidenceStructure(w), func, pre);
    SetIsBijective( map, true );
    
    twinerfunc := function(g)
        local mat,newmat,frob;
        frob := g!.frob;
        mat := cw * Unpack(g!.mat) * cwinv^(frob^-1);
        newmat := [];
        newmat[2] := PluckerCoordinates([mat[3],mat[1]]){[1..5]};
        newmat[3] := PluckerCoordinates([mat[4],mat[1]]){[1..5]};
        newmat[4] := -PluckerCoordinates([-mat[3],mat[2]]){[1..5]};
        newmat[5] := PluckerCoordinates([mat[4],-mat[2]]){[1..5]};
        newmat[1] := PluckerCoordinates([mat[2]+mat[3],mat[1]+mat[4]]){[1..5]} - newmat[2] - newmat[5];
        newmat := perm5 * newmat * perm5^(frob^-1);
        newmat := newmat{[2..5]}{[2..5]};
        return ProjElWithFrob(cwinv * newmat * cw^(frob^-1),frob,f);
    end;

	twinerprefun := function( el )
		local newmat,mat,frob,i,vec,y,newvec,z,zprime,
            lines, pts, ipts, ilines, e, x, ept, ielines, 
            ie, cs, iept, j;
		frob := el!.frob;
		mat := cw * Unpack(el!.mat) * cwinv^(frob^-1);
		newmat := NullMat(5,5,f);
		newmat{[2..5]}{[2..5]} := mat;

        #compute the first column of newmat (except for newmat[1,1].
		for i in [2..5] do
			vec := Concatenation([0*Z(q)^0],mat[i-1])^frob;
			newmat[i,1] := ((vec^can_form)^(q/2))^(frob^-1);
		od;

        if not q = 2 then
            vec := ShallowCopy(ones);
            vec[1] := 0*one;
            vec[2] := Z(q);
            z := (vec^can_form)^(q/2);

            y := vec*newmat;
            y := y[1];
            newvec := (vec * newmat)^frob; #remind that the symplectic part is in [2..n].
            newvec[1] := 0*one;
            zprime := ((newvec^can_form)^(q/2))^(frob^-1);
        
        #image of (alpha,1,1,...,1) under el (symplectic point!)

            newmat[1,1] := (zprime+y)/z;
        else
            newmat[1,1] := one;
        fi;
		
        mat := perm5 * newmat * perm5;
 
        lines:= [];
        lines[1] := [[id[1],id[3]],[id[1],id[4]]];
        lines[2] := [[id[2],id[3]],[id[2],id[4]]];
        lines[3] := [[id[3],id[1]],[id[3],id[2]]];
        lines[4] := [[id[4],id[1]],[id[4],id[2]]];
        pts := List(lines,x->List(x,y->PluckerCoordinates(y){[1..5]}));
        ipts := List(pts,x->x*mat);
        for i in [1..Length(ipts)] do
            for j in [1..2] do
                ipts[i][j][6] := -ipts[i][j][1];
            od;
        od;
        ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
        ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
        newmat := List(ipts,x->x[1]);
        e := [1,1,1,1]*one;
        ept := List([[e,id[1]+id[4]],[e,id[1]+id[2]]],y->PluckerCoordinates(y){[1..5]});
        iept := List(ept,x->x*mat);
        iept[1][6] := -iept[1][1];
        iept[2][6] := -iept[2][1];
	
        ielines := List(iept,x->InversePluckerCoordinates(x));
        ie := SumIntersectionMat(ielines[1],ielines[2])[2];
        cs := SolutionMat(newmat,ie[1]);
        for i in [1..4] do
            newmat[i] := newmat[i]*cs[i];
        od;
        return ProjElWithFrob(cwinv * newmat * cw^(frob^-1),frob,f);

	end;

    if computeintertwiner then
        if (HasIsCanonicalPolarSpace( w ) and IsCanonicalPolarSpace( w )) or
            HasCollineationGroup( w ) then
			coll1 := CollineationGroup(w);
			hom := GroupHomomorphismByFunction(coll1, coll1, twinerfunc, twinerprefun);
			SetIntertwiner( map, hom );
			#UseIsomorphismRelation(coll1,coll2);
 		else
			Info(InfoFinInG, 1, "No intertwiner computed. The polar space must have a collineation group computed");
		fi;
    fi;

    return map;
end );

# Added july 2014 jdb.
#############################################################################
#O SelfDualityParabolic
# combines NaturalDualitySymplectic with IsomorphismPolarSpacesProjectionFromNucleus,
# most of the code is recycled from these operations, and is documented there.
# Note that we have less base changes to deal with. Starting from a canonical W(3,q), 
# the obtained Q(4,q) has form X_0^2+X1X4+X2X3=0. The base change X4<->X2 is represented
# by perm5 at the Q(4,q) side and perm4 at the projected W(3,q) side.
# This operation is a helper operation for SelfDuality, there are no checks 
# on the input.
# order: Q(4,q) -> W(3,q) -> Q(4,q), by projection and then Plucker.
# -> twinerfunc: 1. PGO(5,q) -> PsP(4,q) by projection (twinerfunc of Isomorphism...Nucleus)
#                2. PsP(4,q) -> PGO(5,q) by Plucker (twinerfunc of NaturalDualitySymplectic)
# -> twinerprefun: 1. PGO(5,q) -> PsP(4,q) by inverse Plucker (twinerprefun of NaturalDualitySymplectic)
#                  2. PsP(4,q) -> PPGO(5,q) by inverse projection (twinerprefun of Isomorphism...Nucleus)
#############################################################################
#
InstallMethod( SelfDualityParabolic,
	"for a symplectic GQ and a boolean",
	[ IsClassicalGQ, IsBool ],
	function( q4q, computeintertwiner )
    local f, one, mat, quad_form, quadric, data, cq, cqinv, q, map, can_form,
        func, pre, formq, delta, hyp, coll1, coll2, gens1, gens2, hom, ones,
		twinerfunc, twinerprefun, id, hyp2, nucleus, perm4, perm5, gram;

    f := q4q!.basefield;
    one := One(f);
    hyp := IdentityMat(6,f){[1..5]}; #this and next are in fact HyperplaneByDualCoorindates([1,0,0,0,0,1]..)
    hyp[1][6] := -one;
    mat := IdentityMat(5,f);
    hyp2 := mat{[2..5]};
    perm4 := IdentityMat(4,f){[1,4,3,2]};
    perm5 := IdentityMat(5,f){[1,2,5,4,3]};
    quad_form := QuadraticFormByMatrix(CanonicalQuadraticForm("parabolic",5,f),f);
    q := Size(f);
    nucleus := [1,0,0,0,0]*one;
    delta := CanonicalGramMatrix("symplectic",4,f); #we will hard code the polarity :-)
    formq := QuadraticForm(q4q);
    
    cq := IdentityMat(5,f);
    if not IsCanonicalPolarSpace( q4q ) then
        cq := BaseChangeToCanonical( formq );
    fi;
    cqinv := cq^-1;

    func := function( el )
        local list,plane,vec,proj;
        if el!.type = 1 then
            vec := [Unpack(el!.obj) * cqinv, nucleus];
        else
            vec := Concatenation(Unpack(el!.obj) * cqinv, [nucleus]);
        fi;
        proj := SumIntersectionMat(vec, hyp2)[2]; #hard coded Meet operation :-)
        vec := List(proj,x->x{[2..5]}); #make it a GF(q)^(n-1) vector ends the projection.
        if el!.type = 2 then
            vec := [PluckerCoordinates(vec){[1..5]}] * perm5;
            return VectorSpaceToElement(q4q,vec * cq);
        else
            plane := NullspaceMat(TransposedMat(vec*delta));
            list := [ PluckerCoordinates(plane{[1,2]}), PluckerCoordinates(plane{[2,3]}), PluckerCoordinates(plane{[1,3]}) ];
            plane := SumIntersectionMat(list, hyp)[2]; #hard coded Meet operation :-)
            list := List(plane,x->x{[1..5]}) * perm5;
            return VectorSpaceToElement(q4q,list * cq);
        fi;
    end;

    pre := function( el )
        local vec,ivec,i;
        if el!.type = 1 then #dealing with a point
            vec := [Unpack(el!.obj) * cqinv ] * perm5;
            vec[1][6] := -vec[1][1];
            ivec := InversePluckerCoordinates(vec[1]);
        else
            vec := (Unpack(el!.obj) * cqinv )* perm5;
            vec[1][6] := -vec[1][1];
            vec[2][6] := -vec[2][1];
            ivec := SumIntersectionMat(InversePluckerCoordinates(vec[1]), InversePluckerCoordinates(vec[2]))[2];
        fi;
        ivec := List(ivec,x->Concatenation([0*Z(q)^0],x));
        for i in [1..Length(ivec)] do
            ivec[i][1] := (ivec[i]^quad_form)^(q/2);
        od;
        return VectorSpaceToElement(q4q,ivec * cq);
    end;

    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(q4q), ElementsOfIncidenceStructure(q4q), func, pre);
    SetIsBijective( map, true );

    can_form := QuadraticFormByMatrix( CanonicalQuadraticForm("parabolic", 5, f), f);
    ones := List([1..5],x->one); #all ones for several purposes.
    id := IdentityMat(4, f);

	twinerfunc := function( el )
		local mat,frob, newmat;
		frob := el!.frob;
		mat := cq * Unpack(el!.mat) * cqinv^(frob^-1);
		mat := mat{[2..5]}{[2..5]};
        newmat := [];
        newmat[2] := PluckerCoordinates([mat[3],mat[1]]){[1..5]};
        newmat[3] := PluckerCoordinates([mat[4],mat[1]]){[1..5]};
        newmat[4] := -PluckerCoordinates([-mat[3],mat[2]]){[1..5]};
        newmat[5] := PluckerCoordinates([mat[4],-mat[2]]){[1..5]};
        newmat[1] := PluckerCoordinates([mat[2]+mat[3],mat[1]+mat[4]]){[1..5]} - newmat[2] - newmat[5];
        newmat := perm5 * newmat * perm5;
        return ProjElWithFrob(cqinv * newmat * cq^(frob^-1),frob,f);
	end;
    
	twinerprefun := function( el )
		local newmat,mat,frob,i,vec,y,newvec,z,zprime,
            lines, pts, ipts, ilines, e, x, ept, ielines, 
            ie, cs, iept, j;
		frob := el!.frob;
		mat := perm5 * cq * Unpack(el!.mat) * cqinv^(frob^-1) * perm5;

        lines:= [];
        lines[1] := [[id[1],id[3]],[id[1],id[4]]];
        lines[2] := [[id[2],id[3]],[id[2],id[4]]];
        lines[3] := [[id[3],id[1]],[id[3],id[2]]];
        lines[4] := [[id[4],id[1]],[id[4],id[2]]];
        pts := List(lines,x->List(x,y->PluckerCoordinates(y){[1..5]}));
        ipts := List(pts,x->x*mat);
        for i in [1..Length(ipts)] do
            for j in [1..2] do
                ipts[i][j][6] := -ipts[i][j][1];
            od;
        od;
        ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
        ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
        newmat := List(ipts,x->x[1]);
        e := [1,1,1,1]*one;
        ept := List([[e,id[1]+id[4]],[e,id[1]+id[2]]],y->PluckerCoordinates(y){[1..5]});
        iept := List(ept,x->x*mat);
        iept[1][6] := -iept[1][1];
        iept[2][6] := -iept[2][1];
	
        ielines := List(iept,x->InversePluckerCoordinates(x));
        ie := SumIntersectionMat(ielines[1],ielines[2])[2];
        cs := SolutionMat(newmat,ie[1]);
        for i in [1..4] do
            newmat[i] := newmat[i]*cs[i];
        od;
        mat := ShallowCopy(newmat);

		newmat := NullMat(5,5,f);
		newmat{[2..5]}{[2..5]} := mat;

        #compute the first column of newmat (except for newmat[1,1].
		for i in [2..5] do
			vec := Concatenation([0*Z(q)^0],mat[i-1])^frob;
			newmat[i,1] := ((vec^can_form)^(q/2))^(frob^-1);
		od;

        if not q = 2 then
            vec := ShallowCopy(ones);
            vec[1] := 0*one;
            vec[2] := Z(q);
            z := (vec^can_form)^(q/2);

            y := vec*newmat;
            y := y[1];
            newvec := (vec * newmat)^frob; #remind that the symplectic part is in [2..n].
            newvec[1] := 0*one;
            zprime := ((newvec^can_form)^(q/2))^(frob^-1);
        
        #image of (alpha,1,1,...,1) under el (symplectic point!)

            newmat[1,1] := (zprime+y)/z;
        else
            newmat[1,1] := one;
        fi;
		
        return ProjElWithFrob(cqinv * newmat * cq^(frob^-1),frob,f);

	end;

    if computeintertwiner then
        if (HasIsCanonicalPolarSpace( q4q ) and IsCanonicalPolarSpace( q4q )) or
            HasCollineationGroup( q4q ) then
			coll1 := CollineationGroup(q4q);
			hom := GroupHomomorphismByFunction(coll1, coll1, twinerfunc, twinerprefun);
			SetIntertwiner( map, hom );
			#UseIsomorphismRelation(coll1,coll2);
 		else
			Info(InfoFinInG, 1, "No intertwiner computed. The polar space must have a collineation group computed");
		fi;
    fi;

    return map;
end );

# Added july 2014 jdb.
#############################################################################
#O  SelfDuality( <gq>, <bool> )
# This is the interface to the helper functions. It simply checks the input 
# and decides which NaturalDuality... to use.
##
InstallMethod( SelfDuality,
	"for a GQ and a boolean",
	[ IsClassicalGQ, IsBool ],
	function( gq, computeintertwiner )
    local f;
    f := gq!.basefield;
    if not IsEvenInt(Characteristic(f)) then
        Error( "<gq> must be a GQ over a field of even characteristic");
    fi;
    if IsSymplecticSpace(gq) then
        return SelfDualitySymplectic(gq, computeintertwiner );
    elif IsParabolicQuadric( gq ) then
        return SelfDualityParabolic(gq, computeintertwiner );
    else
        Error("<gq> must be a symplectic GQ or a parabolic quadric");
    fi;
end );

# Added july 2014 jdb.
#############################################################################
#O  SelfDuality( <gq>, <bool> )
# This is the interface to the helper functions. It returns SelfDuality(<gq>,true)
##
InstallMethod( SelfDuality,
	"for a GQ and a boolean",
	[ IsClassicalGQ ],
	function( gq )
        return SelfDuality(gq, true);
end );


