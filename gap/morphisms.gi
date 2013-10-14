#############################################################################
##
##  morphisms.gi              FinInG package
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
##  Copyright 2011	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for incidence geometry morphisms
##
#############################################################################

########################################
#
# Things To Do:
#
# - Preimages for GrassmannMap, VeroneseMap, and SegreMap.HermitianPolarSpace
# - intertwiners for GrassmannMap and SegreMap
# - should there be a type function as an attribute?
# - maybe make a more userfriendly system to avoid em!.prefun( <arg> )
# - in the same sense: do we want a NaturalDuality starting from Q(4,q) and Q-(5,q)? 
#   And the self duality of Q(4,q) and W(3,q), q even.

# - test operations BlownUpSubspaceOfProjectiveSpace, BlownUpSubspaceOfProjectiveSpaceBySubfield, 

# - an optimalization is possible: each time VectorSpaceToElement is used, this might be replaced
#   by Wrap. VectorSpaceToElement is of course useful for testing purposes. Currently, we left it on
#   to find bugs quicker. But it looks already quite well.
#
# Documentation check list
# - IsGeometryMorphism: done
# - Intertwiner: done
# - NaturalEmbeddingBySubspace(NC): done
# - NaturalProjectionBySubspace(NC): done
# - NaturalEmbeddingByFieldReduction: done
# - NaturalEmbeddingBySubfield: done
# - IsomorphismPolarSpaces(NC): done
# - VeroneseMap: move this to varieties: done
# - SegreMap: move this to varieties: done
# - PluckerCoordinates: not documented, but there is a good reason for, see the comments at the code
# - InversePluckerCoordinates: not documented, see one line above this.
# - KleinCorrespondence: done
# - GrassmannMap: move this to varieties: done
# - NaturalDuality: done
# - ProjectiveCompletion: done
#
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
		basis := v!.obj;
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
		ConvertToMatrixRep(invbasis, f);
		func := x -> VectorSpaceToElement(geom2 , x!.obj * basis);
		pre := function(y)
			local newy;
			if not y in v then
				Error("Applying preimage to an element which is not in the range");
			fi;
			newy:= y!.obj * invbasis;
			if not IsMatrix(newy) then 
				newy := newy{[1..d1]}; 
                ConvertToVectorRepNC(newy, f);
			else
				newy := newy{[1..Size(newy)]}{[1..d1]};
				ConvertToMatrixRepNC(newy, f);
			fi;
			return VectorSpaceToElement(geom1, newy);
		end;   
		morphism := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                           ElementsOfIncidenceStructure(geom2), 
                                           func, false, pre );
		SetIsInjective( morphism, true );  
		return morphism;
	end );

# CHECKED 27/09/11 jdb + ml
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
		basis := v!.obj;
		d1 := geom1!.dimension + 1;
		f := geom2!.basefield;
		bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
		invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
		ConvertToMatrixRep(invbasis, f);
		func := x -> VectorSpaceToElement(geom2 , x!.obj * basis);
		pre := function(y)
			local newy;
			newy:= y!.obj * invbasis;
			if not IsMatrix(newy) then 
				newy := newy{[1..d1]}; 
				ConvertToVectorRepNC(newy, f);
			else
				newy := newy{[1..Size(newy)]}{[1..d1]};
				ConvertToMatrixRepNC(newy, f);
			fi;
			return VectorSpaceToElement(geom1, newy);
		end;   
		morphism := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                           ElementsOfIncidenceStructure(geom2), 
                                           func, false, pre );
		SetIsInjective( morphism, true );  
		return morphism;
	end );

# CHECKED 27/09/11 jdb + ml
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
			#return; #why was this return? 
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
			#return;
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

			basis := v!.obj;

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
			ConvertToMatrixRep(change, f);
			invchange := change^-1;
			bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
			invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
			ConvertToMatrixRep(invbasis, f);
			func := x -> VectorSpaceToElement( geom2, x!.obj * change * basis);
			pre := function(y)
				local newy;
				if not y in v then
					Error("Applying preimage to an element which is not in the range");
				fi;
				newy:= y!.obj * invbasis;
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
#############################################################################
#O  NaturalEmbeddingBySubspace( <ps1>, <ps2>, <v> ) 
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

		basis := v!.obj;

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
		ConvertToMatrixRepNC(change, f);
		invchange := change^-1;
		bs := BaseSteinitzVectors(Basis(geom2!.vectorspace), basis);
		invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
		ConvertToMatrixRepNC(invbasis, f);

		func := x -> VectorSpaceToElement( geom2, x!.obj * change * basis);
		pre := function(y)
			local newy;
			newy:= y!.obj * invbasis;
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
          coll1, coll2, gens1, gens2, x, y, func, inv, twinerfunc, twinerprefun;
		ty1 := PolarSpaceType( ps1 );
		ty2 := PolarSpaceType( ps2 );
		if ty1 <> ty2 then 
			Error("Polar spaces are of different type");
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
		change := c1^-1 * c2;       
		ConvertToMatrixRep(change, f);
		invchange := change^-1;     
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
                return ProjElWithFrob( change * y * invchange^x!.frob, x!.frob, f );  
			end;  
       
    # map from gens1 to gens2
		twinerfunc := function( y )
			local x;
				x := MutableCopyMat( y!.mat );
                return ProjElWithFrob( invchange * x * change^y!.frob, y!.frob, f ); 
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
		change := c1^-1 * c2;       
		ConvertToMatrixRepNC(change, f);
		invchange := change^-1;     
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
            return ProjElWithFrob( change * y * invchange^x!.frob, x!.frob, f );  
		end;  
       
    # map from gens1 to gens2
		twinerfunc := function( y )
			local x;
            x := MutableCopyMat( y!.mat );
            return ProjElWithFrob( invchange * x * change^y!.frob, y!.frob, f ); 
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
	m:=DimensionsMat(mat)[1];
	n:=DimensionsMat(mat)[2];
  # First we check if m and n are multiples of d
	if not (IsInt(m/d) and IsInt(n/d)) then 
		Error("The matrix does not have the right dimensions");
	fi;
	blocks:=List(Cartesian([0..m/d-1],[0..n/d-1]),ij->mat{[ij[1]*d+1 .. ij[1]*d+d]}{[ij[2]*d+1 ..ij[2]*d+d]});
	newmat:=[];
	for i in [1..m/d] do
		row:=[]; 
		for j in [1..n/d] do
			submat:=blocks[(i-1)*d+j];
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
		
		
## THE OLD ShrinkMat function:		
# InstallGlobalFunction( ShrinkMat, "preimage of BlownUpMat",
#  function( B, mat )
#    local result, vectors, row, i, j, resrow, b;
#    vectors := BasisVectors( B );
#    result := [];
#    b := Size(B);
#    for i in [1..Size(mat)/b] do
#        resrow := [];
#        for j in [1..Size(mat[1])/b] do
#            row :=  mat[(i-1) * b + 1]{[(j-1) * b + 1 .. j * b]};
#            Add(resrow, row * vectors);
#        od;
#        Add(result, resrow);
#    od;
#    ConvertToMatrixRepNC( result );
#    return result;
#  end );

InstallMethod( ShrinkMat, 
	"for a field, a subfield and a vector",
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
#O  BlownUpSubspaceOfProjectiveSpace( <B>, <mat> ) 
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
#O  BlownUpSubspaceOfProjectiveSpaceBySubfield( <B>, <mat> ) 
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
#O  IsDesarguesianSpreadElement( <B>, <mat> ) 
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

#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <field>, <basis> ) 
# <geom2> is a projective space over a field K, <geom1> is a projective space
# over a field extension L, and considering L as a vector space over K, yields 
# that <geom1> and <geom2> have the same ambient vectorspace over K, then this
# operation returns the natural embedding, i.e. with relation to the given basis
# of L over K.
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
		#f2 := pg2!.basefield;
		d1 := pg1!.dimension + 1;
		d2 := d1*t;
		pg2 := ProjectiveSpace(d2-1,q2);
		#d2 := pg2!.dimension + 1;
		#if not (IsInt(d2/d1)) then 
		#	Error("The second geometry is not obtained from the first geometry by field reduction");
		#fi;
		if not (IsBasis(basis) and f1=GF((basis!.q)^basis!.d) and f2=GF(basis!.q) and d1*(basis!.d)=d2) then
			Error("The basis is not a basis or is not compatible with the basefields of the geometries");
		fi;
		#t:=d2/d1;
		fun := function( x ); # This map blows up a subspace of geom1 to a subspace of geom2
			return BlownUpSubspaceOfProjectiveSpace(basis,x);
		end; 
		prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
			local flag,basvecs,mat1,span,x,v,v1,i;
			flag:=true;
			if not subspace in pg2 then 
				Error("The input is not in the range fo the field reduction map!");
			fi;
			if not IsInt((Dimension(subspace)+1)/t) then 
				flag:=false;
			else
				basvecs:=BasisVectors(basis);
				mat1:=[];
				span:=[];
				repeat
					repeat 
						x:=Random(Points(subspace)); 
					until not x in span;
					v:=Coordinates(x);
					v1:=List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
					Add(mat1,v1);
					span:=VectorSpaceToElement(pg2,BlownUpMat(basis,mat1));
				until Dimension(span)=Dimension(subspace);
				if not span = subspace then 
					flag:= false;
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
		hom := function( m )
			local image;      
			image := BlownUpMat(basis, m!.mat); 
			ConvertToMatrixRepNC( image, f2 );       
			return CollineationOfProjectiveSpace(image, f2);
		end;

		hominv := function( m )
			local preimage;      
			preimage := ShrinkMat(basis, m!.mat); 
			ConvertToMatrixRepNC( preimage, f1 );       
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

####
#
# First some operations that we will need
#
#####

#############################################################################
#O  BilinearFormFieldReduction( <bil1>, <f2>, <alpha>, <basis> ) 
# <bil1> is a bilinear form over <f1>, <f2> is a subfield of <f1>. 
# <alpha> determines the linear map from f1 to <f2>, f1 the field extension of <f2>
# with basis <basis>. This operation then
# returns the bilinear form L(bil1(.,.)), L(x) = T(alpha*x), T the trace from <f1> to <f2>.
##
InstallMethod( BilinearFormFieldReduction,
	"for a bilinear form and a field",
	[ IsBilinearForm, IsField, IsFFE, IsBasis ],
	function(bil1,f2,alpha,basis)
	# f2 is a subfield of the basefield of the bilinear form bil1
	local mat1,f1,d1,t,d2,V2,V1,phi,b2,b2vecs,mat2,i,row,j,bil2;
	mat1:=bil1!.matrix;
	f1:=bil1!.basefield;
	d1:=Size(mat1);
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
#O  BilinearFormFieldReduction( <bil1>, <f2>, <alpha>, <basis> ) 
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
# returns the bilinear form T(qf1(.,.)), T the trace from <f1> to <f2>.
##
InstallMethod( QuadraticFormFieldReduction,
	"for a quadratic form and a field",
	[ IsQuadraticForm, IsField, IsFFE, IsBasis ],
	function(qf1,f2,alpha,basis)
	# f2 is a subfield of the basefield for the quadratic form q1
	local f1,basvecs,d1,t,d2,V2,V1,phi,b2,b2vecs,mat2,i,bil1,j,qf2;
	f1:=qf1!.basefield;
	basvecs:=BasisVectors(basis);
	d1:=Size(qf1!.matrix);
	t:=Dimension(AsVectorSpace(f2,f1));
	d2:=d1*t;
	V2:=f2^d2;
	V1:=f1^d1;
	b2:=Basis(V2);
	b2vecs:=BasisVectors(b2);
	mat2:=IdentityMat(d2,f2);
	for i in [1..d2] do
		mat2[i][i]:=Trace(f1,f2,alpha*((ShrinkVec(f1,f2,b2vecs[i]))^qf1));
	od;
	for i in [1..d2-1] do
		for j in [i+1..d2] do
			mat2[i][j]:=Trace(f1,f2,alpha*((ShrinkVec(f1,f2,b2vecs[i]+b2vecs[j]))^qf1
							-(ShrinkVec(f1,f2,b2vecs[i]))^qf1-(ShrinkVec(f1,f2,b2vecs[j]))^qf1));
			#mat2[j][i]:=mat2[i][j]; THESE entries need to be zero
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


# CHANGED jdb 9/4/12: added arguments basis and alpha.
# one more remark. Some values of alpha will cause HermitianFormByMatrix (so odd t) 
# to produce an error message. This is normal, see overview in N. Gill's paper
# but as we don't check this in the inputs, this function is not meant for the
# user.
#############################################################################
#O  HermitianFormFieldReduction( <qf1>, <f2> ) 
# <bil1> is a hermitian form over <f1>, <f2> is a subfield of <f1>. This operation
# returns the form T(qf1(.,.)). Depending on the degree of the field extension, this
# yields either a bilinear form or a hermitian form.
##
InstallMethod( HermitianFormFieldReduction,
	"for a hermitian form and a field",
	[ IsHermitianForm, IsField, IsFFE, IsBasis ],
	function(hf1,f2,alpha,basis)
	# f2 is a subfield of the basefield for the hermitian form hf1
	# here the basefield is always a square
	local f1,basvecs,d1,t,d2,V2,V1,phi,b2,b2vecs,mat2,i,row,j,hf2;
	f1:=hf1!.basefield;
	#basis:=Basis(AsVectorSpace(f2,f1));
	basvecs:=BasisVectors(basis);
	d1:=Size(hf1!.matrix);
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
			Add(row,Trace(f1,f2,alpha*([ShrinkVec(f1,f2,b2vecs[i]),ShrinkVec(f1,f2,b2vecs[j])]^hf1)));
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

##########################################################
#
# The embeddings for polar spaces
# 
#############################################################################

# master version for the user: handle all parameters.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <ps1>, <f2>, <alpha>, <basis>, <bool> ) 
# returns a geometry morphism, described below. If <bool> is true, then an intertwiner
# is computed.
#
#  A good reference on field reduction of polar spaces is
#  "Polar spaces and embeddings of classical groups" by Nick Gill
#  (New Zealand J. Math). 
##
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space, a field, a basis, a finite field element, and a boolean",
	[IsClassicalPolarSpace, IsField, IsFFE, IsBasis, IsBool],
	function(ps1,f2,alpha,basis,computeintertwiner)
	# f2 must be a subfield of the basefield of the classical polar space
	local type1,qf1,qf2,ps2,bil1,bil2,hf1,hf2,em,fun,prefun,f1,
					map,hom,hominv,g1,gens,newgens,g2,twiner;
	type1 := PolarSpaceType(ps1);
	f1:=ps1!.basefield;
	
	# 1. the polar space is of orthogonal type
	if type1 in ["hyperbolic","elliptic","parabolic"] then
		qf1:=QuadraticForm(ps1);
#		qf2:=QuadraticFormFieldReduction(qf1,f2,alpha,basis);
		qf2:=QuadraticFormFieldReduction(qf1,f2,alpha);  # John's method (without "basis")
		
		if IsSingularForm(qf2) then 
			Error("The field reduction does not yield a natural embedding");
		else ps2:=PolarSpace(qf2);
		fi;
	# 2. the polar space is of symplectic type
	elif type1 in ["symplectic"] then
		bil1:=SesquilinearForm(ps1);
		bil2:=BilinearFormFieldReduction(bil1,f2,alpha,basis);
		ps2:=PolarSpace(bil2);
	# 1. the polar space is of hermitian type	
	elif type1 in ["hermitian"] then
		hf1:=SesquilinearForm(ps1);
		hf2:=HermitianFormFieldReduction(hf1,f2,alpha,basis);
		ps2:=PolarSpace(hf2);
	fi;
	
	em:=NaturalEmbeddingByFieldReduction(AmbientSpace(ps1),AmbientSpace(ps2));
	
	fun:=function(x)
		local projfun;
		projfun:=em!.fun;
		return VectorSpaceToElement(ps2,projfun(x)!.obj);
	end;
		
	prefun:=function(x)
		local projprefun;
		projprefun:=em!.prefun;
		return VectorSpaceToElement(ps1,projprefun(x)!.obj);
	end;
	
	map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1),
                                         ElementsOfIncidenceStructure(ps2),
                                         fun, false, prefun);
		
	SetIsInjective( map, true );
	
	## Now creating intertwiner
	
	if computeintertwiner then
		hom := function( m )
			local image;      
			image := BlownUpMat(basis, m!.mat); 
			ConvertToMatrixRepNC( image, f2 );       
			return CollineationOfProjectiveSpace(image, f2);
		end;
		hominv := function( m )
			local preimage;      
			preimage := ShrinkMat(basis, m!.mat); 
			ConvertToMatrixRepNC( preimage, f1 );       
			return CollineationOfProjectiveSpace(preimage, f1);
		end;
		g1 := SimilarityGroup( ps1 );
		gens := GeneratorsOfGroup( g1 );
		newgens := List(gens, hom);
		g2 := Group( newgens );
		SetSize(g2, Size(g1));
		twiner := GroupHomomorphismByFunction(g1, g2, hom, hominv);
		SetIntertwiner( map, twiner);
	fi;
	return map;
	end );

# first particular version: user agrees with everything
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
	
# second particular version: user agrees but wants to control intertwiner
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <bool> ) 
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <one>, <basis>, <bool> )
# where <basis> is the canonical basis of BaseField(geom1) over <f2>, and
# <one> is One(BaseField(geom1)). This might be usefull if you agree with the defaults, but doesn't 
# want the intertwiner.
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space and a field",
	[IsClassicalPolarSpace, IsField, IsBool],
	function(ps1,f2,bool)
		return NaturalEmbeddingByFieldReduction(ps1,f2,One(f2),Basis(AsVectorSpace(f2,BaseField(ps1))),bool);
	end );

#third particular version: user wants a particular alpha, agrees with base and bool.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <bool> ) 
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis>, true )
# where <basis> is the canonical basis of BaseField(geom1) over <f2>.
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space and a field",
	[IsClassicalPolarSpace, IsField, IsFFE],
	function(ps1,f2,alpha)
		return NaturalEmbeddingByFieldReduction(ps1,f2,alpha,Basis(AsVectorSpace(f2,BaseField(ps1))),true);
	end );

#fourth version: user wants a particular alpha, and basis, agrees with bool.
#############################################################################
#O  NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <bool> ) 
# returns NaturalEmbeddingByFieldReduction( <geom1>, <f2>, <alpha>, <basis>, true )
#
InstallMethod (NaturalEmbeddingByFieldReduction,
	"for a polar space and a field",
	[IsClassicalPolarSpace, IsField, IsFFE, IsBasis],
	function(ps1,f2,alpha,basis)
		return NaturalEmbeddingByFieldReduction(ps1,f2,alpha,basis,true);
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

#the method below is UNFINISHED. I just put the framework here (jdb 21/12/2011).
#############################################################################
#O  CanonicalEmbeddingByFieldReduction( <ps1>, <f2>, <bool> ) 
# returns a geometry morphism, described below. If <bool> is true, then an intertwiner
# is computed. We embed into a canonical polar space. The function is 
# strongly based on its non-canonical variant.
##
InstallMethod (CanonicalEmbeddingByFieldReduction,
	"for a polar space, a field, and a boolean",
	[IsClassicalPolarSpace, IsField, IsBool],
	function(ps1,f2,computeintertwiner)
	# f2 must be a subfield of the basefield of the classical polar space
	local type1,qf1,qf2,ps2,bil1,bil2,hf1,hf2,em,fun,prefun,basis,f1,d,iso,form2,type2,
					map,hom,hominv,g1,gens,newgens,g2,twiner,canonicalform,canonical,
					c1,c2,change,invchange;
	type1 := PolarSpaceType(ps1);

	# 1. the polar space is of orthogonal type
	if type1 in ["hyperbolic","elliptic","parabolic"] then
		qf1:=QuadraticForm(ps1);
		form2:=QuadraticFormFieldReduction(qf1,f2);   
		if IsSingularForm(qf2) then 
			Error("The field reduction does not yield a natural embedding");
		else ps2:=PolarSpace(qf2);
		fi;
	# 2. the polar space is of symplectic type
	elif type1 in ["symplectic"] then
		bil1:=SesquilinearForm(ps1);
		form2:=BilinearFormFieldReduction(bil1,f2);
		ps2:=PolarSpace(form2);
	# 1. the polar space is of hermitian type	
	elif type1 in ["hermitian"] then
		hf1:=SesquilinearForm(ps1);
		form2:=HermitianFormFieldReduction(hf1,f2); #could be another form than a hermitian form
		ps2:=PolarSpace(form2);
	fi;
	type2 := PolarSpaceType(ps2);
	d := ProjectiveDimension(ps2);
	if type2 = "hermitian" then
		canonical := HermitianPolarSpace(d, f2);  
		canonicalform := SesquilinearForm(canonical);             
	elif type2 = "symplectic" then
		canonical := SymplecticSpace(d, f2);  
		canonicalform := SesquilinearForm(canonical);              
	elif type2 = "elliptic" then 
		canonical := EllipticQuadric(d, f2); 
		canonicalform := QuadraticForm(canonical);        
	elif type2 = "parabolic" then
		canonical := ParabolicQuadric(d, f2);
		canonicalform := QuadraticForm(canonical);        
	elif type2 = "hyperbolic" then
		canonical := HyperbolicQuadric(d, f2);
		canonicalform := QuadraticForm(canonical);        
	fi;
	c1 := BaseChangeToCanonical( form2 );
	c2 := BaseChangeToCanonical( canonicalform );
	change := c1^-1 * c2;       
	ConvertToMatrixRepNC(change, f2);
	invchange := change^-1;     
		
	#iso := IsomorphismPolarSpacesNC(ps2, canonical, false);
	
	#em:=NaturalEmbeddingByFieldReduction(AmbientSpace(ps1),AmbientSpace(ps2));
	
	f1:=ps1!.basefield;
	basis:=Basis(AsVectorSpace(f2,f1));
	fun:=function(x)
		local mat;
		mat := x!.obj;
		if x!.type = 1 then
			mat := [mat];
		fi;
		#projfun:=em!.fun;
		#isofun := iso!.fun;
		return VectorSpaceToElement(canonical,(BlownUpMat(basis,mat)*change));
		#return isofun(VectorSpaceToElement(ps2,projfun(x)!.obj));
	end;
	
	prefun:=function(x)
		local mat; #projprefun,isoprefun;
		mat := x!.obj;
		if x!.type = 1 then
			mat := [mat];
		fi;
		mat := mat*invchange;
		#projprefun:=em!.prefun;
		#isoprefun := iso!.prefun;
		#return VectorSpaceToElement(ps1,projprefun(isoprefun(x))!.obj);
		return VectorSpaceToElement(ps1,ShrinkMat(basis,mat));
	end;
	
	map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1),
                                         ElementsOfIncidenceStructure(canonical),
                                         fun, false, prefun);
		
	SetIsInjective( map, true );
	
	## Now creating intertwiner
	
	if computeintertwiner then
		hom := function( m )
			local image;      
			image := BlownUpMat(basis, m!.mat); 
			ConvertToMatrixRepNC( image, f2 );       
			return CollineationOfProjectiveSpace(image, f2);
		end;
		hominv := function( m )
			local preimage;      
			preimage := ShrinkMat(basis, m!.mat); 
			ConvertToMatrixRepNC( preimage, f1 );       
			return CollineationOfProjectiveSpace(preimage, f1);
		end;
		g1 := SimilarityGroup( ps1 );
		gens := GeneratorsOfGroup( g1 );
		newgens := List(gens, hom);
		g2 := Group( newgens );
		SetSize(g2, Size(g1));
		twiner := GroupHomomorphismByFunction(g1, g2, hom, hominv);
		SetIntertwiner( map, twiner);
	fi;
	return map;
	end );


#####################################################################################
#
# John's method for CanonicalEmbeddingByFieldReduction. It works well except for the
# last two quadratic form cases. Since q is odd, I have come up with a work around.
# Something is wrong with QuadraticFormFieldReduction.
#
#####################################################################################

# JB: Is the argument "basis" really needed? I've made two operations for my work-around below
# that can be distinguished from the other methods by the number of arguments (I do not have "basis").

InstallMethod( QuadraticFormFieldReduction,
	"for a quadratic form and a field",
	[ IsQuadraticForm, IsField, IsFFE ],
	function(qf1, f2, alpha)
		# f2 is a subfield of the basefield for the quadratic form q1
	local f1,d1,t,d2,V2,phi,b2,b2vecs,mat1,mat2,i,j,bil1,qf2,bil2;

	# In the odd characteristic case, we can use the bilinear form.
	if IsOddInt(Size(f2)) then
	   	bil1 := AssociatedBilinearForm( qf1 );
	   	bil2 := BilinearFormFieldReduction(bil1, f2, alpha);
	    qf2 := QuadraticFormByBilinearForm(bil2);
    else
		# There is still a problem here that needs resolving.
		mat1 := qf1!.matrix;
		f1 := qf1!.basefield;
		d1 := Size(mat1);
		t := Dimension(AsVectorSpace(f2,f1));
		d2 := d1*t;
		b2 := Basis(f2^d2);
		b2vecs := BasisVectors(b2);
		for i in [1..d2-1] do
			for j in [i+1..d2] do
				mat2[i][j]:=Trace(f1,f2,alpha*(ShrinkVec(f1,f2,b2vecs[i]+b2vecs[j]))^qf1
								-(ShrinkVec(f1,f2,b2vecs[i]))^qf1-(ShrinkVec(f1,f2,b2vecs[j]))^qf1);
			od;
		od;
		qf2 := QuadraticFormByMatrix(mat2,f2);
	fi;	
	return qf2;
end );

InstallMethod( BilinearFormFieldReduction,
	"for a bilinear form and a field",
	[ IsBilinearForm, IsField, IsFFE ],
	function(bil1, f2, alpha)
		# f2 is a subfield of the basefield of the bilinear form bil1
	local mat1,f1,d1,t,d2,b2,b2vecs,mat2, bil2;
	mat1 := bil1!.matrix;
	f1 := bil1!.basefield;
	d1 := Size(mat1);
	t := Dimension(AsVectorSpace(f2,f1));
	d2 := d1*t;
	b2 := Basis(f2^d2);
	b2vecs := BasisVectors(b2);	
	mat2 := List([1..d2], i -> List([1..d2], j -> 
		Trace(f1,f2,alpha*(ShrinkVec(f1,f2,b2vecs[i]) * mat1 * ShrinkVec(f1,f2,b2vecs[j])))));	
	bil2 := BilinearFormByMatrix(mat2,f2);
	return bil2;
end );

InstallMethod (CanonicalEmbeddingByFieldReduction,
	"for a polar space, a field, and a boolean",
	[IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool],
	function(ps1, ps2, computeintertwiner)

	#####################################################################
	#  Different cases of PolarSpace(A-1, q^w) -> PolarSpace(wA-1, q)
	#
	#  Sesquilinear forms
	#  	H -> H (w odd)
	#  	H -> W (w even)
	#  	H -> Q+ (w even, A even)
	#  	H -> Q- (w even, A odd)
	#  	W -> W (always)
	#
	#  Quadratic forms	
	# A even			
	#	Q+ -> Q+ (always)
	#	Q- -> Q- (always)
	# A odd
	#	Q -> Q (w odd, q odd)
	#   Q -> Q+ (w even, q odd)
	#   Q -> Q- (w even, q odd)
	#####################################################################

    local type1, type2, form1, form1b, form2, f1, f2, w, newps, sigma, map, 
		  gamma, q, n, A, alpha, basis, is_square, morphism, iso;
					
	type1 := PolarSpaceType(ps1);
	type2 := PolarSpaceType(ps2);
	if type1 in ["hyperbolic", "parabolic", "elliptic"] then 
	   form1 := QuadraticForm(ps1);
	else
	   form1 := SesquilinearForm(ps1);
	fi;
	f1 := ps1!.basefield;
	f2 := ps2!.basefield;
	q := Size(f2);
	
	if not IsSubset(f1,f2) then
	   Error("Fields are incompatible");
	fi;
	w := Log(Size(f1),q);	# conforming to Nick's notation
	A := ProjectiveDimension(ps1)+1; 	# conforming to Nick's notation
	basis := CanonicalBasis(AsVectorSpace(f2,f1));
    is_square := function(x, f) return IsZero(x) or IsEvenInt(LogFFE(x,PrimitiveRoot(f))); end;
      
	if IsSesquilinearForm(form1) then
       	if type1 = "hermitian" and type2 = "hermitian" and IsOddInt(w) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
				# need alpha to be fixed by sigma: x -> x^q
			alpha := One(f1);
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
			
	   	elif type1 = "hermitian" and type2 = "symplectic" and IsEvenInt(w) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			if IsEvenInt(q) then
				# need alpha to be fixed by sigma: x -> x^q
				alpha := One(f1);				
			else
				# need sigma(alpha)=-alpha
				sigma := Sqrt(Size(f1));
				alpha := First(f1, t->not IsZero(t) and t^sigma = -t);				
			fi;	
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
		
	   	elif type1 = "hermitian" and type2 = "hyperbolic" and IsEvenInt(w) and IsEvenInt(A) 
				and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);	
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
						
	   	elif type1 = "hermitian" and type2 = "elliptic" and IsEvenInt(w) and IsOddInt(A) 
				and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);	
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
			
	   	elif type1 = "symplectic" and type2 = "symplectic" then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);				
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
		else
		 	Error("These polar spaces are not suitable for field reduction.");
		fi;
	else 	# form1 is a quadratic form
		if type1 = "hyperbolic" and type2 = "hyperbolic" then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
		elif type1 = "elliptic" and type2 = "elliptic" then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );	
		elif type1 = "parabolic" and type2 = "parabolic" and IsOddInt(w) and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction");
			alpha := One(f1);
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );			
		elif type1 = "parabolic" and type2 = "hyperbolic" and IsEvenInt(w) and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction: Parabolic -> Hyperbolic");
			# JB: I couldn't get it to work purely with quadratic forms, but we can use
			# the bilinear form since q is odd

			if q mod 4 = 1 then
 				alpha := First(f1, a -> not IsZero(a) and not is_square( a, f1 ) );
			else
				alpha := First(f1, a -> not IsZero(a) and is_square( a, f1 ) );
			fi;
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );
						
		elif type1 = "parabolic" and type2 = "elliptic" and IsEvenInt(w) and IsOddInt(q) then
			Info(InfoFinInG, 1, "These polar spaces are suitable for field reduction: Parabolic -> Elliptic");
			if q mod 4 = 1 then
 				alpha := First(f1, a -> not IsZero(a) and is_square( a, f1 ) );
			else
				alpha := First(f1, a -> not IsZero(a) and not is_square( a, f1 ) );
			fi;
			map := NaturalEmbeddingByFieldReduction( ps1, f2, alpha, basis, computeintertwiner );			
			
		else
		 	Error("These polar spaces are not suitable for field reduction.");
		
     	fi;	
	fi;
	iso := IsomorphismPolarSpacesNC(AmbientGeometry(Range(map)), ps2, computeintertwiner);	
	morphism := GeometryMorphismByFunction(ElementsOfIncidenceStructure(ps1), 
                                       ElementsOfIncidenceStructure(ps2), 
                                        x -> iso!.fun(map!.fun(x)), false,  x -> map!.prefun(iso!.prefun(x)) );

	# The intertwiner is a record element and so we need to adjust this manually
	# This still needs testing.
	
    SetIntertwiner(morphism, CompositionMapping(Intertwiner(iso), Intertwiner(map)) );
		
	return morphism;
end );




# CHECKED 28/09/11 jdb
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
		func :=  x -> VectorSpaceToElement(geom2, ShallowCopy(x!.obj));
		prefun := function( x )
			if not ForAll( Flat(x!.obj), i -> i in f1 ) then
				Error("Element does not have all of its coordinates in ", f1);
			fi;
			return VectorSpaceToElement(geom1, ShallowCopy(x!.obj));
		end;
		map := GeometryMorphismByFunction( ElementsOfIncidenceStructure(geom1), 
                                       ElementsOfIncidenceStructure(geom2),
                                       func, false, prefun ); 
		SetIsInjective( map, true );

    ## Intertwiner...
    
		twinerfunc := x -> CollineationOfProjectiveSpace( x!.mat, f2 );   
		twinerprefun := y -> CollineationOfProjectiveSpace( y!.mat, f1 );
        g1 := ProjectivityGroup( geom1 );
		gens1 := GeneratorsOfGroup( g1 );
		gens2 := List(gens1, twinerfunc );
		g2 := Group(gens2);
		SetSize(g2, Size(g1));
		hom := GroupHomomorphismByFunction(g1, g2, twinerfunc, twinerprefun);    
		SetIntertwiner(map, hom);
		return map;
	end );
  
# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalEmbeddingBySubfield( <geom1>, <geom2>, <bool> )
# returns the embedding of <geom1> in <geom2>, two polar spaces of the
# same dimension, over the fields K and L respectively, where L is an extension
# of K. An Intertwiner is computed if <bool> is true.
##
InstallMethod( NaturalEmbeddingBySubfield, 
	"for two polar spaces and a boolean",
	[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
	function( geom1, geom2, computeintertwiner )
		local map, f1, f2, q1, q2, gamma, func, prefun, g1, g2, gens1, gens2, f,
          r, type1, type2, iso, gram, newgram, form, ps, hom, twinerfunc, twinerprefun;
        
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
				type2 = "hermitian" and r =2 and IsOddInt(q2) then
				form := HermitianFormByMatrix(gram, f2);
			elif (IsOddInt(r) and type2 in ["elliptic", "parabolic", "hyperbolic"] and 
				type1 = type2) or (IsEvenInt(r) and [type1, type2] in [["elliptic","hyperbolic"], 
				["hyperbolic","hyperbolic"], ["parabolic","parabolic"]]) then
				form := BilinearFormByMatrix(gram, f2);           
			else
				Error("Not possible for these geometries\n");
			fi;   
		else
			Error("Dimensions and/or field sizes are incompatible"); return;
		fi;
		ps := PolarSpace(form);
		iso := IsomorphismPolarSpacesNC(ps, geom2, computeintertwiner);
		func :=  x -> VectorSpaceToElement(ps, ShallowCopy(x!.obj))^iso;
		prefun := function( x )
			local y;            
            y := iso!.prefun(x);
            if not ForAll( Flat(y!.obj), i -> i in f1 ) then
				Error("Element does not have all of its coordinates in ", f1);
			fi;
            return VectorSpaceToElement(geom1, ShallowCopy(y!.obj));
		end;
		map := GeometryMorphismByFunction( ElementsOfIncidenceStructure(geom1), 
                                       ElementsOfIncidenceStructure(geom2),
                                       func, false, prefun ); 
		SetIsInjective( map, true );
    
    ## intertwiner...

		if HasIntertwiner( iso ) then 
			f := Intertwiner(iso);
			twinerfunc := x -> ImageElm(f, CollineationOfProjectiveSpace( x!.mat, f2 ));   
			twinerprefun := y -> CollineationOfProjectiveSpace( f!.prefun(y)!.mat, f1 );
			g1 := SimilarityGroup( geom1 );
			gens1 := GeneratorsOfGroup( g1 );
			gens2 := List(gens1, twinerfunc );
			g2 := Group(gens2);
			SetSize(g2, Size(g1));
			hom := GroupHomomorphismByFunction(g1, g2, twinerfunc, twinerprefun);    
			SetIntertwiner(map, hom);
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

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalProjectionBySubspace( <ps>, <v> )
# returns the morphism from the projective space to the quotient space of the 
# subspace <v>. It is checked if <v> is a subspace of <ps>.
##
InstallMethod( NaturalProjectionBySubspace, 
	"for a projective space and a singular subspace",
	[ IsProjectiveSpace, IsSubspaceOfProjectiveSpace ], 
	function(ps, v)

  ## map from geometry of elements incident with v
  ## to the projective space of lesser dimension 

    local psdim, b, vdim, vs, sub, func, pre,
          Wvectors, mb, compl, gen, bas, img, 
          canbas, zero, basimgs, ps2, map, hom, f;
    if not v in ps then
       Error("Subspace is not a member of the projective space");
    fi;
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
        if not v in x then
			Error("Subspace is not incident with the subspace of projection");
		fi;
        y := List(x^_,i-> Coefficients(bas,i))*basimgs;  
        if not IsEmpty(y) then 
           ## Note: TriangulizeMat does not return a matrix of full
           ##       rank, whereas SemiEchelonMat does!
     		y := SemiEchelonMat(y)!.vectors;  ## JB: 3/11/2012. Moved this line from the "else" to here so it also applies to the first case.

			if x!.type - vdim = 1 then 
				y := y[1]; 
				ConvertToVectorRep(y, f);
			else
				ConvertToMatrixRepNC(y, f);
			fi;
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
#O  \QUO( <ps>, <v> )
# returns the quotient space of the subspace <v> of the projective space <ps>
# it is checked if <v> is a subspace of <ps>
##
InstallOtherMethod(\QUO,  
	"for a projective space and a subspace",
	[ IsProjectiveSpace and IsProjectiveSpaceRep, IsSubspaceOfProjectiveSpace],
	function( ps, v )
		if not v in ps then 
			Error( "Subspace does not belong to the projective space" );
		fi;
		return Range( NaturalProjectionBySubspace( ps, v ) )!.geometry;
	end );

# CHECKED 28/09/11 jdb
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
		Error("Subspace is not a member of the polar space");
    fi;
    pstype := PolarSpaceType(ps); 
    psdim := ps!.dimension;
    f := ps!.basefield;
    b := v!.obj;
    vdim := v!.type;  
    if vdim = 1 then b:=[b]; fi; 
    perp := PolarMap(ps);
    vs := VectorSpace(f, perp(v)!.obj);

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
    ConvertToMatrixRep(basimgs, f);
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
                 y := List(x!.obj,i-> Coefficients(bas,i))*basimgs;  
                 y := SemiEchelonMat(y)!.vectors;   
                 if x!.type - vdim = 1 then 
                    y := y[1]; 
                    ConvertToVectorRep(y, f);
                 else
                    ConvertToMatrixRepNC(y, f);
                 fi;
                 return VectorSpaceToElement(ps2, y);
              fi;
           end;

    pre := function( y )
              local x;   
              if not y in ps2 then
                 Error("Subspace is not an element of the polar space");
              fi;
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
# two helper operations. They can be used by the users, but on the other hand
# the form of the Klein quadric is fixed, so this is against the philiosphy of
# FinInG, at least for user functions. Also, there are no checks. So I will not
# describe these in the documentation.
#############################################################################

# CHECKED 28/09/11 jdb
#############################################################################
#O  PluckerCoordinates( <l> )
# returns the Plucker coordinates of the line <l>.
##
InstallMethod( PluckerCoordinates, 
	"for a line of PG(3,q)",
    [ IsSubspaceOfProjectiveSpace ],
	function( l )
		local pij, lobj, u, v, coords;
		pij := function(u,v,i,j)
			return u[i]*v[j] - u[j] * v[i];
		end;
		lobj := l!.obj; 
		u := lobj[1];
		v := lobj[2];
		coords := [pij(u,v,1,2),pij(u,v,1,3),pij(u,v,1,4),
			pij(u,v,2,3),pij(u,v,4,2),pij(u,v,3,4)];
		return coords;
	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  InversePluckerCoordinates( <var> )
# returns a list of two vectors spanning the line with Plucker coordinates <var>
# cuation: this list is not Triangulized!
##
InstallMethod( InversePluckerCoordinates, 
	"for a point of Q+(5,q)",
    [ IsSubspaceOfProjectiveSpace ],

   ## The point must satisfy the Pluecker relation x1x6+x2x5+x3x4=0.
	function( var )
		local pairs, i, pair, l, f, zero, x;
		x := var!.obj;
		pairs := [[1,2],[1,3],[1,4],[2,3],[4,2],[3,4]];
		i := PositionNonZero(x);
		pair := pairs[i];
		f := var!.geo!.basefield;
		zero := Zero(f);
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

# CHECKED 28/09/11 jdb
#############################################################################
#O  KleinCorrespondence( <var> )
# returns the well known morphism from the lines of PG(3,q) to the points
# of Q+(5,q). Of course, the bilinear form determining the latter, can be chosen
# by the users.
##
InstallMethod( KleinCorrespondence, 
	"for a hyperbolic quadric",
     [ IsClassicalPolarSpace ],
	function( quadric )
		local f, i, form, map, pg, mat,
			pre, plucker, ps, iso, inv, one;
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
			mat[i][7-i] := one;
		od;
		if IsEvenInt( Size(f) ) then
			form := QuadraticFormByMatrix(mat, f);
		else
			form := BilinearFormByMatrix(mat + TransposedMat(mat), f);
		fi;
		ps := PolarSpace( form );
		iso := IsomorphismPolarSpacesNC( ps, quadric );
		pg := ProjectiveSpace(3,f);
		plucker := 
           function( l )
             local pt1, pt2;
             pt1 := PluckerCoordinates( l );
             pt2 := ImageElm( iso, VectorSpaceToElement(ps, pt1) );
             return pt2;
           end;
		inv := 
           function( var )
             local x, l;
             x := iso!.prefun(var);
             l := InversePluckerCoordinates( x );
             return VectorSpaceToElement(pg, l);
           end;
		map := GeometryMorphismByFunction(Lines(pg), Points(quadric), plucker, inv);
		SetIsBijective(map, true);
		return map;

    ## put intertwiner in PGammaL(4,q)->PGO^+(6,q) 
	end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalDuality( <w> )
# returns the well known isomorphism between W(3,q) and Q(4,q). The latter 
# will be the standard one, the former a user defined one.
##
InstallMethod( NaturalDuality, 
	"for a symplectic polar space of rank 2",
	[IsSymplecticSpace and IsGeneralisedPolygon ],
	function( w )
    ## First, we define the klein quadric which naturally corresponds
    ## with the canonical symplectic quadrangle. We then
    ## setup a polar space isomorphism for the given symplectic space.
    ## The function of our mapping takes a line of w, maps
    ## it to the canonical quadrangle, and then maps it to the Klein
    ## quadric.
    local f, iso, mat, form_quadric, quadric, klein, hyp, pg,
          q4q, emq4q, func, map, one, i, pre;
    f := w!.basefield;
    one := One(f);
    mat := NullMat(6, 6, f);
    for i in [1..3] do
       mat[i][7-i] := one;
    od;
    if IsOddInt(Size(f)) then
       form_quadric := BilinearFormByMatrix(mat+TransposedMat(mat), f);
    else  
       form_quadric := QuadraticFormByMatrix(mat, f);
    fi;
    quadric := PolarSpace( form_quadric );
    SetIsHyperbolicQuadric( quadric, true );
    klein := KleinCorrespondence( quadric ); 
    pg := AmbientSpace(quadric);
    hyp := IdentityMat(6, f){[1..5]};
    hyp[1][6] := -one;
    hyp := VectorSpaceToElement(pg, hyp);
    q4q := ParabolicQuadric(4, f);
    emq4q := NaturalEmbeddingBySubspace(q4q, quadric, hyp);
    if IsCanonicalPolarSpace( w ) then
       func := l -> PreImageElm(emq4q, ImageElm(klein, l));
       pre := p -> Wrap( w, 2, PreImageElm(klein, ImageElm(emq4q, p))!.obj );
    else
       iso := IsomorphismCanonicalPolarSpace( w );
       func := function( l )
              local p;         
              p := ImageElm(klein, PreImageElm(iso, l));
              return PreImageElm(emq4q, p);
            end;
       pre := function( p )
              local l;
              l := ImageElm(emq4q, p);
              return Wrap( w, 2, PreImageElm(klein, ImageElm(iso, l))!.obj );
            end;
    fi;
    map := GeometryMorphismByFunction(Lines(w), Points(q4q), func, pre);
    SetIsBijective( map, true );
    return map;
 end );

# CHECKED 28/09/11 jdb
#############################################################################
#O  NaturalDuality( <w> )
# returns the well known isomorphism between H(3,q^2) and Q-(5,q). The latter 
# will be the standard one, the former a user defined one.
##
InstallMethod( NaturalDuality, 
	"for a hermitian variety of rank 2",
	[ IsHermitianPolarSpace and IsGeneralisedPolygon ],
	function( h )
    ## The way this works is that we map the lines of h to the canonical H(3,q^2),
    ## which Klein corresponds to points of Q+(5,q^2) using the usual plucker map.
    ## This subgeometry is then mapped to PG(5,q), where we can associate it to
    ## Q-(5,q).

    local f, frob, q, one, iso, eq, pg5, alpha, mat1, mat2, x, xinv,
          func, pre, i, map, e, ps, form, iso2;
    f := h!.basefield;
    frob := FrobeniusAutomorphism(f);
    q := Sqrt(Size(f));
    one := One(f);   
   
    eq := EllipticQuadric(5, q);
    pg5 := AmbientSpace( eq );

    ## We need a projectivity of PG(5,q^2) which maps
    ## the Plucker coordinates of the lines of H(3,q^2)
    ## to PG(5,q). See "Finite Projective Spaces of Three Dimensions"
    ## by Hirschfeld and Thas (section 19.2).

    alpha := First(f, a -> (a^q)^2 <> a^2);
    mat1 := IdentityMat(6, f) * alpha;
    mat2 := NullMat(6, 6, f);
    for i in [1..6] do
       mat2[i][7-i] := one;
    od;
    x := mat1 + mat2 * alpha^q;

    e := alpha^(q-1) + alpha^(1-q);

    ## The form for the Elliptic Quadric that we get is
    ## x1^2+x2^2+x3^2+x4^2+x5^2+x6^2 - e(x1x6+x2x5+x3x4)  

    mat1 := IdentityMat(6, GF(q));
    mat2 := NullMat(6, 6, f);
    for i in [1..3] do
       mat2[2*i-1][8-2*i] := one;
    od; 
    mat1 := mat1 - e * mat2;

    if IsOddInt( q ) then
       form := BilinearFormByMatrix(mat1 + TransposedMat(mat1), GF(q));
    else
       form := QuadraticFormByMatrix(mat1, GF(q));  
    fi;
    ps := PolarSpace( form );
    iso2 := IsomorphismPolarSpaces(ps, eq);
    xinv := x^-1;

    if IsCanonicalPolarSpace( h ) then
       func := function( l )
         local pl, mu, pos;
         pl := PluckerCoordinates( l );

         ## We must be careful at this step.
         ## Normalisation by a scalar actually matters here.
         ## Up to a scalar, pl is of the form
         ##     (x,y,z,z^q,y^q,x^q)
         ## and so before we multiply by x, we must have
         ## that our vector is really in this form (up to no scalar!).

         pos := PositionNonZero( pl );
         mu := First( AsList(f), m -> m^(q-1) = pl[pos] / pl[7-pos]);      
         pl := mu * pl;
         return VectorSpaceToElement(ps, pl * x)^iso2; 
       end;
 
       pre := function( p )
         local p2, invpl;
         p2 := PreImageElm(iso2, p); 
         p2 := VectorSpaceToElement(pg5, p2!.obj * xinv);
         invpl := InversePluckerCoordinates( p2 );
         return VectorSpaceToElement(h,invpl);         
       end;
    else
       iso := IsomorphismCanonicalPolarSpace( h );
    
       func := function( l )
         local pl, mu, pos;
         pl := PluckerCoordinates( PreImageElm(iso, l) );
         pos := PositionNonZero( pl );
         mu := First( AsList(f), m -> m^(q-1) = pl[pos] / pl[7-pos]);      
         pl := mu * pl;
         return VectorSpaceToElement(ps, pl * x)^iso2; 
       end;
 
       pre := function( p )
         local p2, invpl;
         p2 := PreImageElm(iso2, p); 
         p2 := VectorSpaceToElement(pg5, p2!.obj * xinv);
         invpl := InversePluckerCoordinates( p2 );
         return ImageElm(iso, VectorSpaceToElement(h,invpl));         
       end;
    fi;

    map := GeometryMorphismByFunction(Lines(h), Points(eq), func, pre);
    SetIsBijective( map, true );
    return map;
 end );

