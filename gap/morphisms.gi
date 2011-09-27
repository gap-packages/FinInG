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
##  Copyright 2008 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Implementation stuff for incidence geometry morphisms
##
#############################################################################

########################################
#
# Things To Do:
#
# - Better method for LeukBasis
# - Grassmann map
#   slightly different to Klein correpondence, is this a problem?
# - Preimages for GrassmannMap, VeroneseMap, and SegreMap.
# - SegreMap: source is not domain, what should we do?
# - intertwiners for GrassmannMap and SegreMap
# - should there be a type function as an attribute?
#
# Documentation check list
# - IsGeometryMorphism: done
# - Intertwiner: done
# - NaturalEmbeddingBySubspace(NC): done
# - NaturalProjectionBySubspace(NC): done
# - NaturalEmbeddingByFieldReduction: done
# - NaturalEmbeddingBySubfield: done
# - IsomorphismPolarSpaces(NC): done
# - VeroneseMap: move this to schemes
# - SegreMap: move this to schemes
# - PluckerCoordinates: not documented
# - InversePluckerCoordinates: not documented
# - KleinCorrespondence: done
# - GrassmannMap: move this to schemes
# - NaturalDuality: done
# - ProjectiveCompletion: done
#
########################################




############################################################
## 9.1 GENERAL 
## Here are some methods for the Image* operations
## for IsGeometryMorphism. 
##
############################################################


InstallOtherMethod( \^, [IsElementOfIncidenceStructure, IsGeometryMorphism],
  function(x, em)
    return ImageElm(em,x);
  end );

InstallOtherMethod( ImageElm, [IsGeometryMorphism, IsElementOfIncidenceStructure],
  function(em, x)
    return em!.fun(x); 
  end );

InstallOtherMethod( ImagesSet, [IsGeometryMorphism, IsElementOfIncidenceStructureCollection],
  function(em, x)
    return List(x, t -> em!.fun(t));
  end );

InstallOtherMethod( PreImageElm, [IsGeometryMorphism, IsElementOfIncidenceStructure],
  function(em, x)
    if IsInjective(em) then
       return em!.prefun(x); 
    else
       Error("Map is not injective");
    fi;
  end );
  
InstallOtherMethod( PreImagesSet, [IsGeometryMorphism, IsElementOfIncidenceStructureCollection],
  function(em, x)
    return List(x, t -> em!.prefun(t)); 
  end );


############################################################
##
## Operations
## 
############################################################


## The three methods for GeometryMorphismByFunction are analogous
## with the MappingByFunction function. It behaves in exactly the same
## way except that we return an IsGeometryMorphism.


InstallMethod( GeometryMorphismByFunction, 
  [ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction, IsBool, IsFunction ],
  function( els1, els2, fun, bool, prefun )
    local morphism;
    morphism := MappingByFunction(els1, els2, fun, bool, prefun );
    SetFilterObj( morphism, IsGeometryMorphism ); 
    return morphism;
  end );  
  
InstallMethod( GeometryMorphismByFunction, 
  [ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction, IsFunction ],
  function( els1, els2, fun, inv )
    local morphism;
    morphism := MappingByFunction(els1, els2, fun, inv );
    SetFilterObj( morphism, IsGeometryMorphism ); 
    return morphism;
  end );  

InstallMethod( GeometryMorphismByFunction, 
  [ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction ],
  function( els1, els2, fun )
    local morphism;
    morphism := MappingByFunction(els1, els2, fun);
    SetFilterObj( morphism, IsGeometryMorphism ); 
    return morphism;
  end );  

  
##########################################################
### 9.2 When to use geometry morphisms in FinInG (see documentation)
##########################################################
  

##########################################################
### 9.3 NATURAL GEOMETRY MORPHISMS
##########################################################

## The specialised operations...

### 9.3-1 NaturalEmbeddingBySubspace

InstallMethod( NaturalEmbeddingBySubspace, 
      "for a geometry into another, via a specified subspace",  
      [ IsProjectiveSpace, IsProjectiveSpace, IsSubspaceOfProjectiveSpace ],
  function( geom1, geom2, v ) 
    local d1, d2, rk, f, invbasis, basis, 
          func, pre, map, morphism, bs;
   
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
             if IsVector(newy) then 
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

InstallMethod( NaturalEmbeddingBySubspaceNC, 
      "for a geometry into another, via a specified subspace",  
      [ IsProjectiveSpace, IsProjectiveSpace, IsSubspaceOfProjectiveSpace ],
      
      ## This operation is just like its namesake except that it 
      ## has No Checks
      
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
             if IsVector(newy) then 
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



InstallMethod( NaturalEmbeddingBySubspace, 
      "for a geometry into another, via a specified subspace",  
      [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],

  function( geom1, geom2, v ) 
    local map, d1, d2, rk, f, i, basis, invbasis, bs,
          c1, c2, orth, tyv, quad1, quad2,
          change, perp2, formonv, ses1, ses2, 
          ty1, ty2, newmat, func, pre, invchange;
    rk := v!.type;
    ty1 := PolarSpaceType(geom1); 
    ty2 := PolarSpaceType(geom2); 
    f := geom1!.basefield;
    d1 := geom1!.dimension;
    d2 := geom2!.dimension;
    tyv := TypeOfSubspace( geom2, v );

    ## Check that fields are the same and v is non-degenerate 
       
    if geom2!.basefield <> f then
       Error("fields of both spaces must be the same");
       return;
    fi;

    if not (ty2 = "parabolic" and IsEvenInt(Size(f))) then
      perp2 := Polarity(geom2);
      if perp2(v) in geom2 then
         Error("subspace is degenerate"); 
         return;
      fi;
    fi;

    if rk > d2 or d1 > d2 then
       Error("dimensions are incompatible"); 
       return;
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
  
  
  
  
  
InstallMethod( NaturalEmbeddingBySubspaceNC, 
      "for a geometry into another, via a specified subspace, no-check version",  
      [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ],

  function( geom1, geom2, v ) 
    local map, rk, f, i, basis, invbasis, bs,
          c1, c2, orth, tyv, quad1, quad2,
          change, perp2, formonv, ses1, ses2, 
          ty1, ty2, newmat, func, pre, invchange;
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
  
  
  
InstallMethod( IsomorphismPolarSpaces, 
      "returns intertwiner for two similar polar spaces",  
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

InstallMethod( IsomorphismPolarSpaces, 
      "returns intertwiner for two similar polar spaces",  
      [ IsClassicalPolarSpace, IsClassicalPolarSpace ],
  function( ps1, ps2 )
    return IsomorphismPolarSpaces( ps1, ps2, true );
  end );

InstallMethod( IsomorphismPolarSpacesNC, 
      "returns intertwiner for two similar polar spaces, no-check version",  
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
  
InstallMethod( IsomorphismPolarSpacesNC, 
      "returns intertwiner for two similar polar spaces, no-check version",  
      [ IsClassicalPolarSpace, IsClassicalPolarSpace ],
  function( ps1, ps2 )  
    return IsomorphismPolarSpacesNC( ps1, ps2, true );
  end );


### 9.3-2 NaturalEmbeddingByFieldReduction

InstallMethod( ShrinkMat, "preimage of BlownUpMat",
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
  if not (IsInt(m/d) and IsInt(n/d)) then Error("The matrix does not have the right dimensions");
  fi;
  blocks:=List(Cartesian([0..m/d-1],[0..n/d-1]),ij->mat{[ij[1]*d+1 .. ij[1]*d+d]}{[ij[2]*d+1 ..ij[2]*d+d]});
  newmat:=[];
  for i in [1..m/d] do
    row:=[]; 
	for j in [1..n/d] do
		submat:=blocks[(i-1)*d+j];
		checklist:=List([1..d],i->(vecs[i]^(-1)*(Sum([1..d],j->submat[i][j]*vecs[j]))));
		if Size(AsSet(checklist))<>1 then Error("The matrix is not a blown up matrix");
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



InstallGlobalFunction( BlownUpProjectiveSpace,
    "blows up a projective space by field reduction",
  function(basis,pg1)
  local q,t,r,pg2,mat1,mat2;
  	q:=basis!.q;
	t:=basis!.d;
	if not pg1!.basefield=GF(q^t) then Error("The basis and the projective space are not compatible!");
	fi;
	r:=Dimension(pg1)+1;
	pg2:=PG(r*t-1,q);
	return pg2;
  end );

InstallGlobalFunction( BlownUpProjectiveSpaceBySubfield,
    "blows up a projective space by field reduction w.r.t. the canonical basis",
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


InstallGlobalFunction( BlownUpSubspaceOfProjectiveSpace,
    "blows up a subspace of a projective space by field reduction",
  function(basis,subspace)
  local pg1,q,t,r,pg2,mat1,mat2;
    pg1:=AmbientGeometry(subspace);
  	q:=basis!.q;
	t:=basis!.d;
	if not pg1!.basefield=GF(q^t) then Error("The basis and the subspace are not compatible!");
	fi;
	r:=Dimension(pg1)+1;
	pg2:=PG(r*t-1,q);
	mat1:=subspace!.obj;
	if subspace!.type = 1 then mat1 := [subspace!.obj]; fi; 
	mat2:=BlownUpMat(basis,mat1);
	return VectorSpaceToElement(pg2,mat2);
  end );

InstallGlobalFunction( BlownUpSubspaceOfProjectiveSpaceBySubfield,
# This is w.r.t. to the canonical basis of the field over the subfield.
	"blows up a subspace of projective space by field reduction",
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

  
InstallGlobalFunction( IsDesarguesianSpreadElement, 
    "checks if a subspace is a blown up point using field reduction",
  function(basis,subspace)
      local flag,q,t,pg1,pg2,em,basvecs,v,v1,i,mat,rt,r;
	flag:=true;
	q:=basis!.q;
	t:=basis!.d;
	pg2:=AmbientGeometry(subspace);
	rt:=Dimension(pg2)+1;
	if not (Dimension(subspace)+1 = t and IsInt(rt/t)) then flag:=false;
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
	  
	  
InstallGlobalFunction( IsBlownUpSubspaceOfProjectiveSpace, 
	"checks if a subspace is blown up using field reduction",
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
	if not (IsInt(rt/t) and IsInt((Dimension(subspace)+1)/t)) then flag:=false;
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
	  if not span = subspace then flag:= false;
	  fi;
	fi;
	return flag;
 end );	  


  
InstallMethod( NaturalEmbeddingByFieldReduction, 
     "for a geometry into another, via field reduction, wrt a basis",
     [ IsProjectiveSpace, IsProjectiveSpace, IsBasis ],
  function( geom1, geom2, basis )
  
  ##
  ## This morphism contains a func and prefunc with built-in check.
  ##
  
    local map, f1, f2, d1, d2, t, func, prefun, 
          g1, gens, newgens, g2, twiner, hom, hominv;
    f1 := geom1!.basefield; 
    f2 := geom2!.basefield;
	 
    d1 := geom1!.dimension + 1;
    d2 := geom2!.dimension + 1;
	if not (IsInt(d2/d1)) then 
		Error("The second geometry is not obtained from the first geometry by field reduction");
	fi;
	if not (IsBasis(basis) and f1=GF((basis!.q)^basis!.d) and f2=GF(basis!.q) and d1*(basis!.d)=d2) then
		Error("The basis is not a basis or is not compatible with the basefields of the geometries");
	fi;
	t:=d2/d1;
	
	func := function( x ); # This map blows up a subspace of geom1 to a subspace of geom2
		return BlownUpSubspaceOfProjectiveSpace(basis,x);
	end; 
	
	prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
	  local flag,basvecs,mat1,span,x,v,v1,i;
	  flag:=true;
	  if not subspace in geom2 then 
		Error("The input is not in the range fo the field reduction map!");
	  fi;
	  if not IsInt((Dimension(subspace)+1)/t) then flag:=false;
	  else
		basvecs:=BasisVectors(basis);
		mat1:=[];
		span:=[];
		repeat
		repeat x:=Random(Points(subspace)); 
		until not x in span;
		v:=Coordinates(x);
		v1:=List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
		Add(mat1,v1);
		span:=VectorSpaceToElement(geom2,BlownUpMat(basis,mat1));
		until Dimension(span)=Dimension(subspace);
		if not span = subspace then flag:= false;
		fi;
	  fi;
	  if flag= false then Error("The input is not in the range of the field reduction map!");
	  fi;
	  return VectorSpaceToElement(geom1,mat1);
	end;
       
	map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1),
                                         ElementsOfIncidenceStructure(geom2),
                                         func, false, prefun);
               
    ## Now creating intertwiner

    hom := function( m )
      local image;      
      image := BlownUpMat(basis, m!.mat); 
      ConvertToMatrixRepNC( image, f1 );       
      return ProjectiveSemilinearMap(image, f1);
    end;

    hominv := function( m )
      local preimage;      
      preimage := ShrinkMat(basis, m!.mat); 
      ConvertToMatrixRepNC( preimage, f1 );       
      return ProjectiveSemilinearMap(preimage, f1);
    end;

    g1 := HomographyGroup( geom1 );
    gens := GeneratorsOfGroup( g1 );
    newgens := List(gens, hom);
    g2 := Group( newgens );
    SetSize(g2, Size(g1));
    twiner := GroupHomomorphismByFunction(g1, g2, hom, hominv);

    SetIntertwiner( map, twiner);
    return map;
  end );

InstallMethod( NaturalEmbeddingByFieldReduction, 
     "for a projective space into another, via field reduction",
     [ IsProjectiveSpace, IsProjectiveSpace ],
  function( geom1, geom2 )
	local basis;
	basis:=Basis(AsVectorSpace(geom2!.basefield,geom1!.basefield));
	return NaturalEmbeddingByFieldReduction(geom1,geom2,basis);
  end );

#######################
########################
######################
#######################
########################
######################
#######################
########################
######################
#######################
########################
######################
#######################
########################
######################

## need a quicker method for this
#######################
########################
######################
## CHECK if this is necessary ... probably not

InstallGlobalFunction( LeukBasis, 
  function( f1, f2 )
  
    ## This function finds a basis {b_1,...,b_e}
    ## such that the sum of the (q+1) powers of these
    ## elements is zero.
  
    local q2, q1, e, tuples, iter, t, vec;
    vec := AsVectorSpace(f2,f1);
    q1 := Size(f1);
    q2 := Size(f2);
    e := Log(q1,q2);
    tuples := EnumeratorOfTuples(f1, e);;
    iter := Iterator( tuples );
    repeat
      t := NextIterator(iter);
    until not IsZero(Product(t)) and IsZero(Sum(List(t,i->i^(q2+1)))) and Basis(vec,t) <> fail;
    return t;
  end );



InstallMethod( NaturalEmbeddingByFieldReduction, [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
  function( geom1, geom2, computeintertwiner )
    local map, pgmap, f1, f2, q1, q2, d1, d2, vec, block, w, i, iter, f,
          bas, e, type1, type2, iso, gram, newgram, form, ps, func, prefun,
          hom, g1, gens, newgens, g2, twiner, twinerfunc, twinerprefun, hominv;
    
    #######################################################################
    #
    #  A good reference on field reduction of polar spaces is
    #  "Polar spaces and embeddings of classical groups" by Nick Gill
    #  (New Zealand J. Math). 
    #  There, the conditions on the polarity types can be found.
    #
    #  All possible cases:
    #  W -> W (bug for W(1,q^5) -> W(9,q))
    #  Q+ -> Q+
    #  Q- -> Q-
    #  Q (q = 1 mod 4) -> Q+
    #  Q (q = -1 mod 4) -> Q-
    #  Q (odd field ext.) -> Q
    #  H (odd field ext.) -> H 
    #  H (even dim) -> Q+
    #  H (odd dim) -> Q-
    #  H (even field ext.) -> W
    #
    #######################################################################

    f1 := geom1!.basefield; q1 := Size(f1); 
    f2 := geom2!.basefield; q2 := Size(f2);       
    d1 := geom1!.dimension + 1;
    d2 := geom2!.dimension + 1;
    type1 := PolarSpaceType(geom1);
    type2 := PolarSpaceType(geom2);
       
    if q1^d1 = q2^d2 and IsSubset(f1,f2) and d2 mod d1 = 0 then  
       e := d2/d1; 
       vec := AsVectorSpace(f2, f1);

       if [type1, type2] in [["symplectic", "symplectic"], 
                             ["elliptic", "elliptic"], 
                             ["hyperbolic", "hyperbolic"]] or
          ([type1, type2] = ["parabolic", "elliptic"] and q2 mod 4 = 3 and IsEvenInt(e)) or
          ([type1, type2] = ["parabolic", "hyperbolic"] and q2 mod 4 = 1 and IsEvenInt(e)) or
          ([type1, type2] = ["parabolic", "parabolic"] and IsOddInt(q2) and IsOddInt(e)) then
           
           ## This part has been tested for small examples            
           ## The basis here must have the sum of the b^(q+1) equal to 0 

          bas := Basis(vec, LeukBasis(f1,f2) );        
          if HasQuadraticForm(geom1) then
             gram := GramMatrix( QuadraticForm(geom1) );
             newgram := BlownUpMat( bas, gram );
             form := QuadraticFormByMatrix(newgram, f2);
          else
             gram := GramMatrix( SesquilinearForm(geom1) );
             newgram := BlownUpMat( bas, gram );
             form := BilinearFormByMatrix(newgram, f2);
          fi;        
                  
       elif (([type1, type2] = ["hermitian", "hyperbolic"] and IsEvenInt(d1) and IsEvenInt(e))) or
            (([type1, type2] = ["hermitian", "elliptic"] and IsOddInt(d1) and IsEvenInt(e))) then
       
          ## This part has been tested for small examples 
          ## THe new form is B'(x,y) = Tr( B(x,y) )
          ## Matrix?        

          bas := Basis(vec, LeukBasis(f1,f2) );
          w := First(bas, t->not t in f2);
          block := [[1,w],[w^q2,w^(q2+1)]]*One(f2);
          newgram := IdentityMat(d2, f2);
          for i in [1..d2/2] do
              newgram{[2*i-1,2*i]}{[2*i-1,2*i]} := block;
          od;
gram := GramMatrix( SesquilinearForm(geom1) );
newgram := BlownUpMat( bas, gram );
      #    form := QuadraticFormByMatrix(newgram, f2);

          form := QuadraticFormByMatrix(newgram, f1);

       elif [type1, type2] = ["hermitian", "hermitian"] and IsOddInt(e) then
       
          ## works for H(1,4^3) -> H(5,4)
       
          bas := Basis(vec, LeukBasis(f1,f2) );        
          gram := GramMatrix( SesquilinearForm(geom1) );
          newgram := BlownUpMat( bas, gram );
          form := HermitianFormByMatrix(newgram, f2);
                 
       elif [type1, type2] = ["hermitian", "symplectic"] and e = 2 then  
       
          ## for the moment, we just use the usual copy of H
          Info(InfoFinInG, 1, "Only works (at the moment) for the canonical Hermitian variety");
          
          bas := Basis(vec, LeukBasis(f1,f2) );        
          newgram := CanonicalGramMatrix("symplectic", d2, f2);
          form := BilinearFormByMatrix( newgram, f2 );                  
       else
          Error("Not implemented for these geometries\n");
       fi;   
    else
         Error("Dimensions and/or field sizes are incompatible"); return;
    fi;
     
    ps := PolarSpace(form);
    iso := IsomorphismPolarSpacesNC(ps, geom2, computeintertwiner);

    pgmap := NaturalEmbeddingByFieldReduction( AmbientSpace(geom1), AmbientSpace(geom2), bas);
    func := x -> iso!.fun( pgmap!.fun( x ) );
    prefun := x -> pgmap!.prefun( iso!.prefun( x ) );
    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(geom1), 
                                      ElementsOfIncidenceStructure(geom2), 
                                      func, false, prefun );
    SetIsInjective( map, true );

    if HasIntertwiner( iso ) then
       f := Intertwiner(iso);
    
       hom := function( m )
         local image;      
         image := BlownUpMat(bas, m!.mat); 
         ConvertToMatrixRepNC( image, f2 );       
         return ProjectiveSemilinearMap(image, f2);
       end;
    
       hominv := function( m )
         local preimage;      
         preimage := ShrinkMat(bas, m!.mat); 
         ConvertToMatrixRepNC( preimage, f1 );       
         return ProjectiveSemilinearMap(preimage, f1);
       end;

       twinerfunc := x -> ImageElm(f, hom( x ));    
       twinerprefun := y -> hominv( PreImageElm(f, y) );

       g1 := IsometryGroup( geom1 );
       gens := GeneratorsOfGroup( g1 );    
       newgens := List(gens, twinerfunc);   
       g2 := GroupWithGenerators( newgens );      
       SetSize(g2, Size(g1));
       twiner := GroupHomomorphismByFunction(g1, g2, twinerfunc, twinerprefun);
       SetIntertwiner( map, twiner);
    fi;
    return map;
  end );
  
InstallMethod( NaturalEmbeddingByFieldReduction, [ IsClassicalPolarSpace, IsClassicalPolarSpace ],
  function( geom1, geom2 )
    return NaturalEmbeddingByFieldReduction( geom1, geom2, true );
  end );

InstallMethod( NaturalEmbeddingBySubfield, [ IsProjectiveSpace, IsProjectiveSpace ],
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
    
    twinerfunc := x -> ProjectiveSemilinearMap( x!.mat, f2 );   
    twinerprefun := y -> ProjectiveSemilinearMap( y!.mat, f1 );
    
    g1 := ProjectivityGroup( geom1 );
    gens1 := GeneratorsOfGroup( g1 );
    gens2 := List(gens1, twinerfunc );
    g2 := Group(gens2);
    SetSize(g2, Size(g1));
    hom := GroupHomomorphismByFunction(g1, g2, twinerfunc, twinerprefun);    
    SetIntertwiner(map, hom);
    return map;
  end );

  
InstallMethod( NaturalEmbeddingBySubfield, [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ],
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
             type1 = type2) or       
            (IsEvenInt(r) and [type1, type2] in [["elliptic","hyperbolic"], 
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
       twinerfunc := x -> ImageElm(f, ProjectiveSemilinearMap( x!.mat, f2 ));   
       twinerprefun := y -> ProjectiveSemilinearMap( f!.prefun(y)!.mat, f1 );
    
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

InstallMethod( NaturalEmbeddingBySubfield, [ IsClassicalPolarSpace, IsClassicalPolarSpace ],
  function( geom1, geom2 )
    return NaturalEmbeddingBySubfield( geom1, geom2, true );
  end );

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
                if x!.type - vdim = 1 then 
                   y := y[1]; 
                   ConvertToVectorRep(y, f);
                else
                   y := SemiEchelonMat(y)!.vectors;  
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
              return Wrap(ps2, x!.type - vdim, y);
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



InstallOtherMethod(\QUO,  "quotients for projective spaces",
   [ IsProjectiveSpace and IsProjectiveSpaceRep, IsSubspaceOfProjectiveSpace],
  function( ps, v )
    if not v in ps then 
       Error( "Subspace does not belong to the projective space" );
    fi;
    return Range( NaturalProjectionBySubspace( ps, v ) )!.geometry;
  end );


InstallMethod( NaturalProjectionBySubspace, 
      "for a polar space and a singular subspace",
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
    perp := Polarity(ps);
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
    perp := Polarity(ps);
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





InstallMethod( PluckerCoordinates, "for a line of PG(3,q)",
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

InstallMethod( InversePluckerCoordinates, "for a point of Q+(5,q)",
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


InstallMethod( KleinCorrespondence, 
     "from PG(3,q) to a Klein quadric (of the user's choice)",
     [ IsHyperbolicQuadric ],
  function( quadric )
    local f, i, form, map, pg, mat,
          pre, plucker, ps, iso, inv, one;
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

InstallMethod( NaturalDuality, "from the lines of W(3,q) to the points of Q(4,q)",
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

InstallMethod( NaturalDuality, "from the lines of H(3,q^2) to the points of Q-(5,q)",
               [ IsHermitianVariety and IsGeneralisedPolygon ],
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

InstallMethod( VeroneseMap, "given a projective space PG(n,q)",
    [ IsProjectiveSpace ],
  function( pgdomain )
    local n,F,n2,pgimage,varmap,func,
          tups,beta,betainv,hom,
          g1,g2,twiner,gens,newgens;
    n := pgdomain!.dimension + 1;
    F := pgdomain!.basefield;
    n2 := (n-1)*(n+2)/2;
    pgimage := VeroneseVariety(n2, F);

    func := function( point )
      local i,j,list;
      list:=[];
      for i in [1..n] do
        for j in [i..n] do
          Add(list, point!.obj[i]*point!.obj[j] );
        od;
      od;
      ConvertToVectorRepNC( list, F );
      return Wrap(pgimage, 1, list);
    end;

    tups := Filtered(Tuples([1..n], 2),i->i[2]>=i[1]);

    beta := function( m )
      local rows;
      rows := List([1..n], i -> m[i]{[i..n]});
      return Concatenation(rows);
    end;

    betainv := function( v )
      local matb, i, j, x;
      matb := ShallowCopy( NullMat(n, n, F) );
          for i in [1..n] do
              for j in [i..n] do
                  x := v[Position(tups,[i,j])];
                  matb[i][j] := x;
                  matb[j][i] := x;
              od;
          od;
      return matb;
    end;
      
    hom := function( m )
      local basis1, basis2, image, mat;
      mat := m!.mat;
      basis1 := IdentityMat(n2+1, F);
      basis2 := List(basis1, betainv);
      image := List(basis2, b -> beta( TransposedMat(mat) * b * mat ));  
      ConvertToMatrixRepNC( image, F );       
      return ProjElWithFrob(image, IdentityMapping(F), F);
    end;
   
    g1 := HomographyGroup( pgdomain );
    gens := GeneratorsOfGroup( g1 );
    newgens := List(gens, hom);
    g2 := Group( newgens );
    SetSize(g2, Size(g1));
    twiner := GroupHomomorphismByImagesNC(g1, g2, gens, newgens);
    SetIsBijective(twiner, true);

    varmap := GeometryMorphismByFunction(Points(pgdomain), Points(pgimage), func);
    SetIsInjective( varmap, true );
    SetIntertwiner(varmap, twiner);
    return varmap;
  end );

InstallMethod( VeroneseMap, "given a dimension and field",
    [ IsPosInt, IsField ],
  function( d, F )
    return VeroneseMap( ProjectiveSpace(d, F) );
  end );

InstallMethod( VeroneseMap, "given a dimension and field order",
    [ IsPosInt, IsPosInt ],
  function( d, q )
    return VeroneseMap( ProjectiveSpace(d, q) );
  end );

InstallMethod( GrassmannMap, "given a dimension k and a projective space",
    [ IsPosInt, IsProjectiveSpace ],

  ## Warning: this operation is not compatible with
  ## PluckerCoordinates. To get the same image, you
  ## need to multiply the fifth coordinate by -1.

  function( k, pgdomain )
    local n,F,pgimage,varmap,func,dim;
    n := pgdomain!.dimension;  ## projective dimension
    F := pgdomain!.basefield;
 
    if (k <= 0  or k >= n-1) then 
         Error("The dimension of the subspace has to be at least 1 and at most ", n-2);
    fi;

   ## ambient projective space of image has dimension Binomial(n+1,k+1)-1
    dim := Binomial( n+1, k+1 ) - 1;
    pgimage := GrassmannVariety(k, n, F); 

    func := function( var )
      local basis,vector,list;
      if ProjectiveDimension(var) <> k then 
         Error("Input must have projective dimension ", k, "\n");
      fi;
      basis := var!.obj;
      list := TransposedMat(basis); 
      vector := List(Combinations([1..n+1], k+1), i -> DeterminantMat( list{i} ));  
      ConvertToVectorRepNC( vector, F );
      return Wrap(pgimage, 1, vector);
    end;

    varmap := GeometryMorphismByFunction(ElementsOfIncidenceStructure(pgdomain, k+1), 
                                         Points(pgimage), func);
    SetIsInjective( varmap, true );
    return varmap;
  end );

InstallMethod( GrassmannMap, "given a dimension k and a projective space",
    [ IsPosInt, IsPosInt, IsPosInt ],
  function( k, n, q )
    return GrassmannMap( k, ProjectiveSpace(n, q));
  end );

#InstallMethod( GrassmannMap, "given collection of varieties of a projectivespace",
#    [ IsAllSubspacesOfProjectiveSpace ],
#  function( vars )
#    return GrassmannMap( vars!.type-1, vars!.geometry);
#  end );



#############################################################################
#
# Display methods:
#
#############################################################################

InstallMethod( ViewObj, [ IsGeometryMorphism ],
  function( f )
     Print("<geometry morphism from "); 
     ViewObj(Source(f));
     Print( " to " );
     ViewObj(Range(f));
     Print(">");
  end );

InstallMethod( PrintObj, [ IsGeometryMorphism ],
  function( f )
     Print("Geometry morphism:\n ", f, "\n");
  end );

InstallMethod( Display, [ IsGeometryMorphism ],
  function( f )
     Print("Geometry morphism: ", Source(f), " -> ", Range(f), "\n");
  end );


InstallMethod( ViewObj, [ IsGeometryMorphism and IsMappingByFunctionWithInverseRep ],
  function( f )
     Print("<geometry morphism from "); 
     ViewObj(Source(f));
     Print( " to " );
     ViewObj(Range(f));
     Print(">");
  end );


InstallMethod( ViewObj, [ IsGeometryMorphism and IsMappingByFunctionRep ],
  function( f )
     Print("<geometry morphism from "); 
     ViewObj(Source(f));
     Print( " to " );
     ViewObj(Range(f));
     Print(">");
  end );

InstallMethod( PrintObj, [ IsGeometryMorphism and IsMappingByFunctionRep ],
  function( f )
     Print("Geometry morphism:\n ", f, "\n");
  end );

InstallMethod( Display, [ IsGeometryMorphism and IsMappingByFunctionRep ],
  function( f )
     Print("Geometry morphism: ", Source(f), " -> ", Range(f), "\n");
  end );


