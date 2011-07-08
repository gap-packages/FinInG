#############################################################################
##
##  enumerators.gi              FinInG package
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
##  Implementation stuff for enumerators of varieties of polar spaces
##
#############################################################################

########################################
#
# Things To Do:
#
# - testing
# - enumerators for symplectic spaces
#   perhaps we don't bother and just leave as is
# - remove "iso" from Enumerator for IsShadowSubspacesOfClassicalPolarSpace ?
# - make "specialresidual" more efficient
#   this function computes the "residual not in a hyperplane"
# - documentation
#
########################################
#now comes a separate section with function to be renamed :-)
#don't tell celle now about this disaster programming style :-D
## utility functions

# pos is the inverse function of ffenumber:

pos := function(q, x)
  return Position(AsList(GF(q)), x) - 1;
end;

div := function(a, b)
	return (a - (a mod b)) / b;
end;

ffenumber := function(q, a)       
  if a = 0 then return 0 * Z(q);
  else return Z(q)^(a-1);
  fi;
end;

# the next six are brand new.
unrank_GFQ := function(q, rk)
  local Q;
  Q := q * q;
  if rk = 0 then
    return 0*Z(Q);
  fi;
  return Z(Q)^(rk-1);
end;

rank_GFQ := function(q, x)
  local Q;
  Q := q * q;
  return Position(AsList(GF(Q)), x) - 1;
end;

alpha_power := function(q, a)
  local Q;
  Q := q * q;     
  return Z(Q)^a;
end;

log_alpha := function(q, x)   
	local b, Q;

	Q := q * q;
	if IsZero(x) then
		Error("Error in log_alpha: x is zero");
	fi;
	b := pos(Q, x);
	return b;
end;

beta_power := function(q, a)
  local Q;   
  Q := q * q;
  return Z(Q)^((q + 1) * a);
end;

log_beta := function(q, x)   
	local b, Q;

	Q := q * q;
	if IsZero(x) then
		Error("Error in log_beta: x is zero");
	fi;
    #Print("log_beta x=", x, "\n");
	b := pos(Q, x) - 1;
    #Print("log_beta x=", x, " b=", b, "\n");
	if ((b mod (q + 1)) > 0) then
		Error("Error in log_beta");
	fi;
	return div(b, q + 1);
end;

norm_one_element := function(q, a)
  local Q;
  Q := q * q;
  return Z(Q)^((q - 1) * a);
end;

index_of_norm_one_element := function(q, x)   
	local b, Q;

	Q := q * q;
	b := log_alpha(Q, x);
	if ((b mod (q - 1)) > 0) then
		Error("Error in index_of_norm_one_element");
	fi;
	return div(b, q - 1);
end;

#############################################################################
# Constructor operations:
#############################################################################

InstallMethod( EnumeratorByOrbit, [ IsSubspacesOfClassicalPolarSpace ],
  function( vs )

  ## In this operation, we provide a way to compute the full
  ## list of varieties by using the transitive action of the isometry
  ## group. Using an enumerator to get a full list is much slower.                              

  ## We use the package "orb" by Mueller, Neunhoeffer and Noeske,
  ## which is much quicker than using the standard orbit algorithm
  ## that exists in GAP.

    local ps, j, rep, vars, isom, gens;    
     ps := vs!.geometry;
     j := vs!.type;
     isom := IsometryGroup(ps);
     gens := GeneratorsOfGroup(isom);
     rep := RepresentativesOfElements(ps)[j];
     vars := Orb(gens,rep,OnProjSubspaces, rec( hashlen:=Int(5/4*Size(vs)) )); 
     Enumerate(vars, Size(vs));   
     return vars;
   end );

InstallMethod( AsList, "for subspaces of a polar space", [IsSubspacesOfClassicalPolarSpace],
 function( vs )
   return EnumeratorByOrbit( vs );
 end );

InstallMethod( AsSortedList, "for subspaces of a polar space", [IsSubspacesOfClassicalPolarSpace],
 function( vs )
   return EnumeratorByOrbit( vs );
 end );

InstallMethod( AsSSortedList, "for subspaces of a polar space", [IsSubspacesOfClassicalPolarSpace],
 function( vs )
   return EnumeratorByOrbit( vs );
 end );




InstallMethod( Enumerator, [ IsSubspacesOfClassicalPolarSpace ],
  function( vs )

    ## This method returns the enumerator for the varieties of
    ## a polar space, of a given type. There are three "helper"
    ## operations involved here: enum_orthogonal, enum_hermitian,
    ## and enum_symplectic. At the moment, we do not have nice methods
    ## for the points of a hermitian space, and for varieties of 
    ## a symplectic space.

    local ps, j, rep, enum, e, type, eEN, eNE, iso, pscanonical;
     ps := vs!.geometry;
     j := vs!.type;
     type := PolarSpaceType(ps);

     ## A bug here was fixed. When using non-canonical polar spaces, there was some problem
     ## in using the enum_* commands before an isomorphism was made. Fixed now. John 02/06/2010

     if HasIsCanonicalPolarSpace( ps ) and IsCanonicalPolarSpace( ps ) then     
        if type in [ "hyperbolic", "elliptic", "parabolic" ] then
           e := enum_orthogonal(ps, j);
        elif type = "hermitian" then
           e := enum_hermitian(ps, j);
        elif type = "symplectic" then
           e := enum_symplectic(ps, j);  
        else
           Error("Unknown polar space type");
        fi;

        enum := EnumeratorByFunctions( vs, rec(
                  ElementNumber := e!.ElementNumber,
                  NumberElement := e!.NumberElement,
                  PrintObj := function ( e )
                    Print( "EnumeratorOfSubspacesOfClassicalPolarSpace( " );
                    View( vs ); 
                    Print( " )" );
                    return;
                  end,
                  Length := function( e )
                    return Size(vs);
                    end
                ) );
     else
        ## The following is an isomorphism from the canonical polar space TO ps
  
        iso := IsomorphismCanonicalPolarSpace( ps );
        pscanonical := Source(iso)!.geometry;     

        if type in [ "hyperbolic", "elliptic", "parabolic" ] then
           e := enum_orthogonal(pscanonical, j);
        elif type = "hermitian" then
           e := enum_hermitian(pscanonical, j);
        elif type = "symplectic" then
           e := enum_symplectic(pscanonical, j);  
        else
           Error("Unknown polar space type");
        fi;

        ## These are enumerators in the canonical polar spaces

        eEN := e!.ElementNumber;
        eNE := e!.NumberElement;
                                                                        
        enum := EnumeratorByFunctions( vs, rec(
                   ElementNumber := function(e, n)
                     return iso!.fun( eEN( e, n ) );
                   end,
                   NumberElement := function(e, x)
                     return eNE( e, iso!.prefun(x) );
                   end,
                   PrintObj := function ( e )
                     Print( "EnumeratorOfSubspacesOfClassicalPolarSpace( " );
                     View( vs ); 
                     Print( " )" );
                     return;
                   end,
                   Length := function( e )
                     return Size(vs);
                   end
                 ) );  
     fi;
  ##   SetIsSSortedList( enum, true );  this line introduces a bug. It would be nice for it to be sorted though
     return enum;
   end );


InstallMethod( Enumerator, [IsShadowSubspacesOfClassicalPolarSpace and
                            IsShadowSubspacesOfClassicalPolarSpaceRep ],
  function( res )
    
    # Here we use the quotient geometry over res!.inner to compute
    # the residual.

    local pstype, psdim, f, b, vardim, perp, vs, 
          ps2, newdim, canon_quot, Wvectors, mb, compl,
          gen, m, newform, changeform_inv, cquot_to_res, 
          enumps2, enum, ps, var, j, out, m2, iso, proj,
          img, changeform, res_to_cquot, bas, canbas, zero, basimgs;

    ps := res!.geometry;
    var := res!.inner;
    out := res!.outer;
    j := res!.type;
    f := ps!.basefield;
    psdim := ps!.dimension;
    pstype := PolarSpaceType(ps); 

    if IsEmpty(var) then

       ## This is the residual for varieties contained in a subspace, such
       ## as "points on a line". Hence we need only use a method which 
       ## simulates the residual of a projective subspace.

       ps2 := res!.factorspace; 
	
       enumps2 := Enumerator( Subspaces(ps2, j) ); 
	
       enum := EnumeratorByFunctions( res, rec(
          ElementNumber := function(e, num)
             local x;
             x := BasisVectors( Basis( enumps2[num] ) );
             if j = 1 then
                return Wrap(ps, j, x[1]);
             else
                return Wrap(ps, j, x);
             fi;           
          end, 
          NumberElement := function( e, elm )
             local unelm;
             unelm := elm!.obj;
             if j = 1 then
                unelm := [ unelm ]; 
             fi;
             return Position(enumps2, SubspaceNC(ps2, unelm));
          end ) );

    elif var = out then    

       ## This is the trivial residual for the variety itself, i.e.,
       ## "lines incident with line" = "just the line itself".

       enum := EnumeratorByFunctions( res, rec(
          ElementNumber := function(e, num)
             return VectorSpaceToElement(ps, var);
          end, 
          NumberElement := function( e, elm )
             return 1;
          end ) );
    else

       ## This is the residual for varieties containing a subspace, such
       ## as "lines on a point". We use projection to create the enumerator.

       b := VectorSpaceToElement(ps, var);     
       proj := NaturalProjectionBySubspace(ps, b);  
       img := Range(proj)!.geometry;
       newdim := j - b!.type;
       canon_quot := ElementsOfIncidenceStructure(img, newdim); 
       enumps2 := Enumerator( canon_quot );

       enum := EnumeratorByFunctions( res, rec(
          ElementNumber := function(e, num)
             return proj!.prefun( enumps2[num] );  
          end, 
          NumberElement := function( e, elm )
             return Position( enumps2, proj!.fun(elm ) );
          end ) );

    fi;
    return enum;
  end );

InstallGlobalFunction(specialresidual, 
  function(ps, v, hyp)

  ## This function returns an enumerator for the j spaces on a 
  ## given j-1 space v which are not contained in a hyperplane hyp.
  ## This could be made more efficient.

  local j, enum, res;
    j := v!.type + 1; 
    res := Enumerator( ShadowOfElement(ps, v, j) );
    enum := Filtered(res, x -> not x in hyp); 
    return enum;
  end );

InstallGlobalFunction( enum_orthogonal,
 
  ## This is a "helper" function which makes the enumerator
  ## for a set of varieties of a canonical orthogonal space.

  function( ps, j )
  local hyp, pg, d, f, iter, x, ps2, vs, ressize, 
        em, varsps2jmin1, varsps2j, enum2, enum, enumextra, 
        vars, rep, disc, sqs, type, q, zero, one;

  vs := ElementsOfIncidenceStructure(ps, j);
  if j = 1 then
    enum := AntonEnumerator( vs );
  elif j > 1 then
    pg := AmbientSpace(ps);
    type := PolarSpaceType(ps);
    d := ps!.dimension;
    f := ps!.basefield;
    q := Size(f);   
    zero := Zero(f);
    one := One(f);
    
    ## The number of j spaces on a j-1 space is the number of points
    ## in the quotient polar space (over a j-1 space)
    
    x := List([1..d], i -> zero);
    hyp := ShallowCopy(IdentityMat(d,f));    
    if type = "parabolic" or type = "elliptic" then       
       hyp := Concatenation([x], hyp);     
    else 
       x[2] := one;
       x[d] := one;
       Append(hyp, [x]);
    fi;
    hyp := TransposedMat(hyp);
    hyp := VectorSpaceToElement(pg, hyp);               
    disc := TypeOfSubspace(ps, hyp); 

         ## Here ps2 is the polar space induced by the hyperplane section.
         ## ressize is the number of j spaces not contained in hyp 
         ## incident with a j-1 space contained in hyp. Of course, we
         ## have a special case when j is the Witt index.

    if type = "hyperbolic" then
       ps2 := ParabolicQuadric(d-1, f);
       ressize := Size(ElementsOfIncidenceStructure(HyperbolicQuadric(d+2-2*j, f), 1)); 
       if j <= WittIndex( SesquilinearForm(ps2) ) then #was "SesquilinearForm"
           ressize := ressize - Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+1-2*j, f), 1));
       fi;
    elif type = "elliptic" then   
       ps2 := ParabolicQuadric(d-1, f);
       ressize := Size(ElementsOfIncidenceStructure(EllipticQuadric(d+2-2*j, f), 1));
       if j <= WittIndex( SesquilinearForm(ps2) ) then #was "SesquilinearForm"
          ressize := ressize - Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+1-2*j, f), 1));
       fi;
    # type = "parabolic" in what follows
    elif disc = "elliptic" then
       ps2 := EllipticQuadric(d-1, f);
       ressize := Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+2-2*j, f), 1)); 
       if j <= WittIndex( SesquilinearForm(ps2) ) then #was "SesquilinearForm"
          ressize := ressize - Size(ElementsOfIncidenceStructure(EllipticQuadric(d+1-2*j, f), 1));
       fi;
    else 
       ps2 := HyperbolicQuadric(d-1, f);        
       ressize := Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+2-2*j, f), 1));
       if j <= WittIndex( SesquilinearForm(ps2) ) then #was "SesquilinearForm"
          ressize := ressize - Size(ElementsOfIncidenceStructure(HyperbolicQuadric(d+1-2*j, f), 1));
       fi;
    fi;

    em := NaturalEmbeddingBySubspace(ps2, ps, hyp);

    ## Enumerate last the j-spaces contained in the non-deg hyperplane  

    if j <= WittIndex( SesquilinearForm(ps2) ) then
       varsps2j := ElementsOfIncidenceStructure( ps2, j );
       enumextra := Enumerator( varsps2j );
    fi;
      
    varsps2jmin1 := ElementsOfIncidenceStructure( ps2, j-1 );
    enum2 := enum_orthogonal( ps2, j-1);  

    enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           local l, k, enumres, t, v;   
           if n > Size(enum2) * ressize then
              l := n - Size(enum2) * ressize;
              return enumextra[l]^em;
           else

           ## The way this works is that n is the position
           ## of the l-th j-space incident with the k-th (j-1)-space
           ## of vars2ps2. So we must first decompose n as n=(k-1)ressize+l.

              l := n mod ressize;
              if l = 0 then l := ressize; fi;  
              k := (n-l) / ressize + 1;    
              v := enum2[k]^em;   
              enumres := specialresidual(vs!.geometry, v, hyp);  
              return enumres[l];
           fi;
         end,
         NumberElement := function( e, x )

           ## Here we must first find the unique j-1 space
           ## incident with x, and then find its place in the ordering.

           local w, prew, k, enumres, l, numinhyp;

             ## first deal with the case that x is in hyp

             if x in hyp then
                numinhyp := Size(enum2) * ressize;
                prew := em!.prefun(x);
                return numinhyp + Position(enumextra, prew);
             else            
                w := Meet(hyp, x);        
                prew := em!.prefun(w);    
                k := Position(enum2, prew);    
                enumres := specialresidual(vs!.geometry, w, hyp); 
                l := Position(enumres, x);
                return (k-1)*ressize + l;
             fi;
         end ) );
  else 
     Error("j must be a positive integer");
  fi;
  return enum;
  end );

InstallGlobalFunction( enum_hermitian,

  ## This is a "helper" function which makes the enumerator
  ## for a set of varieties of an hermitian space.

  function( ps, j )
  local hyp, pg, d, f, ps2, vs, ressize, em, varsps2jmin1, 
        varsps2j, enum2, enum, enumextra, vars;
  vs := ElementsOfIncidenceStructure(ps, j);
  if j = 1 then
    enum := AntonEnumerator( vs );
  elif j > 1 then
    pg := AmbientSpace(ps);
    d := ps!.dimension;
    f := ps!.basefield;
    hyp := NullMat(d,d+1,f);
    hyp{[1..d]}{[2..d+1]} := IdentityMat(d, f);    
    hyp := VectorSpaceToElement(pg, hyp);
    
         ## ps2 is the polar space induced by the hyperplane section.
         ## ressize is the number of j spaces not contained in hyp 
         ## incident with a j-1 space contained in hyp. 

     ps2 := HermitianVariety(d-1, f);
     ressize := Size(ElementsOfIncidenceStructure(HermitianVariety(d+2-2*j, f), 1)); 
     if j < WittIndex( SesquilinearForm(ps2) ) then
        ressize := ressize - Size(ElementsOfIncidenceStructure(HermitianVariety(d+1-2*j, f), 1));
        varsps2j := ElementsOfIncidenceStructure( ps2, j );
        enumextra := Enumerator( varsps2j );
     fi;
     
     em := NaturalEmbeddingBySubspace(ps2, ps, hyp);        
     varsps2jmin1 := ElementsOfIncidenceStructure( ps2, j-1 );
     enum2 := enum_hermitian( ps2, j-1 ); 

     enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           local l, k, enumres, t, v;  
           if n > Size(enum2) * ressize then
              l := n - Size(enum2) * ressize;
              return enumextra[l]^em;
           else
              l := n mod ressize;
              if l = 0 then l := ressize; fi;  
              k := (n-l) / ressize + 1;    
              v := enum2[k]^em;                     
              enumres := specialresidual(vs!.geometry, v, hyp);  
              return enumres[l];
           fi;
         end,
         NumberElement := function( e, x )
           local w, prew, k, enumres, l, numinhyp;
             if x in hyp then
                numinhyp := Size(enum2) * ressize;
                prew := PreImage(em, x);
                return numinhyp + Position(enumextra, prew);
             else            
                w := Meet(hyp, x);
                prew := PreImage(em, w);
                k := Position(varsps2jmin1, prew);
                enumres := specialresidual(vs!.geometry, w, hyp); 
                l := Position(enumres, x);
                return (k-1)*ressize + l;
             fi;
         end ) );
  else 
     Error("j must be a positive integer");
  fi;
  return enum;
end );

InstallGlobalFunction( enum_symplectic,

  ## This is a "helper" function which makes the enumerator
  ## for a set of varieties of a symplectic space. We would
  ## like an algorithm for these. Recall that for the orthogonal
  ## and hermitian cases, we used induction by non-degenerate
  ## hyperplane sections to calculate the enumerators. We do not
  ## have non-degenerate hyperplanes in symplectic spaces!

  function( ps, j )
    local vs, vars, enum;
    vs := ElementsOfIncidenceStructure(ps, j);
    vars := EnumeratorByOrbit( vs );

    enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return vars[n];
         end,
         NumberElement := function( e, x )
           return Position(vars, x);
         end ) );

    return enum;
end );


#############################################################################
# Anton's Enumerator:
#############################################################################

## counting functions

InstallMethod(nb_pts_Nbar, [IsPosInt, IsPosInt],  
  function(n, q)
	if n = 1 then
		return q - 1;
	fi;
  end );

InstallMethod(nb_pts_S, [IsPosInt, IsPosInt],
  function(n, q)
	local a;	
	if n = 1 then
		return 2 * q - 1;
	fi;
	a := nb_pts_S(1, q) * nb_pts_S(n - 1, q);
	a := a + nb_pts_N(1, q) * nb_pts_N1(n - 1, q);
	return a;
  end );

InstallMethod(nb_pts_N, [IsPosInt, IsPosInt],
  function(n, q)
	local a;
	if n = 1 then
		return (q - 1) * (q - 1);
	fi;
	a := nb_pts_S(1, q) * nb_pts_N(n - 1, q);
	a := a + nb_pts_N(1, q) * nb_pts_S(n - 1, q);
	a := a + nb_pts_N(1, q) * (q - 2) * nb_pts_N1(n - 1, q);
	return a;
  end );

InstallMethod(nb_pts_N1, [IsPosInt, IsPosInt],
  function(n, q)
      local a;
	if n = 1 then
		return q - 1;
	fi;
	a := nb_pts_S(1, q) * nb_pts_N1(n - 1, q);
	a := a + nb_pts_N1(1, q) * nb_pts_S(n - 1, q);
	a := a + nb_pts_N1(1, q) * (q - 2) * nb_pts_N1(n - 1, q);
	return a;
  end );

InstallMethod(nb_pts_Sbar, [IsPosInt, IsPosInt],
  function(n, q)
	local a;
	if n = 1 then
		return 2;
	fi;
	a := nb_pts_Sbar(n - 1, q);
	a := a + nb_pts_Sbar(1, q) * nb_pts_S(n - 1, q);
	a := a + nb_pts_Nbar(1, q) * nb_pts_N1(n - 1, q);
	return a;
  end );

#counting functions for hermitian. to be renamed.

# June 25, 2011, Anton

InstallMethod(herm_nb_pts_N, [IsPosInt, IsPosInt],  
  function(n, q)
	local Q, a, b, c;
	Q := q * q;
	if n = 1 then
		return Q - 1;
	fi;
	a := herm_nb_pts_N(n - 1, q) * (Q - q - 1);
	b := herm_nb_pts_S(n - 1, q) * (Q - 1);
	c := a + b;
	return c;
  end );

InstallMethod(herm_nb_pts_S, [IsPosInt, IsPosInt],  
  function(n, q)
	local Q, a, b, c;
	Q := q * q;
	if n = 1 then
		return 1;
	fi;
	a := herm_nb_pts_N(n - 1, q) * (q + 1);
	b := herm_nb_pts_S(n - 1, q);
	c := a + b;
	return c;
  end );

InstallMethod(herm_nb_pts_N1, [IsPosInt, IsPosInt],  
  function(n, q)
	local Q, a, b, c;
	Q := q * q;
	if n = 1 then
		return q + 1;
	fi;
	a := herm_nb_pts_N1(n - 1, q);
	b := herm_nb_pts_N1(n - 1, q) * (q - 2) * (q + 1);
	c := herm_nb_pts_S(n - 1, q) * (q + 1);
	return a + b + c;
  end );

InstallMethod(herm_nb_pts_Sbar, [IsPosInt, IsPosInt],  
  function(n, q)
	local Q, a, b;
	Q := q * q;
	if n = 1 then
		return 0;
	fi;
	a := herm_nb_pts_Sbar(n - 1, q);
	b := herm_nb_pts_N1(n - 1, q);
	return a + b;
  end );


##############################
# "ElementNumber" functions
##############################

InstallMethod(N1_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, a)
  local l, i, j, k, j1, x, y, z, yz, u, alpha, beta, gamma, one, zero;
  one := Z(q)^0;
  zero := 0*Z(q);
  if n = 1 then
     l := q - 1;
     if a < l then
        alpha := ffenumber(q,a+1);
        beta := alpha^-1;
        v[offset + 0] := alpha;
        v[offset + 1] := beta;	
        return;
     fi;
     Error("Error in N1_unrank");
  fi;
  x := nb_pts_S(1, q);
  y := nb_pts_N1(n - 1, q);
  l := x * y;
  if a < l then
     i := div(a, y);    
     j := a mod y;
     S_unrank(q, v, offset + (n - 1) * 2, 1, i);
     N1_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := nb_pts_N1(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;

  if a < l then
     i := div(a, y);
     j := a mod y;
     N1_unrank(q, v, offset + (n - 1) * 2, 1, i);
     S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := nb_pts_N1(1, q);
  y := q - 2;
  z := nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  if a < l then
     i := div(a, yz);
     j1 := a mod yz;
     j := div(j1, z);
     k := j1 mod z;
     N1_unrank(q, v, offset + (n - 1) * 2, 1, i);
     alpha := ffenumber(q,2+j);  
     v[offset + 2 * (n - 1)] := alpha * v[offset + 2 * (n - 1)];
     N1_unrank(q, v, offset, n - 1, k);
     beta := -alpha;
     gamma := one + beta;
     for u in [0..n-2] do
	   v[offset + 2 * u] := gamma * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in N1_unrank (2)");
end );

InstallMethod(S_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, a)
  local l, i, j, x, y, u, alpha, beta, one, zero;
  one := Z(q)^0;
  zero := 0*Z(q);

  if n = 1 then 
     if a < q then 
        v[offset + 0] := ffenumber(q,a);
        v[offset + 1] := zero;		
        return;
     fi;
     a := a - (q - 1);
     if a < q then	
        v[offset + 0] := zero;
        v[offset + 1] := ffenumber(q,a);
        return;
     fi;
     Error("Error in S_unrank");
  fi;
  x := nb_pts_S(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;
  if a < l then 
     i := div(a, y);
     j := a mod y;
     S_unrank(q, v, offset + (n - 1) * 2, 1, i);
     S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := nb_pts_N(1, q);
  y := nb_pts_N1(n - 1, q);
  l := x * y;
  if a < l then
     i := div(a, y);
     j := a mod y;
     N_unrank(q, v, offset + (n - 1) * 2, 1, i);
     N1_unrank(q, v, offset, n - 1, j);
     alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
     beta := -alpha;
     for u in [0..n-2] do
         v[offset + 2 * u] := beta * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in S_unrank (2)");
end );

InstallMethod(Sbar_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, a)
	local l, i, j, x, y, u, alpha, beta, one, zero;
  one := Z(q)^0; 
  zero := 0*Z(q);

  if n = 1 then 
     if a = 0 then 
	  v[offset + 0] := one;
	  v[offset + 1] := zero;
	  return;
     fi;
     if a = 1 then
        v[offset + 0] := zero;
        v[offset + 1] := one;
        return;
     fi;
     Error("Error in Sbar_unrank");
  fi;
  y := nb_pts_Sbar(n - 1, q);
  l := y;
  if a < l then 
     u := n - 1;
     v[offset + 2 * u] := zero;
     v[offset + 2 * u + 1] := zero;
     Sbar_unrank(q, v, offset, n - 1, a);
     return;
  fi;
  a := a - l;
  x := nb_pts_Sbar(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;
  if a < l then
     i := div(a, y);
     j := a mod y; 
     Sbar_unrank(q, v, offset + (n - 1) * 2, 1, i);
     S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := nb_pts_Nbar(1, q);
  y := nb_pts_N1(n - 1, q);
  l := x * y;
  if a < l then
     i := div(a, y);
     j := a mod y;
     Nbar_unrank(q, v, offset + (n - 1) * 2, 1, i);
     N1_unrank(q, v, offset, n - 1, j);
     alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
     beta := -alpha;
     for u in [0..n-2] do
	   v[offset + 2 * u] := beta * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in Sbar_unrank (2)");
end );

InstallMethod(Nbar_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, a)
  if n = 1 then
	if a < q - 1 then
		v[offset + 0] := ffenumber(q,a+1);
		v[offset + 1] := Z(q)^0;
		return;
	fi;
      Error("Error in Nbar_unrank");
  fi;
  Error("Error in Nbar_unrank (2)");
end );

InstallMethod(N_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, a)
	local l, i, j, k, j1, x, y, z, yz, u, alpha, beta, gamma, delta, epsilon, 
            one, zero, w;
  w := Z(q);
  one := w^0;
  zero := 0*w;
  if n = 1 then
     x := q - 1;
     y := q - 1;
     l := x * y;
     if a < l then
        i := div(a, y);
        j := a mod y;
        v[offset + 0] := ffenumber(q,1+j); 
        v[offset + 1] := ffenumber(q,1+i);
        return;
     fi;
     Error("Error in N_unrank");
  fi;
  x := nb_pts_S(1, q);
  y := nb_pts_N(n - 1, q);
  l := x * y;
  if a < l then
     i := div(a, y);
     j := a mod y;
     S_unrank(q, v, offset + (n - 1) * 2, 1, i);
     N_unrank(q, v, offset, n - 1, j);
  return;
  fi;
  a := a - l;
  x := nb_pts_N(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;
  if a < l then
     i := div(a, y);
     j := a mod y;
     N_unrank(q, v, offset + 2 * (n - 1), 1, i);
     S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := nb_pts_N(1, q);;
  y := q - 2;
  z := nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  if a < l then
     i := div(a, yz);
     j1 := a mod yz;
     j := div(j1, z);
     k := j1 mod z;
     N_unrank(q, v, offset + (n - 1) * 2, 1, i);
     N1_unrank(q, v, offset, n - 1, k);
     alpha := w;  
     beta := alpha^(j + 1);
     gamma := v[offset + (n - 1) * 2] * v[offset + (n - 1) * 2 + 1];
     delta := -gamma;
     epsilon := delta * beta;
     for u in [0..n-2] do
         v[offset + 2 * u] := epsilon * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in N_unrank(2)");
end );

#######hermitian ranking stuff, new from Anton.
## New hermitian stuff...

##############################
# unrank/rank functions in pairs: herm_N_unrank / herm_N_rank
##############################

InstallMethod(herm_N_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, a)
  local Q, A, coset, rk1, x, one, zero, val, coset0, rk0, m_val, log, nb, l;
  Print("herm_N_unrank v=", v, " n=", n, " rk=", a, " offset=", offset, "\n");
  Q := q * q;
  one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
     l := Q - 1;
     if a < l then
        x := alpha_power(q, a);
        v[offset + 0] := x;
        return;
     fi;
     Error("Error in herm_N_unrank");
  fi;
  A := Q - q - 1;
  nb := herm_nb_pts_N(n - 1, q);
  if a < A * nb then
    coset := div(a, nb);
	rk1 := a mod nb;
    herm_N_unrank(q, v, offset, n - 1, rk1);
    if coset = 0 then
      v[offset + n - 1] := zero;
    else
      coset := coset - 1;
      val := evaluate_hermitian_form(q, v, offset, n - 1);
      coset0 := div(coset, q + 1);
      rk0 := coset mod q + 1;
      m_val := - val;
      log := log_beta(q, m_val);
      if coset0 >= log then
        coset0 := coset0 + 1;
      fi;
      v[offset + n - 1] := alpha_power(q, coset0) * norm_one_element(q, rk0);     
    fi;
  else
    a := a - A * nb;
	nb := herm_nb_pts_S(n - 1, q);
    rk1 := a mod nb;
    coset := div(a, nb);
    herm_S_unrank(q, v, offset, n - 1, rk1);
    v[offset + n - 1] := alpha_power(q, coset);
  fi;
end );

InstallMethod(herm_N_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local Q, rk, val, m_val, alpha, A, rk1, coset, a, coset0, rk0, beta, nb, log;
  Q := q * q;
  if n = 1 then
     alpha := v[offset + 0];
     rk := log_alpha(q, alpha);  
     return rk;
  fi;
  val := evaluate_hermitian_form(q, v, offset, n - 1);
  nb := herm_nb_pts_N(n - 1, q);
  if not IsZero(val) then
    rk1 := herm_N_rank(q, v, offset, n - 1);
    if IsZero(v[offset + n - 1]) then
      coset := 0;
    else
      m_val := - val;
      log := log_beta(q, m_val);
      a := v[offset + n - 1]^(q + 1);
      coset0 := log_beta(q, a);
      beta := v[offset + n - 1] * alpha_power(q, coset0)^-1;
      if coset0 > log then
        coset0 := coset0 - 1;
      fi;
      rk0 := index_of_norm_one_element(q, beta);
      coset := coset0 * (q + 1) + rk0;
      coset := coset + 1;
    fi;
	rk := coset * nb + rk1;
  else
    A := Q - q - 1;
    rk := A * nb;
    coset := log_alpha(q, v[offset + n - 1]);
	rk1 := herm_S_rank(q, v, offset, n - 1);
    rk := rk + coset * herm_nb_pts_S(n - 1, q) + rk1;
  fi;
  return rk;
end );


##############################
# unrank/rank functions in pairs: herm_S_unrank / herm_S_rank
##############################

InstallMethod(herm_S_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, rk)
  local Q, zero, nb, coset, rk1, log, val, m_val;
  Print("herm_S_unrank v=", v, " n=", n, " rk=", rk, " offset=", offset, "\n");
  Q := q * q;
  #one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
     v[offset + 0] := zero;
     return;
  fi;
  nb := herm_nb_pts_N(n - 1, q);
  if rk < (q + 1) * nb then
    coset := div(rk, nb);
    rk1 := rk mod nb;
    herm_N_unrank(q, v, offset, n - 1, rk1);
    val := evaluate_hermitian_form(q, v, offset, n - 1);
    m_val := - val;
    log := log_beta(q, m_val);
    v[offset + n - 1] := alpha_power(q, log) * norm_one_element(q, coset);
  else
    rk := rk - (q + 1) * nb;
    herm_S_unrank(q, v, offset, n - 1, rk);
    v[offset + n - 1] := zero;
  fi;
  Print("herm_S_unrank done: v=", v, " n=", n, " rk=", rk, "\n");
end );

InstallMethod(herm_S_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local val, rk, rk1, m_val, log, a, log1, nb, coset;
  if n = 1 then
     return 0;
  fi;
  rk := 0;
  if not IsZero(v[offset + n - 1]) then
    rk1 := herm_N_rank(q, v, offset, n - 1);
    val := evaluate_hermitian_form(q, v, offset, n - 1);
    m_val := - val;
    log := log_beta(a, m_val);
    a := v[offset + n - 1]^(q + 1);
    log1 := log_beta(q, a);
    if log1 <> log then
      Error("Error in hermitian::S_rank fatal: log1 != log");
    fi;
    a := v[offset + n - 1] * alpha_power(q, log)^-1;
    coset := index_of_norm_one_element(q, a);
    nb := herm_nb_pts_N(n - 1, q);
    rk := coset * nb + rk1;
  else
    rk := herm_S_rank(q, v, offset, n - 1);
    rk := rk + (q + 1) * nb;
  fi;
  return rk;
end );


##############################
# unrank/rank functions in pairs: herm_N1_unrank / herm_N1_rank
##############################

InstallMethod(herm_N1_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, rk)
  local Q, i, one, zero, nb, rk1, rk2, coset, coset1, coset2, nb1, a, val, new_val, log, A;

  Print("herm_N1_unrank v=", v, " n=", n, " rk=", rk, " offset=", offset, "\n");

  Q := q * q;
  one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
     v[offset + 0] := norm_one_element(q, rk);
     Print("herm_N1_unrank done: v=", v, " n=", n, " rk=", rk, "\n");
     return;
  fi;
  nb := herm_nb_pts_N1(n - 1, q);
  if rk < nb then
    herm_N1_unrank(q, v, offset, n - 1, rk);
    v[offset + n - 1] := zero;
    Print("herm_N1_unrank done: v=", v, " n=", n, " rk=", rk, "\n");
    return;
  else
    rk := rk - nb;
    A := (q + 1) * (q - 2) * nb;
    if rk < A then
      nb1 := (q - 2) * nb;
      coset1 := div(rk, nb1);
      rk1 := rk mod nb1;
      coset2 := div(rk1, nb);
      rk2 := rk1 mod nb;
      Print("herm_N1_unrank before call to herm_N1_unrank: v=", v, " n=", n, " rk2=", rk2, "\n");
      herm_N1_unrank(q, v, offset, n - 1, rk2);
      Print("herm_N1_unrank after call to herm_N1_unrank: v=", v, " IsFFECollection(v)=", IsFFECollection(v)," n=", n, " rk=", rk, " offset=", offset, "\n");
      val := evaluate_hermitian_form(q, v, offset, n - 1);
      coset2 := coset2 + 1;
      a := alpha_power(q, coset2);
      for i in [0 .. n - 2] do
        v[offset + i] := a * v[offset + i];
      od;
      Print("herm_N1_unrank before second call to evaluate_hermitian_form: v=", v, " IsFFECollection(v)=", IsFFECollection(v)," n=", n, " rk=", rk, " offset=", offset, "\n");
      val := evaluate_hermitian_form(q, v, offset, n - 1);
      new_val := one - val;
      log := log_beta(q, new_val);
      v[offset + n - 1] := alpha_power(q, log) * norm_one_element(q, coset1);
	else
      rk := rk - A;
      nb := herm_nb_pts_S(n - 1, q);
      coset := div(rk, nb);
      rk1 := rk mod nb;
      herm_S_unrank(q, v, offset, n - 1, rk1);
      v[offset + n - 1] := norm_one_element(q, coset);
    fi;

  fi;
  Print("herm_N1_unrank done: v=", v, " n=", n, " rk=", rk, "\n");

end );


InstallMethod(herm_N1_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local Q, one, val, rk, rk1, nb, nb1, A, coset, coset1, coset2, a, av, i, new_val, log, rk2, log1;
  Q := q * q;
  one := Z(Q)^0;
  if n = 1 then
     return index_of_norm_one_element(q, v[offset + 0]);
  fi;
  rk := 0;
  if IsZero(v[offset + n - 1]) then
    rk := herm_N1_rank(q, v, offset, n - 1, rk);
    return rk;
  fi;
  nb := herm_nb_pts_N1(n - 1, q);
  rk := nb;
  nb1 := (q - 2) * nb;
  A := (q + 1) * nb1;
	val := evaluate_hermitian_form(q, v, offset, n - 1);
	if not IsZero(val) then
		coset2 := log_beta(q, val);
		a := alpha_power(q, coset2);
		av := a^(-1);
		for i in [0 .. n - 2] do
			v[offset + i] := av * v[offset + i];
		od;
		rk2 := herm_N1_rank(q, v, offset, n - 1);
		coset2 := coset2 - 1;
		new_val := one - val;
		log := log_beta(q, new_val);
		a := v[offset + n - 1]^(q + 1);
		log1 := log_beta(q, a);
		a := alpha_power(q, log)^(-1);
		a := a * v[offset + n - 1];
		coset1 := index_of_norm_one_element(q, a);
		rk1 := coset2 * nb + rk2;
		rk := rk + coset1 * nb1 + rk1;
	else
		rk := rk + A;

		rk1 := herm_S_rank(q, v, offset, n - 1);
		coset := index_of_norm_one_element(q, v[offset + n - 1]);
		rk := rk + coset * herm_nb_pts_S(n - 1, q) + rk1;
    fi;
  return rk;


end );



##############################
# unrank/rank functions in pairs: herm_Sbar_unrank / herm_Sbar_rank
##############################

InstallMethod(herm_Sbar_unrank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt], 
  function(q, v, offset, n, rk)
  local Q, one, zero, a, log, nb;
  Print("herm_Sbar_unrank: v=", v, " n=", n, " rk=", rk, "\n");
  Q := q * q;
  one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
    Error("herm_Sbar_unrank error: n = 1");
  fi;
    nb := herm_nb_pts_Sbar(n - 1, q);

	if rk < nb then
		herm_Sbar_unrank(q, v, offset, n - 1, rk);
		v[offset + n - 1] := zero;
	else
		rk := rk - nb;

		herm_N1_unrank(q, v, offset, n - 1, rk);
		a := - one;
		log := log_beta(q, a);
		v[offset + n - 1] := alpha_power(q, log) * norm_one_element(q, 0);
	fi;
  Print("herm_Sbar_unrank done: v=", v, " n=", n, " rk=", rk, "\n");

end );


InstallMethod(herm_Sbar_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local val, rk, nb, i, a, b, bv;
  if n = 1 then
    Error("herm_Sbar_rank error: n = 1");
  fi;
	if IsZero(v[offset + n - 1]) then
		rk := herm_Sbar_rank(q, v, offset, n - 1);
	else
  		nb := herm_nb_pts_Sbar(n - 1, q);
		rk := nb;

		val := evaluate_hermitian_form(q, v, offset, n - 1);
		if not IsOne(val) then
			a := log_beta(q, val);
			b := alpha_power(q, a);
			bv := b^(-1);
			for i in [0 .. n - 1] do
				v[offset + i] := bv * v[offset + i];
			od;
		fi;
		val := evaluate_hermitian_form(q, v, offset, n - 1);

		rk := rk + herm_N1_rank(q, v, offset, n - 1);
	fi;
  return rk;
end );

####################

## New hermitian stuff...

InstallGlobalFunction( enum_line,
  function( A, B, n )
  
    ## This function returns the n-th point on the interval
    ## ]A,B[. This routine uses the ordering of a finite field GF(q).
    ## For instance, we know that 0 and 1 are the first two elements of GF(q).
    ## The integer n must be between 1 and q - 1.
   
    local pg, q, x, C;
    pg := A!.geo;
    q := Size( pg!.basefield );
    x := ffenumber( q, n + 1 );  ## n = 1 -> primitive root
    C := VectorSpaceToElement( pg, A!.obj + x * B!.obj ); 
    return C;
  end );

InstallGlobalFunction( enum_BaerSubline,
  function( q, n )
  
    ## This function returns the n-th point on the canonical Baer subline
    ## X0^(q+1) + X1^(q+1), of PG(1, q^2).
    ## Note: Z(q^2)^(q+1) = Z(q) in GAP.

    local k, x;
    
    ## Our points are (1, Z(q^2)^i) such that Z(q)^i=-1  (i in [1..q^2-1]).
    ## This condition holds iff i = j (q-1) + k with j in [0..q], 
    ## k = (q-1)/2 for q odd, k = q-1 for q even.
    
    if IsEvenInt(q) then 
       k := q - 1; 
    else 
       k := (q - 1)/2; 
    fi;
    x := [ Z(q)^0, Z(q^2)^( (n-1) * (q-1) + k ) ];        
    return x;    
  end );
   
InstallGlobalFunction( enum_unital,
  function( q, n )
  
    ## This function returns the n-th point on the canonical unital
    ## X0^(q+1) + X1^(q+1) + X2^(q+1).
    ##    
    ## First we use the representation of the classical unital U_{0,beta}
    ## from page 70 of Barwick and Ebert's book "Unitals in Projective Planes".
    ## That is:
    ##    U_{0,beta} = {(x, beta * x^{q+1} + r, 1) : x in GF(q^2), r in GF(q)}\cup {(0,1,0)}.
    ## This representation has the advantage that it is simple to enumerate by
    ## elements of GF(q^2) x GF(q) (plus one extra point (0,1,0)).
    ## The Gram matrix for this Hermitian curve is
    ##    [[beta-beta^q,0,0],[0,0,-1],[0,1,0]] * e where e^q+e=0.
    ## 
        
    local beta, one, i, j, x, r, point, e, mat, b, binv;
    
    beta := Z(q^2);
    one := beta^0;
    
    if n = q^3 + 1 then 
       point := [0,1,0] * one;
    else
       i := (n-1) mod q^2;                 ## an integer in [0..q^2-1]
       j := (n-1-((n-1) mod q^2)) / q^2;   ## an integer in [0..q-1]
       x := ffenumber( q^2, i );
       r := ffenumber( q, j );
       point := [x, beta * x^(q+1)+ r, one];
    fi;
    if IsEvenInt(q) then
       e := Z(q^2)^(q+1);
    else
       e := Z(q^2)^((q+1)/2);
    fi;
    mat := [[beta-beta^q,0,0],[0,0,-1],[0,1,0]] * e;
    b := BaseChangeToCanonical( HermitianFormByMatrix(mat, GF(q^2)) );
    binv := b^-1;
    return point * binv; 
  end );



##############################
# "NumberElement" functions
##############################

PG_element_normalize := function(v, offset, n)

  ## This function takes the first nonzero element
  ## from the right (!) and normalises the vector v
  ## by this element.

  local ii, i, a, one;
  one := One(v[1]);	   
  for ii in [0..n - 1] do 
      i := n - 1 - ii;    
      a := v[offset + i]; 
      if not IsZero(a) then
         if IsOne(a) then  
	     return;
         fi;
         a := a^-1;
         v{[offset..i-1+offset]} := a * v{[offset..i-1+offset]};
         v[offset + i] := one;
         return;
      fi;
  od;
  Error("zero vector");
end;

#merge these two, give it a sensible name, see if one can be thrown away...

InstallGlobalFunction(my_PG_element_normalize,
	[IsFFECollection, IsPosInt, IsPosInt],
	function(v, offset, n)

  ## This function takes the first nonzero element
  ## from the right (!) and normalises the vector v
  ## by this element.

  local ii, i, a, one;
  one := One(v[1]);	   
  for ii in [0..n - 1] do 
      i := n - 1 - ii;    
      a := v[offset + i]; 
      if not IsZero(a) then
         if IsOne(a) then  
	     return;
         fi;
         a := a^-1;
         v{[offset..i-1+offset]} := a * v{[offset..i-1+offset]};
         v[offset + i] := one;
         return;
      fi;
  od;
  Error("zero vector");

end );

InstallMethod(S_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local a, l, i, j, x, y, u, alpha, beta, gamma, delta, epsilon;
  if n = 1 then
     if IsZero(v[offset + 1]) then
        a := pos(q, v[offset + 0]);
        return a;
     fi;
     a := q - 1;
     a := a + pos(q, v[offset + 1]);
     return a;
   fi;
   x := nb_pts_S(1, q);
   y := nb_pts_S(n - 1, q);
   l := x * y;
   alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
   if IsZero(alpha) then
      i := S_rank(q, v, offset + 2 * (n - 1), 1);
      j := S_rank(q, v, offset, n - 1);
      a := i * y + j;
      return a;
   fi;
   a := l;
   x := nb_pts_N(1, q);
   y := nb_pts_N1(n - 1, q);
   i := N_rank(q, v, offset + 2 * (n - 1), 1);
   beta := -alpha;
   delta := -beta;
   for u in [0..n-2] do
       v[offset + 2 * u] := delta * v[offset + 2 * u];
   od;
   j := N1_rank(q, v, offset, n - 1);
   a := a + i * y + j;
   return a;
end );

InstallMethod(N_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local a, l, i, j, k, x, y, z, yz, u, alpha, beta, one,
            gamma, delta, epsilon, gamma2, epsilon_inv;
  one := Z(q)^0;
  if n = 1 then
     x := q - 1;
     y := q - 1;
     j := v[offset + 0] - one;
     i := v[offset + 1] - one;
     a := ffenumber(q, i * y + j); 
     return a;
  fi;
  gamma := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  x := nb_pts_S(1, q);
  y := nb_pts_N(n - 1, q);
  l := x * y;
  if IsZero(gamma) then
     i := S_rank(q, v, offset + 2 * (n - 1), 1);
     j := N_rank(q, v, offset, n - 1);
     a := i * y + j;
     return a;
  fi;
  a := l;
  x := nb_pts_N(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;
  gamma2 := evaluate_hyperbolic_quadratic_form(q, v, offset, n - 1);
  if IsZero(gamma2) then
     i := N_rank(q, v, offset + 2 * (n - 1), 1);
     j := S_rank(q, v, offset, n - 1);
     a := a + i * y + j;
  fi;
  a := a + l;
  x := nb_pts_N(1, q);
  y := q - 2;
  z := nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  i := N_rank(q, v, offset + 2 * (n - 1), 1);
  alpha := Z(q);
  delta := -gamma;
  for j in [0..q-3] do
      beta := alpha^(j + 1);
      epsilon := delta * beta;
      if epsilon = gamma2 then
         epsilon_inv := epsilon^-1;
         for u in [0..n-2] do
             v[offset + 2 * u] := epsilon_inv * v[offset + 2 * u];
         od;
         k := N1_rank(q, v, offset, n - 1);
         a := a + i * yz + j * z + k;
         return a;
      fi;
  od;
  Print("ERROR in N_rank gamma2 not found");
end );

InstallMethod(N1_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local a, l, i, j, k, x, y, z, yz, u, alpha, 
        alpha_inv, beta, gamma, gamma2, gamma_inv;
  if n = 1 then
     alpha := v[offset + 0];
    # beta := v[offset + 1];
    # gamma := alpha^-1;
     a := pos(q, alpha) - 1;  
     return a;
  fi;
  a := 0;
  alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  x := nb_pts_S(1, q);
  y := nb_pts_N1(n - 1, q);
  l := x * y;
  if IsZero(alpha) then
     i := S_rank(q, v, offset + 2 * (n - 1), 1);
     j := N1_rank(q, v, offset, n - 1);
     a := i * y + j;
     return a;
  fi;
  a := a + l;
  gamma2 := evaluate_hyperbolic_quadratic_form(q, v, offset, n - 1);
  x := nb_pts_N1(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;
  if IsZero(gamma2) then
     i := N1_rank(q, v, offset + 2 * (n - 1), 1);
     j := S_rank(q, v, offset, n - 1);
     a := a + i * y + j;
     return a;
  fi;
  a := a + l;
  x := nb_pts_N1(1, q);
  y := q - 2;
  z := nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  j := pos(q,alpha) - 2;
  alpha_inv := alpha^-1;
  v[offset + 2 * (n - 1)] := alpha_inv * v[offset + 2 * (n - 1)];
  i := N1_rank(q, v, offset + 2 * (n - 1), 1);
  gamma2 := evaluate_hyperbolic_quadratic_form(q, v, offset, n - 1);
  gamma_inv := gamma2^-1;
  for u in [0..n-2] do
      v[offset + 2 * u] := gamma_inv * v[offset + 2 * u];
  od;
  k := N1_rank(q, v, offset, n - 1);
  a := a + i * yz + j * z + k;
  return a;
end );

InstallMethod(Sbar_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
  local a, l, i, j, x, y, u, alpha, beta, beta_inv;
  PG_element_normalize(v, offset, 2 * n);
  if n = 1 then
     if IsOne(v[offset + 0]) and IsZero(v[offset + 1]) then
	 a := 0;
	 return a;
     fi;
     if IsZero(v[offset + 0]) and IsOne(v[offset + 1]) then
        a := 1;
        return a;
     fi;
     Print("ERROR in Sbar_rank, n = 1");
  fi;
  a := 0;
  if IsZero(v[offset + 2 * (n - 1)]) and IsZero(v[offset + 2 * (n - 1) + 1]) then
     a := Sbar_rank(q, v, offset, n - 1);
     return a;
  fi;
  l := nb_pts_Sbar(n - 1, q);
  a := a + l;
  alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  x := nb_pts_Sbar(1, q);
  y := nb_pts_S(n - 1, q);
  l := x * y;
  if IsZero(alpha) then
     i := Sbar_rank(q, v, offset + 2 * (n - 1), 1);
     j := S_rank(q, v, offset, n - 1);
     a := a + i * y + j;
     return a;
  fi; 
  a := a + l;
  x := nb_pts_Nbar(1, q);
  y := nb_pts_N1(n - 1, q);
  i := Nbar_rank(q, v, offset + 2 * (n - 1), 1);
  beta := -alpha;
  beta_inv := beta^-1;
  for u in [0..n-2] do
      v[offset + 2 * u] := beta_inv * v[offset + 2 * u];
  od;
  j := N1_rank(q, v, offset, n - 1);
  a := a + i * y + j;
  return a;
end );

InstallMethod(Nbar_rank, [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
    return pos(q,v[offset + 0]) - 1;
end );

InstallMethod(evaluate_hyperbolic_quadratic_form, 
                   [IsPosInt, IsFFECollection, IsPosInt, IsPosInt],
  function(q, v, offset, n)
    local alpha, beta, u;	
    alpha := 0*Z(q);
    for u in [0..n-1] do
	 beta := v[offset + 2 * u] * v[offset + 2 * u + 1];
	 alpha := alpha + beta;
    od;
    return alpha;
end );

####new Anton stuff

InstallMethod(evaluate_hermitian_form, 
                   [IsPosInt, IsFFECollection, IsPosInt, IsInt],
  function(q, v, offset, n)
 # Print("evaluate_hermitian_form q=", q, " v=", v, " offset=", offset, " n=", n, "\n");
    local alpha, beta, u, Q;
	Q := q * q;	
    alpha := 0*Z(Q);
    for u in [0..n-1] do
	 beta := v[offset + u]^(q + 1);
	 alpha := alpha + beta;
    od;
    return alpha;
end );


####


##########################
# The Enumerators *******
##########################

InstallMethod( QElementNumber, [IsPosInt, IsPosInt, IsInt],
  function(d, q, a)

  ## Anton uses the bilinear form (for the 4 dimensional case)
	# 0 1 0 0 0
	# 1 0 0 0 0
	# 0 0 0 1 0
	# 0 0 1 0 0
	# 0 0 0 0 2
  ## Need to change form to canonical. We use the form
	# 2 0 0 0 0
	# 0 0 1 0 0
	# 0 1 0 0 0
	# 0 0 0 0 1
	# 0 0 0 1 0
 ## So we just permute the result by a cycle. Later we should just
 ## change the code so that we do not need this overhead.
 #
 ## Anton's code considers lists which begin at 0, and not at 1.
 ## So we need to make an adjustment for that.


    local n, x, i, one, zero, v, a2, cyc;
    cyc := PermList( Concatenation([2..d+1], [1]));
    one := Z(q)^0;
    zero := 0*Z(q);
    a2 := a - 1;
    v := List([1..d+1], i -> zero);
    n := d / 2;
    x := nb_pts_Sbar(n, q);
    if a2 < x then
       Sbar_unrank(q, v, 1, n, a2);
       return Permuted(v,cyc);
    fi;
    a2 := a2 - x;
    v[1 + 2 * n] := one;
    N1_unrank(q, v, 1, n, a2);
    if IsOddInt(q) then
       for i in [0..n-1] do
	     v[1 + 2 * i] := -v[1 + 2 * i];
       od;
    fi;
    return Permuted(v,cyc);
end );

InstallMethod( QplusElementNumber, [IsPosInt, IsPosInt, IsInt],
  function(d, q, a)
      # The quadratic form here is simply the
      # "sums of hyperbolic pairs" form x1x2 + x3x4 +...
      # In this case, we do not need to permute the
      # result as there is no anisotropic part.

    local n, v, i, zero, a2;
    zero := 0*Z(q);	
    a2 := a - 1;
    v := List([1..d+1], i -> zero);
    n := (d+1) / 2;
    Sbar_unrank(q, v, 1, n, a2);
    return v;
end );


InstallMethod( QminusElementNumber, [IsPosInt, IsPosInt, IsInt],
  function(d, q, a)
    local n, x, i, one, zero, v, w, a2, form, c1, c2, c3,
          b, c, minusz, x1, x2, u, vv, wprim, z, perm;

      # The quadratic form here is 
      # "sums of hyperbolic pairs" form x1x2 + x3x4 +...
      # plus c1 x_{2n}^2 + c2 x_{2n}x_{2n+1} + c3 x_{2n+1}^2
      #
      # The canonical form in FinInG has the anisotropic part
      # of an elliptic quadric in the first two coordinates. Thus
      # we simply apply a double transposition to the coordinates
      # so that Anton's form is the same as ours.

    perm := (1,d)(2,d+1);

    # Choose [c1, c2, c3] compatible with FinInG:
    if IsEvenInt(q) then
       form := QuadraticForm( EllipticQuadric(d, q) )!.matrix;
    else
       form := SesquilinearForm( EllipticQuadric(d, q) )!.matrix;
    fi; 
   
    c1 := form[1][1];
    c2 := form[1][2];
    c3 := form[2][2];

    wprim := Z(q);
    one := wprim^0;
    zero := 0*wprim;
    a2 := a - 1;
    v := List([1..d+1], i -> zero);
    n := (d-1) / 2;
    x := nb_pts_Sbar(n, q); 
    if a2 < x then
       v[1 + 2 * n] := zero;
       v[1 + 2 * n + 1] := zero;
       Sbar_unrank(q, v, 1, n, a2);
       return Permuted(v, perm);
    fi;
    a2 := a2 - x;
    x := nb_pts_N1(n, q); 
    b := div(a2, x);
    c := a2 mod x;
    if IsZero(b) then
       x1 := one;
	 x2 := zero;
    else
       b := b - 1;
       x1 := ffenumber(q, b);
       x2 := one;
    fi;

    v[1 + 2 * n] := x1;
    v[1 + 2 * n + 1] := x2; 
    u := x1 * x1;
    u := c1 * u;
    vv := x1 * x2;
    vv := c2 * vv;
    w := x2 * x2;
    w := c3 * w;
    z := u + vv;
    z := z + w;
    if IsZero(z) then
	 Error("Error in Qminus_unrank: form is not irreducible\n");
    fi;
    N1_unrank(q, v, 1, n, c);
    minusz := -z;
    if not IsOne(minusz) then
       for i in [0..n-1] do
           v[1 + 2 * i] := minusz * v[1 + 2 * i];
       od;
    fi;

    return Permuted(v, perm);
end );

InstallMethod( QNumberElement, [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace],
  function(d, q, var)
    local wittindex, x, a, b, v, i, cyc;
    v := StructuralCopy(var!.obj);
    wittindex := d/2; 
    x := nb_pts_Sbar(wittindex, q);
    if IsZero(v[2 * wittindex + 1]) then
	 a := Sbar_rank(q, v, 1, wittindex);
	 return a;
    fi;
    a := x;
    if not IsOne(v[1 + 2 * wittindex]) then
	 PG_element_normalize(v, 1, d + 1);
    fi;  
    if IsOddInt(q) then
	   for i in [1..wittindex] do
	       v[2 * i] := -v[2 * i];
	   od;
    fi;
    b := N1_rank(q, v, 1, wittindex);
    return a + b + 1; ## adjustment for lists beginning at 1
end );

InstallMethod( QplusNumberElement, [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace],
  function(d, q, var)
    local wittindex, a, v;
    v := StructuralCopy(var!.obj);        
    wittindex := (d+1)/2;
    a := Sbar_rank(q, v, 1, wittindex);
    return a + 1; ## adjustment for lists beginning at 1
end );

## still need to fix QminusNumberElement

InstallMethod( QminusNumberElement, [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace],
  function(d, q, var)
    local wittindex, a, v, x, x1, x2, c1, c2, c3, perm,
          form, i, u, w, vv, z, minusz, minuszv, c, b;
	
    # Choose [c1, c2, c3] compatible with FinInG:
    if IsEvenInt(q) then
       form := QuadraticForm( EllipticQuadric(d, q) )!.matrix;
    else
       form := SesquilinearForm( EllipticQuadric(d, q) )!.matrix;
    fi; 
   
    c1 := form[1][1];
    c2 := form[1][2];
    c3 := form[2][2];

    wittindex := (d-1)/2;

    perm := (1,d)(2,d+1);   ## it would be nice to remove this permutation

    v := StructuralCopy(var!.obj); 
    v := Permuted(v, perm);

    PG_element_normalize(v, 1, d + 1);

    x1 := v[1 + 2 * wittindex];
    x2 := v[2 + 2 * wittindex];

    if IsZero( x1 ) and IsZero( x2 ) then
       a := Sbar_rank(q, v, 1, wittindex);
       return a + 1;  ## adjustment for lists beginning at 1
    fi;

    a := nb_pts_Sbar(wittindex, q);

    if IsOne( x1 ) and IsZero( x2 ) then
	 b := 0;
    else 
	 if not IsOne( x2 ) then
	    Error("ERROR in Qminus_rank x2 <> 1");
	 fi;
	 b := pos(q, x1) + 1;  
    fi;

    x := nb_pts_N1(wittindex, q);
    u := x1 * x1; 
    u := c1 * u;  
    vv := x1 * x2;
    vv := c2 * vv;
    w := x2 * x2; 
    w := c3 * w;  
    z := u + vv;  
    z := z + w;   

    if IsZero( z ) then
       Error("ERROR Qminus_rank z = 0");
    fi;

    minusz := -z; 
    minuszv := minusz^-1; 

    if not IsOne( minusz ) then
       v := minuszv * v;
    fi;

    c := N1_rank(q, v, 1, wittindex);
    return a + b * x + c + 1;  ## adjustment for lists beginning at 1
end );

########
InstallMethod( HermElementNumber, [IsPosInt, IsPosInt, IsInt],
  function(d, q, a)
      # The hermitian form here is simply 
      # x(1) x(2)^q + ... + x(n-1) x(n)^q
	# we need to do something here! 
	local n, v;
    return v;
end );


InstallMethod( HermNumberElement, [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace],
  function(d, q, var)
  return 0;
end );

#########


InstallMethod( AntonEnumerator, [IsSubspacesOfClassicalPolarSpace],

  ## This operation puts together the Anton's code into
  ## an organised form. It computes an enumerator just
  ## for points of a polar space. Thereafter, for higher variety
  ## types, induction by non-degenerate hyperplane sections is
  ## used (except for symplectic spaces).

  function( vs )
    local ps, enum, flavour, d, q, projenum;
    if vs!.type <> 1 then 
       Error("Only applicable to points");
    fi;

    ps := vs!.geometry;
    flavour := PolarSpaceType(ps);
    d := ps!.dimension;
    q := Size(ps!.basefield);   

    if flavour = "parabolic" then

       ## Parabolic

       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, QElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return QNumberElement(d, q, x); 
         end ) 
       );
    elif flavour = "hyperbolic" then

       ## Hyperbolic

       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, QplusElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return QplusNumberElement(d, q, x);
         end ) 
       );
    elif flavour = "elliptic" then

       ## Elliptic

       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, QminusElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return QminusNumberElement(d, q, x); 
         end ) 
       );

    elif flavour = "symplectic" then

       ## Symplectic (uses just the enumerator for the points of 
       ##             the ambient projective space)

       projenum := Enumerator( Points(AmbientSpace(ps)) );
       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, projenum[n]!.obj);
         end,
         NumberElement := function( e, x )
           return Position(projenum, x);
         end ) 
       );
    elif flavour = "hermitian" then

       ## Hermitian 
       
       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, HermElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return HermNumberElement(d, q, x); 
         end ) 
       );
       

    #   projenum := EnumeratorByOrbit( Points( ps ) );  ## this computes the whole list of points!!
    #   enum := EnumeratorByFunctions( vs, rec(
    #     ElementNumber := function( e, n )
    #       return VectorSpaceToElement(ps, projenum[n]!.obj);
    #     end,

    #     NumberElement := function( e, x )
    #       return Position(projenum, x);
    #     end ) 
    #   );
    else
       Error("not implemented for this polar space");
    fi;
    return enum;
  end );

