

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
         pl := PluckerCoordinates( l!.obj );

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
         invpl := InversePluckerCoordinates( p2!.obj );
         return VectorSpaceToElement(h,invpl);         
       end;
    else
       iso := IsomorphismCanonicalPolarSpace( h );
    
       func := function( l )
         local pl, mu, pos;
         pl := PluckerCoordinates( PreImageElm(iso, l)!.obj );
         pos := PositionNonZero( pl );
         mu := First( AsList(f), m -> m^(q-1) = pl[pos] / pl[7-pos]);      
         pl := mu * pl;
         return VectorSpaceToElement(ps, pl * x)^iso2; 
       end;
 
       pre := function( p )
         local p2, invpl;
         p2 := PreImageElm(iso2, p); 
         p2 := VectorSpaceToElement(pg5, p2!.obj * xinv);
         invpl := InversePluckerCoordinates( p2!.obj );
         return ImageElm(iso, VectorSpaceToElement(h,invpl));         
       end;
    fi;

    map := GeometryMorphismByFunction(Lines(h), Points(eq), func, pre);
    SetIsBijective( map, true );
    return map;
 end );



###################### old version of prefun of NaturalEmbeddingByFieldReduction

    #prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
    #    local flag,basvecs,mat1,span,x,v,v1,i;
    #    flag:=true;
    #    if not subspace in ps2 then
    #        Error("The input is not in the range of the field reduction map!");
    #    fi;
    #    if not IsInt((Dimension(subspace)+1)/t) then
    #        flag:=false;
    #    else
    #        basvecs:=BasisVectors(basis);
    #        mat1:=[];
    #        span:=[];
    #        repeat
    #            repeat
    #                x:=Random(Points(subspace));
    #            until not x in span;
    #            v:=Coordinates(x);
    #            v1:=List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
    #            Add(mat1,v1);
    #            span:=VectorSpaceToElement(ps2,BlownUpMat(basis,mat1));
    #        until Dimension(span)=Dimension(subspace);
    #        if not span = subspace then
    #            flag:= false;
    #        fi;
    #    fi;
    #    if flag= false then
    #        Error("The input is not in the range of the field reduction map!");
    #    fi;
    #    return VectorSpaceToElement(ps1,mat1);
    #end;


		#prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
			#local flag,basvecs,mat1,span,x,v,v1,i;
			#flag:=true;
			#if not subspace in pg2 then
		#		Error("The input is not in the range fo the field reduction map!");
		#	fi;
		#	if not IsInt((Dimension(subspace)+1)/t) then
		#		flag:=false;
		#	else
		#		basvecs:=BasisVectors(basis);
		#		mat1:=[];
		#		span:=[];
		#		repeat
		#			repeat
		#				x:=Random(Points(subspace));
		#			until not x in span;
		#			v:=Coordinates(x);
		#			v1:=List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
		#			Add(mat1,v1);
		#			span:=VectorSpaceToElement(pg2,BlownUpMat(basis,mat1));
		#		until Dimension(span)=Dimension(subspace);
		#		if not span = subspace then
		#			flag:= false;
		#		fi;
		#	fi;
		#	if flag= false then
		#		Error("The input is not in the range of the field reduction map!");
		#	fi;
		#	return VectorSpaceToElement(pg1,mat1);
		#end;

