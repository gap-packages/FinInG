#############################################################################
##
##  varieties.gi              FinInG package
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
##  Implementation stuff for varieties
##
#############################################################################

########################################
#
# Things To Do:
#
# - operations for GrassmannVariety, SegreVariety, VeroneseVariety
#   what does the user need? make example code?
# - groups for GrassmannVariety, SegreVariety
# - put in John's code for "QuadricDefinedByPoints" and "HermitianVarietyDefinedByPoints"
#   perhaps this should be generalised?
# - what are things go in here?
# - testing, documentation
#
########################################

Print(", varieties\c");

#############################################################################
# Constructor methods:
#############################################################################

### 1. Projective Varieties ###

InstallMethod( ProjectiveVariety, 
	"for a projective space, a polynomial ring and a list of homogeneous polynomials",
		[ IsProjectiveSpace, IsPolynomialRing, IsList ],
  function( pg, pring, list )
    local f, extrep, t, x, i, j, var, ty, degrees;
	# The list of polynomials should be non-empty
	if Length(list) < 1 then
		Error("The list of polynomials should be non-empty");
	fi;
	# We check that the number of indeterminates of the polynomial ring is less
	# than the 1 + the dimension of the projective space
	if not Size(IndeterminatesOfPolynomialRing(pring)) = 1+pg!.dimension then
		Error("The dimension of the projective space should be 1 less than the number of indeterminates of the polynomial ring");
	fi;
	# Next we check if the polynomial is homogenious
	for f in list do
		if not f in pring then
			Error("The second argument should be a list of elements of the polynomial ring");
		fi;
		extrep:=ExtRepPolynomialRatFun(f);
		t:=List(Filtered([1..Length(extrep)],i->IsOddInt(i)),j->extrep[j]); 
		degrees:=List(t,x->Sum(List(Filtered([1..Length(x)],i->IsEvenInt(i)),j->x[j])));  
		if not Size(AsSet(degrees)) = 1 then
			Error("The second argument should be a list of homogeneous polynomials");
		fi;
	od;
	var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
	ty:=NewType( NewFamily("ProjectiveVarietiesFamily"), IsProjectiveVariety and 
								IsProjectiveVarietyRep );
	ObjectifyWithAttributes(var,ty,
			#AmbientGeometry, pg, 
			#PolynomialRing, pring,
			DefiningListOfPolynomials, list);
	return var;
end );

InstallMethod( ProjectiveVariety,
	"for a projective space and a list of polynomials",
		[ IsProjectiveSpace, IsList ],
	function( pg, list )
		local pring;
		pring:=PolynomialRing(pg!.basefield,pg!.dimension + 1);
		return ProjectiveVariety(pg,pring,list);
	end );
	
InstallMethod( AlgebraicVariety,
	"for a projective space and a list of polynomials",
		[ IsProjectiveSpace, IsList ],
	function( pg, list )
		local pring;
		pring:=PolynomialRing(pg!.basefield,pg!.dimension + 1);
		return ProjectiveVariety(pg,pring,list);
	end );

InstallMethod( ViewObj, [ IsProjectiveVariety and 
                           IsProjectiveVarietyRep ],
  function( var )
    Print("Projective Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( PrintObj, [ IsProjectiveVariety and 
                           IsProjectiveVarietyRep ],
  function( var )
    Print("Projective Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( DualCoordinatesOfHyperplane, 
		[IsSubspaceOfProjectiveSpace],
	function(hyp)
		local mat,a;
		if not Dimension(hyp)=Dimension(hyp!.geo)-1 then
			Error("The argument is not a hyperplane");
		else
			mat:=hyp!.obj;
			a:=NullspaceMat(TransposedMat(mat));
		return a[1];
		fi;
	end );
	
InstallMethod( HyperplaneByDualCoordinates,
		[IsProjectiveSpace,IsList],
	function(pg,a)
		local mat,list;
		if not Size(a)=Dimension(pg)+1 then
			Error("The dual coordinates are not compatible with the projective space");
		else
			mat:=[a];
			list:=NullspaceMat(TransposedMat(mat));
			return VectorSpaceToElement(pg,list);
		fi;
	end );

### 2. Affine Varieties ###

InstallMethod( AffineVariety, 
	"for a Affine space, a polynomial ring and a list of homogeneous polynomials",
		[ IsAffineSpace, IsPolynomialRing, IsList ],
  function( ag, pring, list )
    local f, extrep, t, x, i, j, var, ty, degrees;
	# The list of polynomials should be non-empty
	if Length(list) < 1 then
		Error("The list of polynomials should not be empty");
	fi;
	# We check that the number of indeterminates of the polynomial ring is less
	# than the 1 + the dimension of the Affine space
	if not Size(IndeterminatesOfPolynomialRing(pring)) = ag!.dimension then
		Error("The dimension of the Affine space should be equal to the number of indeterminates of the polynomial ring");
	fi;
	var:=rec( geometry:=ag, polring:=pring, listofpols:=list);
	ty:=NewType( NewFamily("AffineVarietiesFamily"), IsAffineVariety and 
								IsAffineVarietyRep );
	ObjectifyWithAttributes(var,ty,
			#AmbientGeometry, ag, 
			#PolynomialRing, pring,
			DefiningListOfPolynomials, list);
	return var;
end );

InstallMethod( AffineVariety,
	"for a Affine space and a list of polynomials",
		[ IsAffineSpace, IsList ],
	function( ag, list )
		local pring;
		pring:=PolynomialRing(ag!.basefield,ag!.dimension);
		return AffineVariety(ag,pring,list);
	end );
	
InstallMethod( AlgebraicVariety,
	"for a Affine space and a list of polynomials",
		[ IsAffineSpace, IsList ],
	function( ag, list )
		local pring;
		pring:=PolynomialRing(ag!.basefield,ag!.dimension);
		return AffineVariety(ag,pring,list);
	end );

InstallMethod( ViewObj, [ IsAffineVariety and 
                           IsAffineVarietyRep ],
  function( var )
    Print("Affine Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( PrintObj, [ IsAffineVariety and 
                           IsAffineVarietyRep ],
  function( var )
    Print("Affine Variety in ");
	ViewObj(var!.geometry);
  end );



### 3. Algebraic Varieties ###


InstallMethod( \in, "for a point and an algebraic variety", 
	[IsElementOfIncidenceStructure, IsAlgebraicVariety],
	function(point,var)
		local w, pollist, i, n, test, f, nrindets, pring;
		# The point has to be a point of the ambient geometry of the variety 
		if not point in var!.geometry then
			Error("The point should belong to the ambient geometry of the variety");
		fi;
		# The coordinate vector of the point should vanish at each polynomial in
		# the list of defining polynomials of the variety
		w:=point!.obj;
		pollist:=var!.listofpols;
		pring:=var!.polring;
		nrindets:=Size(IndeterminatesOfPolynomialRing(pring));
		n:=Length(pollist);
		i:=0;
		test:=true;
		while i<n and test=true do
			i:=i+1;
			f:=pollist[i];
			if not IsZero(Value(f,[1..nrindets],w)) then 
				test:=false;
			fi;
		od;
		return test;
	end );

InstallMethod( PointsOfAlgebraicVariety, "for an algebraic variety",
	[IsAlgebraicVariety],
	function(var)
		local pts;
		pts:=rec( 
				geometry:=var!.geometry,
				type:=1,
				variety:=var
				);
		return Objectify(
			NewType( ElementsCollFamily,IsAllPointsOfAlgebraicVariety and
										IsAllPointsOfAlgebraicVarietyRep),
			pts
			);
	end );

InstallMethod( ViewObj, [ IsAllPointsOfAlgebraicVariety and 
                           IsAllPointsOfAlgebraicVarietyRep ],
  function( pts )
    Print("<points of ",pts!.variety,">");
  end );
	 
InstallMethod( Points, "for an algebraic variety",
	[IsAlgebraicVariety],
	function(var)
		return PointsOfAlgebraicVariety(var);
	end );

InstallMethod( Iterator, "for points of an algebraic variety", 
	[IsAllPointsOfAlgebraicVariety],
	function(pts)
		local x;
		return IteratorList(Filtered(Points(pts!.geometry), x->x in pts!.variety));
	end );

InstallMethod( AmbientSpace, "for an algebraic variety",
	[IsAlgebraicVariety],
	function(av)
		return ShallowCopy(av!.geometry);
	end );
	
### 4. Segre Varieties ###

InstallMethod( SegreMap, "given a list of projective spaces",
                         [ IsHomogeneousList ],
  function( listofspaces )
    local F, listofdims, l, dim, func;
    F := listofspaces[1]!.basefield;
    listofdims := List(listofspaces, i -> ProjectiveDimension(i) + 1);
    for l in listofspaces do
        if l!.basefield <> F then 
           Error("The proj. spaces need to be defined over the same field"); 
        fi;
    od;
  
    dim := Product( listofdims );

	func:=function(listofpoints)
	#Takes k points of k projective spaces defined over the same basefield
	#to a point of a projective space of dimension (n_1+1)...(n_k+1), 
	#where n_i is the dimension of the i-th projective space
	local listofdims,F,k,list,vector,l,llist,i,dim,cart;
	if not Size(AsSet(List(listofpoints,p->Size(p!.geo!.basefield))))=1 then 
		Error("The points need to be defined over the same field \n"); 
	else
		listofdims:=List(listofpoints,p->Size(p!.obj));
		F:=listofpoints[1]!.geo!.basefield;
		k:=Size(listofpoints);
		cart:=Cartesian(List(listofdims,d->[1..d]));
		vector:=[];
		for l in cart do
			llist:=[];
			for i in [1..k] do
				Append(llist,[listofpoints[i]!.obj[l[i]]]);
			od;
			Append(vector,[Product(llist)]);
		od;
		dim:=Product(listofdims);
		return  VectorSpaceToElement(ProjectiveSpace(dim-1,F),vector);
	fi;
	end;
	return func;
end );

InstallMethod( SegreMap, "given a list of projective spaces",
		[IsHomogeneousList, IsField ],
	function(dimlist,field)
			
	local listofpgs,d;
	
	listofpgs:=List(dimlist,d->PG(d,field));
	return SegreMap(listofpgs);
end );


InstallMethod( SegreVariety, "given a list of projective spaces", [IsHomogeneousList],
	function(listofpgs)
	
	local sv, var, ty, k, F, l, i, listofdims, cart, eta, dim, d, field, r, indets, cartcart, list1, 
										pollist, ij, ij2, s1, s2, s, polset, newpollist, f;

    F := listofpgs[1]!.basefield;
    listofdims := List(listofpgs, i -> ProjectiveDimension(i) + 1);
    for l in listofpgs do
        if l!.basefield <> F then 
           Error("The proj. spaces need to be defined over the same field"); 
        fi;
    od;

	k:=Size(listofpgs);
	listofdims:=List(listofpgs,x->ProjectiveDimension(x)+1);
	cart:=Cartesian(List(listofdims,d->[1..d]));
	eta:=t->Position(cart,t);
	dim:=Product(listofdims);
	field:=listofpgs[1]!.basefield;
	r:=PolynomialRing(field,dim);
	indets:=IndeterminatesOfPolynomialRing(r);
	cartcart:=Cartesian([cart,cart]);
	#list1:=List(cartcart,ij->indets[eta(ij[1])]*indets[eta(ij[2])]);
	pollist:=[];
	for ij in cartcart do
		for s in [1..k] do
			ij2:=StructuralCopy(ij);
			s1:=ij2[1][s];
			s2:=ij2[2][s];
			ij2[1][s]:=s2;
			ij2[2][s]:=s1;		
			Add(pollist,indets[eta(ij[1])]*indets[eta(ij[2])]-indets[eta(ij2[1])]*indets[eta(ij2[2])]);
		od;
	od;
	polset:=AsSet(Filtered(pollist,x-> not x = Zero(r)));
	newpollist:=[];
	for f in polset do
		if not -f in newpollist then
			Add(newpollist,f);
		fi;
	od;

	sv:=ProjectiveVariety(PG(dim-1,field),r,newpollist);
	var:=rec( geometry:=PG(dim-1,field), polring:=r, listofpols:=newpollist, 
								inverseimage:=listofpgs, segremap:=SegreMap(listofpgs));
	ty:=NewType( NewFamily("SegreVarietiesFamily"), IsSegreVariety and 
								IsSegreVarietyRep );
	ObjectifyWithAttributes(var,ty,
			#AmbientGeometry, ag, 
			#PolynomialRing, pring,
			DefiningListOfPolynomials, newpollist);
	return var;
	
end );

InstallMethod( SegreVariety, "given a list of projective dimensions and a field",
		[IsHomogeneousList, IsField ],
	function(dimlist,field)
			
	local listofpgs,d;
	
	listofpgs:=List(dimlist,d->PG(d,field));
	return SegreVariety(listofpgs);
end );

InstallMethod( ViewObj, [ IsSegreVariety and 
                           IsSegreVarietyRep ],
  function( var )
    Print("Segre Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( PrintObj, [ IsSegreVariety and 
                           IsSegreVarietyRep ],
  function( var )
    Print("Segre Variety in ");
	ViewObj(var!.geometry);
  end );
  

InstallMethod( SegreVariety, "given two projective spaces",
		[IsProjectiveSpace, IsProjectiveSpace ],
	function(pg1,pg2)
			
	local listofpgs;
	
	listofpgs:=[pg1,pg2];
	return SegreVariety(listofpgs);
end );

InstallMethod( SegreVariety, "given two pos integers and a field",
	# Note that the given integers are the projective dimensions!
		[ IsPosInt, IsPosInt, IsField ],
	function(d1,d2,field)
			
	local listofpgs;
	
	listofpgs:=[PG(d1,field),PG(d2,field)];
	return SegreVariety(listofpgs);
end );


InstallMethod( SegreVariety, "given two pos integers and a prime power",
	# Note that the given integers are the projective dimensions!
		[ IsPosInt, IsPosInt, IsPosInt ],
	function(d1,d2,q)
			
	local listofpgs;
	
	listofpgs:=[PG(d1,GF(q)),PG(d2,GF(q))];
	return SegreVariety(listofpgs);
end );

InstallMethod( SegreMap, "given two projective spaces",
		[IsProjectiveSpace, IsProjectiveSpace ],
	function(pg1,pg2)
			
	local listofpgs;
	
	listofpgs:=[pg1,pg2];
	return SegreMap(listofpgs);
end );

InstallMethod( SegreMap, "given two pos integers and a field",
	# Note that the given integers are the projective dimensions!
		[ IsPosInt, IsPosInt, IsField ],
	function(d1,d2,field)
			
	local listofpgs;
	
	listofpgs:=[PG(d1,field),PG(d2,field)];
	return SegreMap(listofpgs);
end );

InstallMethod( SegreMap, "given two pos integers and a prime power",
	# Note that the given integers are the projective dimensions!
		[ IsPosInt, IsPosInt, IsPosInt ],
	function(d1,d2,q)
			
	local listofpgs;
	
	listofpgs:=[PG(d1,GF(q)),PG(d2,GF(q))];
	return SegreMap(listofpgs);
end );

InstallMethod( ViewObj, [ IsSegreVariety and 
                           IsSegreVarietyRep ],
  function( var )
    Print("Segre Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( PrintObj, [ IsSegreVariety and 
                           IsSegreVarietyRep ],
  function( var )
    Print("Segre Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( SegreMap, "given a Segre variety",
	[IsSegreVariety],
	function(sv)
	  return ShallowCopy(sv!.segremap);
	end );


InstallMethod( PointsOfSegreVariety, "for a Segre variety",
			[IsSegreVariety],
		# returns a list of the points of a Segre variety using the Segre map
	function(sv)
		local sm,cart,listofpgs,pg,pts;
		sm:=sv!.segremap;
		listofpgs:=sv!.inverseimage;
		cart:=Cartesian(List(listofpgs,pg->Points(pg)));
		pts:=List(cart,sm);
		return pts;
	end );

# IteratorOfPointsOfSegreVariety
# FamiliesOfMaximalSubspacesOfSegreVariety
		

### 5. Veronese Varieties ###
# the map in the last section comes from morphism.gi.

InstallMethod( VeroneseMap, "given a projective space",
				[IsProjectiveSpace],
	function(pg)
	  local F, dim, func;
	  F:=pg!.basefield;
	  dim:=pg!.dimension;
	  func:=function(point)
	    # takes a point and maps it to its image under the veronese map
		local i,j,list,n,F;
		n:=Size(point!.obj);
		F:=point!.geo!.basefield;
		list:=[];
		for i in [1..n] do
          for j in [i..n] do
            Append(list,[point!.obj[i]*point!.obj[j]]);
		  od;
        od;
        return VectorSpaceToElement(ProjectiveSpace((n-1)*(n+2)/2,F),list);
      end;
	return func;
end );

InstallMethod ( VeroneseVariety, "given a projective space",
				[IsProjectiveSpace],
	function(pg)
	  local field,r,n2,indets,list,i,j,k,s,vv,var,ty,n;

	field:=pg!.basefield;
	n:=Dimension(pg)+1;
	n2:=Int(n*(n+1)/2);
	r:=PolynomialRing(field,n2);
	indets:=IndeterminatesOfPolynomialRing(r);
	list:=[];
	for i in [1..n-1] do 
		for j in [i+1..n] do
			Add(list,indets[(i-1)*n-Int((i^2-i)/2)+j]^2
			-indets[(i-1)*n-Int((i^2-i)/2)+i]*indets[(j-1)*n-Int((j^2-j)/2)+j]);
		od;
	od;
	for i in [1..n-2] do 
		for j in [i+1..n-1] do 
			for k in [j+1..n] do
				Add(list,indets[(i-1)*n-Int((i^2-i)/2)+i]*indets[(j-1)*n-Int((j^2-j)/2)+k]
				-indets[(i-1)*n-Int((i^2-i)/2)+j]*indets[(i-1)*n-Int((i^2-i)/2)+k]);
			od;
		od;
	od;
	vv:=ProjectiveVariety(PG(n2-1,field),r,list);
	var:=rec( geometry:=PG(n2-1,field), polring:=r, listofpols:=list, 
								inverseimage:=pg, veronesemap:=VeroneseMap(pg));
	ty:=NewType( NewFamily("VeroneseVarietiesFamily"), IsVeroneseVariety and 
								IsVeroneseVarietyRep );
	ObjectifyWithAttributes(var,ty,
			#AmbientGeometry, ag, 
			#PolynomialRing, pring,
			DefiningListOfPolynomials, list);
	return var;
end );

InstallMethod( VeroneseVariety, "given a positive integer and a field",
	# Note that the given integer is the projective dimension
		[ IsPosInt, IsField ],
	function(d1,field)
			
	local pg;
	
	pg:=PG(d1,field);
	return VeroneseVariety(pg);
end );


InstallMethod( VeroneseVariety, "given a positive integer and a prime power",
	# Note that the given integers are the projective dimensions!
		[ IsPosInt, IsPosInt ],
	function(d1,q)
			
	local pg;
	
	pg:=PG(d1,GF(q));
	return VeroneseVariety(pg);
end );

InstallMethod( ViewObj, [ IsVeroneseVariety and 
                           IsVeroneseVarietyRep ],
  function( var )
    Print("Veronese Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( PrintObj, [ IsVeroneseVariety and 
                           IsVeroneseVarietyRep ],
  function( var )
    Print("Veronese Variety in ");
	ViewObj(var!.geometry);
  end );

InstallMethod( VeroneseMap, "given a Veronese variety",
	[IsVeroneseVariety],
	function(vv)
	  return ShallowCopy(vv!.veronesemap);
	end );
	
InstallMethod( PointsOfVeroneseVariety, "for a Veronese variety",
			[IsVeroneseVariety],
		# returns a list of the points of a Segre variety using the Segre map
	function(vv)
		local vm,cart,listofpgs,pg,pts;
		vm:=vv!.veronesemap;
		pg:=vv!.inverseimage;
		pts:=List(Points(pg),vm);
		return pts;
	end );


### 6. Miscellaneous ###


InstallMethod( ConicOnFivePoints, "given a set of five points of a projective plane",
   [ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection ],
 
  function( pts )

  #  To find the conic, we simply solve an equation
  #
  #  ax^2 + by^2 + cz^2 + dxy + exz + fyz = 0
  #  [x^2,y^2,z^2,xy,xz,yz] . [a,b,c,d,e] = 0
  #
  #  This function returns a projective algebraic variety

    local gf, r, vecs, mat, sol, poly, mat2, plane, embed,
          pg, d, dplus1, pairs, vars, indets;
    if Size(pts) < 5 then
       Error("Not enough points");
    fi;

    if ForAny(pts, t -> ProjectiveDimension(t) <> 0) then
       Error("Not a set of points");
    fi;

    ## check that the points span a plane

    if Rank( List(pts, t -> t!.obj) ) <> 3 then
       Error("Points do not span a plane");
    fi;

    pg := AmbientSpace(pts[1]!.geo);
    gf := pg!.basefield;
    dplus1 := Dimension(pg) + 1;
    r := PolynomialRing(gf, dplus1);
    indets := IndeterminatesOfPolynomialRing(r);
    vecs := List(pts, t -> Unwrap(t));
    pairs := UnorderedTuples( [1..dplus1], 2 );;
    mat := List(vecs, t -> List( pairs, p -> t[p[1]] * t[p[2]] ) );;
    sol := NullspaceMat(TransposedMat(mat))[1];
	vars := List(pairs, p -> indets[p[1]] * indets[p[2]]);
    poly := vars * sol;

    return ProjectiveVariety( pg, [poly] );
  end ); 


### morphism suff.

#InstallMethod( VeroneseMap, "given a projective space PG(n,q)",
#    [ IsProjectiveSpace ],
#  function( pgdomain )
#    local n,F,n2,pgimage,varmap,func,
#          tups,beta,betainv,hom,
#          g1,g2,twiner,gens,newgens;
#    n := pgdomain!.dimension + 1;
#    F := pgdomain!.basefield;
#    n2 := (n-1)*(n+2)/2;
#    pgimage := VeroneseVariety(n2, F);##

#    func := function( point )
#      local i,j,list;
#      list:=[];
#      for i in [1..n] do
#        for j in [i..n] do
#          Add(list, point!.obj[i]*point!.obj[j] );
#        od;
#      od;
#      ConvertToVectorRepNC( list, F );
#      return Wrap(pgimage, 1, list);
#    end;#

#    tups := Filtered(Tuples([1..n], 2),i->i[2]>=i[1]);#

#    beta := function( m )
#      local rows;
#      rows := List([1..n], i -> m[i]{[i..n]});
#      return Concatenation(rows);
#    end;

#    betainv := function( v )
#      local matb, i, j, x;
#      matb := ShallowCopy( NullMat(n, n, F) );
#          for i in [1..n] do
#              for j in [i..n] do
#                  x := v[Position(tups,[i,j])];
#                  matb[i][j] := x;
#                  matb[j][i] := x;
#              od;
#          od;
#      return matb;
#    end;
      
#    hom := function( m )
#      local basis1, basis2, image, mat;
#      mat := m!.mat;
#      basis1 := IdentityMat(n2+1, F);
#      basis2 := List(basis1, betainv);
#      image := List(basis2, b -> beta( TransposedMat(mat) * b * mat ));  
#      ConvertToMatrixRepNC( image, F );       
#      return ProjElWithFrob(image, IdentityMapping(F), F);
#    end;
#   
#    g1 := HomographyGroup( pgdomain );
#    gens := GeneratorsOfGroup( g1 );
#    newgens := List(gens, hom);
#    g2 := Group( newgens );
#   SetSize(g2, Size(g1));
#    twiner := GroupHomomorphismByImagesNC(g1, g2, gens, newgens);
#   SetIsBijective(twiner, true);#
#    varmap := GeometryMorphismByFunction(Points(pgdomain), Points(pgimage), func);
#    SetIsInjective( varmap, true );
#    SetIntertwiner(varmap, twiner);
#    return varmap;
#  end );

#InstallMethod( VeroneseMap, "given a dimension and field",
#    [ IsPosInt, IsField ],
#  function( d, F )
#    return VeroneseMap( ProjectiveSpace(d, F) );
#  end );

#InstallMethod( VeroneseMap, "given a dimension and field order",
#    [ IsPosInt, IsPosInt ],
#  function( d, q )
#    return VeroneseMap( ProjectiveSpace(d, q) );
#  end );


InstallMethod( GrassmannCoordinates, 
	"for a subspace of a projective space",
    [ IsSubspaceOfProjectiveSpace ],

  ## Warning: this operation is not compatible with
  ## PluckerCoordinates. To get the same image, you
  ## need to multiply the fifth coordinate by -1.
	function( sub )
    local basis,k,n,list,vector;
    k := ProjectiveDimension(sub);
	n := ProjectiveDimension(sub!.geo);
	if (k <= 0  or k >= n-1) then 
         Error("The dimension of the subspace has to be at least 1 and at most ", n-2);
    fi;
	basis := sub!.obj;
    list := TransposedMat(basis); 
    vector := List(Combinations([1..n+1], k+1), i -> DeterminantMat( list{i} ));  
    return vector;
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




