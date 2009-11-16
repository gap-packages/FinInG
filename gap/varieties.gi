############################################################################
##
##  varieties.gi              Desargues package
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
##  Implementation stuff for schemes
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



InstallMethod( SegreVariety, "given a list of projective spaces", [IsHomogeneousList],
	function(listofpgs)
	
	local sv, var, ty, k,F,l,i,listofdims,cart,eta,dim,d,field,r,indets,cartcart,list1,pollist,ij,ij2,s1,s2,s,polset,newpollist,f;

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
	list1:=List(cartcart,ij->indets[eta(ij[1])]*indets[eta(ij[2])]);
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
	var:=rec( geometry:=PG(dim-1,field), polring:=r, listofpols:=newpollist, inverseimage:=listofpgs, segremap:=SegreMap(listofpgs));
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



### 5. Veronese Varieties ###

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

#InstallMethod ( VeroneseVariety, "given a projective space",
#				[IsProjectiveSpace],
#	function(pg)
#	  local vv, var, ty, 
