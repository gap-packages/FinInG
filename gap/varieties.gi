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

#############################################################################
#O  ProjectiveVariety( <pg>, <pring>, <list> )
# constructs a projective variety in a projective space <pg>, with polynomials
# in <list>, all polynomials are in <pring>
##
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


#############################################################################
#O  ProjectiveVariety( <pg>, <pring>, <list> )
# constructs a projective variety in a projective space <pg>, with polynomials
# in <list>
##
InstallMethod( ProjectiveVariety,
	"for a projective space and a list of polynomials",
	[ IsProjectiveSpace, IsList ],
	function( pg, list )
		local pring;
		pring:=PolynomialRing(pg!.basefield,pg!.dimension + 1);
		return ProjectiveVariety(pg,pring,list);
	end );
	
#############################################################################
#O  ProjectiveVariety( <pg>, <pring>, <list> )
# constructs a projective variety in a projective space <pg>, with polynomials
# in <list>
##
InstallMethod( AlgebraicVariety,
	"for a projective space and a list of polynomials",
	[ IsProjectiveSpace, IsList ],
	function( pg, list )
		local pring;
		pring:=PolynomialRing(pg!.basefield,pg!.dimension + 1);
		return ProjectiveVariety(pg,pring,list);
	end );

#############################################################################
# View, print methods for projective varieties.
##

InstallMethod( ViewObj, 
	"for a projective algebraic variety",
	[ IsProjectiveVariety and IsProjectiveVarietyRep ],
	function( var )
		Print("Projective Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a projective algebraic variety",
	[ IsProjectiveVariety and IsProjectiveVarietyRep ],
	function( var )
		Print("Projective Variety in ");
		ViewObj(var!.geometry);
	end );

#############################################################################
#O  DualCoordinatesOfHyperplane( <hyp> )
# returns the dual coordinate of a hyperplane in a projective space.
##
InstallMethod( DualCoordinatesOfHyperplane,
	"for a subspace of a projective space",
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
	
#############################################################################
#O  DualCoordinatesOfHyperplane( <hyp> )
# returns the hyperplanes by given dual coordinates.
##
InstallMethod( HyperplaneByDualCoordinates,
	"for a projective space and a list with coordinates",
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

#############################################################################
#O  AffineVariety( <ag>, <pring>, <list> )
# constructs a projective variety in an affine space <ag>, with polynomials
# in <list>, all polynomials are in <pring>
##
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

#############################################################################
#O  AffineVariety( <ag>, <list> )
# constructs a projective variety in an affine space <ag>, with polynomials
# in <list>.
##
InstallMethod( AffineVariety,
	"for a Affine space and a list of polynomials",
	[ IsAffineSpace, IsList ],
	function( ag, list )
		local pring;
		pring:=PolynomialRing(ag!.basefield,ag!.dimension);
		return AffineVariety(ag,pring,list);
	end );
	
#############################################################################
#O  AffineVariety( <ag>, <list> )
# constructs a projective variety in an affine space <ag>, with polynomials
# in <list>.
##
InstallMethod( AlgebraicVariety,
	"for a Affine space and a list of polynomials",
	[ IsAffineSpace, IsList ],
	function( ag, list )
		local pring;
		pring:=PolynomialRing(ag!.basefield,ag!.dimension);
		return AffineVariety(ag,pring,list);
	end );

#############################################################################
# View, print methods for projective varieties.
##

InstallMethod( ViewObj, 
	"for an affine variety",
	[ IsAffineVariety and IsAffineVarietyRep ],
	function( var )
		Print("Affine Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for an affine variety",
	[ IsAffineVariety and IsAffineVarietyRep ],
	function( var )
		Print("Affine Variety in ");
		ViewObj(var!.geometry);
	end );

### 3. Algebraic Varieties ###

#############################################################################
#O  \in ( <point>, <var> )
# checks if <point> lies on <var>
##
InstallMethod( \in,	
	"for an element of an incidence structure and an algebraic variety", 
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

#############################################################################
#O  PointsOfAlgebraicVariety ( <var> )
# returns a object representing all points of an algebraic variety.
##
InstallMethod( PointsOfAlgebraicVariety, 
	"for an algebraic variety",
	[IsAlgebraicVariety and IsAlgebraicVarietyRep],
	function(var)
		local pts;
		pts:=rec( 
				geometry:=var!.geometry,
				type:=1,
				variety:=var
				);
		return Objectify(
			NewType( ElementsCollFamily,IsPointsOfAlgebraicVariety and
										IsPointsOfAlgebraicVarietyRep),
			pts
			);
	end );

#############################################################################
#O  ViewObj ( <var> )
##
InstallMethod( ViewObj,
	"for a collections of points of an algebraic variety",
	[ IsPointsOfAlgebraicVariety and IsPointsOfAlgebraicVarietyRep ],
	function( pts )
		Print("<points of ",pts!.variety,">");
	end );
	 

#############################################################################
#O  Points ( <var> )
# shortcut to PointsOfAlgebraicVariety
##
InstallMethod( Points, 
	"for an algebraic variety",
	[IsAlgebraicVariety and IsAlgebraicVarietyRep],
	function(var)
		return PointsOfAlgebraicVariety(var);
	end );

#############################################################################
#O  Iterator ( <var> )
# iterator for the points of an algebraic variety.
##
InstallMethod( Iterator, 
	"for points of an algebraic variety", 
	[IsPointsOfAlgebraicVariety],
	function(pts)
		local x;
		return IteratorList(Filtered(Points(pts!.geometry), x->x in pts!.variety));
	end );

#############################################################################
#O  Enumerator( <D> )
# generic method that enumerates D, using an Iterator for D
# assuming D belongs to IsElementsOfIncidenceStructure
##
InstallMethod( Enumerator,
	"generic method for IsPointsOfAlgebraicVariety",
	[IsPointsOfAlgebraicVariety],
	function ( pts )
	local  iter, elms;
	iter := Iterator( pts );
	elms := [  ];
	while not IsDoneIterator( iter )  do
		Add( elms, NextIterator( iter ) );
	od;
	return elms;
	end);

#############################################################################
#O  AmbientSpace( <av> )
# returns the AmbientSpace of <av>
##
InstallMethod( AmbientSpace, 
	"for an algebraic variety",
	[IsAlgebraicVariety and IsAlgebraicVarietyRep],
	function(av)
		return ShallowCopy(av!.geometry);
	end );
	
### 4. Segre Varieties ###

#############################################################################
#O  SegreMap( <listofspaces> ), returns a function the is the Segre Map from 
# a list of projective spaces over the same field in <listofspaces>
##
InstallMethod( SegreMap,
	"for a list of projective spaces",
	[ IsHomogeneousList ],
	function( listofspaces )
    local F, listofdims, l, dim, segremap,map, source, range, ty;
    F := listofspaces[1]!.basefield;
    listofdims := List(listofspaces, i -> ProjectiveDimension(i) + 1);
    for l in listofspaces do
        if l!.basefield <> F then 
           Error("The proj. spaces need to be defined over the same field"); 
        fi;
    od;
  
    dim := Product( listofdims );

	segremap:=function(listofpoints)
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
	
	source:=List(listofspaces,x->Points(x));
	range:=Points(SegreVariety(1,1,3));
	
	map:=rec( source:=source, range:=range, segremap:=segremap );
	ty:=NewType( NewFamily("SegreMapsFamily"), IsSegreMap and 
								IsSegreMapRep );
	Objectify(ty, map);
	return map;

end );

#############################################################################
#O  SegreMap( <listofspaces> ), returns a function the is the Segre Map from 
# a list of integers representing projective dimensions of projective spaces over
# the field <field>
##
InstallMethod( SegreMap, 
	"for a list of projective spaces and a field",
	[IsHomogeneousList, IsField ],
	function(dimlist,field)
	return SegreMap(List(dimlist,n->PG(n,field)));
end );

#############################################################################
#O  SegreMap( <pg1>, <pg2> ), returns the Segre map  from
# two projective spaces over the same field.
##
InstallMethod( SegreMap, 
	"for two projective spaces",
	[IsProjectiveSpace, IsProjectiveSpace ],
	function(pg1,pg2)
	return SegreMap([pg1,pg2]);
end );

#############################################################################
#O  SegreMap( <d1>, <d2>, <field> ), returns the Segre map  from
# two projective spaces of dimension d1 and d2 over the field <field>
##
InstallMethod( SegreMap, 
	"for two positive integers and a field",
	# Note that the given integers are the projective dimensions!
	[ IsPosInt, IsPosInt, IsField ],
	function(d1,d2,field)
	return SegreMap([PG(d1,field),PG(d2,field)]);
end );

#############################################################################
#O  SegreMap( <d1>, <d2>, <field> ), returns the Segre map  from
# two projective spaces of dimension d1 and d2 over the field GF(q)
##
InstallMethod( SegreMap, 
	"given two positive integers and a prime power",
	# Note that the given integers are the projective dimensions!
	[ IsPosInt, IsPosInt, IsPosInt ],
	function(d1,d2,q)
	return SegreMap([PG(d1,GF(q)),PG(d2,GF(q))]);
end );

#############################################################################
#O  SegreVariety( <listofspaces> )
# returns the Segre variety from a list of projective spaces over the same field
##
InstallMethod( SegreVariety, 
	"for a list of projective spaces", 
	[IsHomogeneousList],
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
								inverseimage:=listofpgs );
	ty:=NewType( NewFamily("SegreVarietiesFamily"), IsSegreVariety and 
								IsSegreVarietyRep );
	ObjectifyWithAttributes(var,ty,
			DefiningListOfPolynomials, newpollist);
	return var;
	
end );

#############################################################################
#O  SegreVariety( <dimlist> ), returns the Segre variety  from
# a list of integers representing projective dimensions of projective spaces over
# the field <field>
##
InstallMethod( SegreVariety, 
	"given a list of projective dimensions and a field",
	[IsHomogeneousList, IsField ],
	function(dimlist,field)
	return SegreVariety(List(dimlist,n->PG(n,field)));
end );

#############################################################################
# View, print methods for segre varieties.
##
InstallMethod( ViewObj, 
	"for a Segre variety",
	[ IsSegreVariety and IsSegreVarietyRep ],
	function( var )
		Print("Segre Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a Segre variety",
	[ IsSegreVariety and IsSegreVarietyRep ],
	function( var )
		Print("Segre Variety in ");
		ViewObj(var!.geometry);
	end );
  
#############################################################################
#O  SegreVariety( <pg1>, <pg2> ), returns the Segre variety  from
# two projective spaces over the same field.
##
InstallMethod( SegreVariety, 
	"for two projective spaces",
	[IsProjectiveSpace, IsProjectiveSpace ],
	function(pg1,pg2)
	return SegreVariety([pg1,pg2]);
end );


#############################################################################
#O  SegreVariety( <d1>, <d2> ), returns the Segre variety  from
# two projective spaces of dimension d1 and d2 over the field <field>
##
InstallMethod( SegreVariety, 
	"for two positive integers and a field",
	# Note that the given integers are the projective dimensions!
	[ IsPosInt, IsPosInt, IsField ],
	function(d1,d2,field)
	return SegreVariety([PG(d1,field),PG(d2,field)]);
end );

#############################################################################
#O  SegreVariety( <d1>, <d2> ), returns the Segre variety  from
# two projective spaces of dimension d1 and d2 over the field GF(<q>
##
InstallMethod( SegreVariety, 
	"for two positive integers and a prime power",
	# Note that the given integers are the projective dimensions!
	[ IsPosInt, IsPosInt, IsPosInt ],
	function(d1,d2,q)
	return SegreVariety([PG(d1,q),PG(d2,q)]);
end );

#############################################################################
# View, print methods for segre varieties.
##
InstallMethod( ViewObj, 
	"for a Segre variety",
	[ IsSegreVariety and IsSegreVarietyRep ],
	function( var )
		Print("Segre Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a Segre variety",
	[ IsSegreVariety and IsSegreVarietyRep ],
	function( var )
		Print("Segre Variety in ");
		ViewObj(var!.geometry);
	end );

#############################################################################
#O  SegreMap( <sv> ), returns the Segre map corresponding with <sv>
##
InstallMethod( SegreMap, 
	"for a Segre variety",
	[IsSegreVariety],
	function(sv)
	  return ShallowCopy(sv!.segremap);
	end );


#############################################################################
#O  PointsOfSegreVariety ( <var> )
# returns a object representing all points of a segre variety.
##
InstallMethod( PointsOfSegreVariety, 
	"for a Segre variety",
	[IsSegreVariety and IsSegreVarietyRep],
	function(var)
		local pts;
		pts:=rec( 
				geometry:=var!.geometry,
				type:=1,
				variety:=var
				);
		return Objectify(
			NewType( ElementsCollFamily,IsPointsOfSegreVariety and
										IsPointsOfSegreVarietyRep),
			pts
			);
	end );

#############################################################################
#O  ViewObj ( <var> )
##
InstallMethod( ViewObj, 
	"for a collection representing the points of a Segre variety",
	[ IsPointsOfSegreVariety and IsPointsOfSegreVarietyRep ],
	function( pts )
		Print("<points of ",pts!.variety,">");
  end );
	 
#############################################################################
#O  Points ( <var> )
# shortcut to PointsOfSegreVariety
##
InstallMethod( Points, 
	"for a Segre variety",
	[IsSegreVariety and IsSegreVarietyRep],
	function(var)
		return PointsOfSegreVariety(var);
	end );

#############################################################################
#O  Iterator ( <var> )
# iterator for the points of a segre variety.
##
InstallMethod( Iterator, 
	"for points of an Segre variety", 
	[IsPointsOfSegreVariety],
	function(pts)
		local x,sv,sm,cart,listofpgs,pg,ptlist;
		sv:=pts!.variety;
		sm:=sv!.segremap;
		listofpgs:=sv!.inverseimage;
		cart:=Cartesian(List(listofpgs,pg->Points(pg)));
		ptlist:=List(cart,sm);
		return IteratorList(ptlist);
	end );		

#############################################################################
#O  Enumerator ( <var> )
# Enumerator for the points of a segre variety.
##
InstallMethod( Enumerator,
	"generic method for IsPointsOfSegreVariety",
	[IsPointsOfSegreVariety],
	function ( pts )
		local x,sv,sm,cart,listofpgs,pg,ptlist;
		sv:=pts!.variety;
		sm:=sv!.segremap;
		listofpgs:=sv!.inverseimage;
		cart:=Cartesian(List(listofpgs,pg->Points(pg)));
		ptlist:=List(cart,sm);
		return ptlist;
	end);

### 5. Veronese Varieties ###
# the map in the last section comes from morphism.gi.

#############################################################################
#O  VeroneseMap( <pg> ), returns a function that is the Veronese Map from
#<pg>
##
InstallMethod( VeroneseMap, 
	"for a projective space",
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

#############################################################################
#O  VeroneseVariety( <pg> ), returns the Veronese variety from <pg>
##
InstallMethod ( VeroneseVariety, 
	"for a projective space",
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

#############################################################################
#O  VeroneseVariety( <n>, <field> ), returns the Veronese variety from 
# PG(<n>,<field>).
##
InstallMethod( VeroneseVariety, 
	"for a positive integer and a field",
	# Note that the given integer is the projective dimension
	[ IsPosInt, IsField ],
	function(d1,field)
	return VeroneseVariety(PG(d1,field));
end );

#############################################################################
#O  VeroneseVariety( <n>, <q> ), returns the Veronese variety from 
# PG(<n>,<q>).
##
InstallMethod( VeroneseVariety, 
	"for a positive integer and a prime power",
	# Note that the given integers are the projective dimensions!
	[ IsPosInt, IsPosInt ],
	function(d1,q)
	return VeroneseVariety(PG(d1,q));		
end);

#############################################################################
# view print operations for VeroneseVarieties.
##
InstallMethod( ViewObj, 
	"for a Veronese variety",
	[ IsVeroneseVariety and IsVeroneseVarietyRep ],
	function( var )
		Print("Veronese Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a Veronese variety",
	[ IsVeroneseVariety and IsVeroneseVarietyRep ],
	function( var )
		Print("Veronese Variety in ");
		ViewObj(var!.geometry);
  end );
	
#############################################################################
#O  VeroneseMap( <vv> ), returns a function the is the Veronese Map from
# the Veronese variety <vv>
##
InstallMethod( VeroneseMap, 
	"for a Veronese variety",
	[IsVeroneseVariety],
	function(vv)
	  return ShallowCopy(vv!.veronesemap);
	end );

#############################################################################
#O  PointsOfVeroneseVariety ( <var> )
# returns a object representing all points of a Veronese variety.
##
InstallMethod( PointsOfVeroneseVariety, 
	"for a Veronese variety",
	[IsVeroneseVariety and IsVeroneseVarietyRep],
	function(var)
		local pts;
		pts:=rec( 
				geometry:=var!.geometry,
				type:=1,
				variety:=var
				);
		return Objectify(
			NewType( ElementsCollFamily,IsPointsOfVeroneseVariety and
										IsPointsOfVeroneseVarietyRep),
			pts
			);
	end );

#############################################################################
#O  ViewObj method for points of Veronese variety
##
InstallMethod( ViewObj, 
	"for points of Veronese variety",
	[ IsPointsOfVeroneseVariety and IsPointsOfVeroneseVarietyRep ],
	function( pts )
		Print("<points of ",pts!.variety,">");
	end );
	 
#############################################################################
#O  Points ( <var> )
# shortcut to PointsOfVeroneseVariety
##
InstallMethod( Points,
	"for a Veronese variety",
	[IsVeroneseVariety and IsVeroneseVarietyRep],
	function(var)
		return PointsOfVeroneseVariety(var);
	end );

#############################################################################
#O  Iterator ( <ptsr> )
# Iterator for points of a Veronese variety.
##
InstallMethod( Iterator, 
	"for points of a Veronese variety", 
	[IsPointsOfVeroneseVariety],
	function(pts)
		local vv,vm,pg,ptlist;
		vv:=pts!.variety;
		vm:=vv!.veronesemap;
		pg:=vv!.inverseimage;
		ptlist:=List(Points(pg),vm);
		return IteratorList(ptlist);
	end );		

#############################################################################
#O  Enumerator ( <ptsr> )
# Enumerator for points of a Veronese variety.
##
InstallMethod( Enumerator,
	"for points of a Veronese variety", 
	[IsPointsOfVeroneseVariety],
	function ( pts )
		local vv,vm,pg,ptlist;
		vv:=pts!.variety;
		vm:=vv!.veronesemap;
		pg:=vv!.inverseimage;
		ptlist:=List(Points(pg),vm);
		return ptlist;
	end);


### 6. Miscellaneous ###

#############################################################################
#O  ConicOnFivePoints ( <pts> )
# returns the conic through five given points <pts>, as a projective variety.
##
InstallMethod( ConicOnFivePoints, 
	"for a set of five points of a projective plane",
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


#############################################################################
#O  GrassmannCoordinates ( <sub> )
# returns the Grassmann coordinates of the projective subspace <sub>
##
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

#############################################################################
#O  GrassmannMap ( <k>, <pgdomain> )
# returns the map that maps k-subspaces of <pgdomain> to a point with GrassmannCoordinates  
##
InstallMethod( GrassmannMap, 
	"for an integer and a projective space",
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

#############################################################################
#O  GrassmannMap ( <k>, <n>, <q> )
# shortcut to GrassmannMap(<k>,PG(<n>,<q>))
##
InstallMethod( GrassmannMap, 
	"for three positive integers",
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
#O  PolarSpace ( <var> )
# returns the polar space defined by the equation in the list of polynomials
# of <var>. It is of course checked that this list contains only one equation.
# it is then decided if we try to convert the polynomial to a quadric form or to 
# a hermitian form.
##
InstallMethod( PolarSpace,
	"for a projective algebraic variety",
	[IsProjectiveVariety and IsProjectiveVarietyRep],	
	function(var)
		local list,form,f,eq,r,degree,lm,l;
		list := DefiningListOfPolynomials(var);
		if Length(list) <> 1 then
			Error("<var> does not define a polar space");
		else
			f := BaseField(AmbientSpace(var));
			r := var!.polring;
			eq := list[1];
			lm := LeadingMonomial(eq);
			l := Length(lm)/2;
			degree := Sum(List([1..l],x->lm[2*x]));
			if degree = 2 then
				form := QuadraticFormByPolynomial(eq,r);
			else
				form := HermitianFormByPolynomial(eq,r);
			fi;
			return PolarSpace(form);
		fi;
	end);
	
	
	
	