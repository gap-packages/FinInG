#############################################################################
##
##  varieties.gi              FinInG package
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
##  Implementation stuff for varieties
##
#############################################################################

#############################################################################
# Constructor methods:
#############################################################################


#############################################################################
#############################################################################
#############################################################################
### 0. Algebraic Varieties (generic methods) ###
#############################################################################
#############################################################################
#############################################################################


#############################################################################
#O  AlgebraicVariety( <pg>, <pring>, <list> )
# constructs a projective variety in a projective space <pg>, with polynomials
# in <list>
##

InstallMethod( AlgebraicVariety,
	"for a projective space, a polynomial ring and a list of polynomials",
	[ IsProjectiveSpace, IsPolynomialRing, IsList ],
	function( pg, pring, list )
		return ProjectiveVariety(pg,pring,list);
	end );

#############################################################################
#O  AlgebraicVariety( <pg>, <list> )
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
#O  AlgebraicVariety( <ag>, <pring>, <list> )
# constructs an affine variety in an affine space <ag>, with polynomials
# in <list>
##

InstallMethod( AlgebraicVariety,
	"for an affine space, a polynomial ring and a list of polynomials",
	[ IsAffineSpace, IsPolynomialRing, IsList ],
	function( pg, pring, list )
		return AffineVariety(pg,pring,list);
	end );
	
#############################################################################
#O  AlgebraicVariety( <ag>, <list> )
# constructs an affine variety in an affine space <ag>, with polynomials
# in <list>
##
	
InstallMethod( AlgebraicVariety,	
	"for an affine space and a list of polynomials",
	[ IsAffineSpace, IsList ],
	function( ag, list )
		local pring;
		pring:=PolynomialRing(ag!.basefield, ag!.dimension);
		return AffineVariety(ag, pring, list);
	end );


#############################################################################
#############################################################################
#############################################################################
### 1. Projective Varieties ###
#############################################################################
#############################################################################
#############################################################################


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
#O  ProjectiveVariety( <pg>, <list> )
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
		PrintObj(var!.geometry);
	end );

InstallMethod( Display, 
	"for a projective algebraic variety",
	[ IsProjectiveVariety and IsProjectiveVarietyRep ],
	function( var )
		Print("Projective Variety in ");
		ViewObj(var!.geometry);
		Print("\n Defining list of polynomials: ", DefiningListOfPolynomials(var),"\n");
	end );

#############################################################################
#O HermitianVariety( <n>, <fld>);
# returns a nondegenerate hermitian variety in PG(n,fld)
InstallMethod( HermitianVariety,
	"for a positive integer and a field",
	[IsPosInt, IsField],
	function(n,fld)
		local pg, pring, list, hf, var, ty;
		pg:=PG(n,fld);
		pring:=PolynomialRing(fld,n+1);
		list:=[EquationForPolarSpace(HermitianPolarSpace(n,fld))];
		hf:=SesquilinearForm(HermitianPolarSpace(n,fld));
		
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("HermitianVarietiesFamily"), IsHermitianVariety and 
									IsHermitianVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, SesquilinearForm, hf, IsStandardHermitianVariety, true);
		return var;
	end );

#############################################################################
#O HermitianVariety( <n>, <q>);
# returns a nondegenerate hermitian variety in PG(n,q)
InstallMethod( HermitianVariety,
	"for a positive integer and a prime power",
	[IsPosInt, IsPosInt],
	function(n,q)
		local fld;
		fld:=GF(q);
		return HermitianVariety(n,fld);
	end );

#############################################################################
#O HermitianVariety( <pg>,<pring>,<pol>);
# returns a hermitian variety in pg
InstallMethod( HermitianVariety,
	"for a projective space, a polynomial ring and a polynomial",
	[IsProjectiveSpace,IsPolynomialRing, IsPolynomial],
	function(pg,pring,pol)
		local list,hf,var,ty;
		hf:=HermitianFormByPolynomial(pol,pring);
		list:=[pol];
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("HermitianVarietiesFamily"), IsHermitianVariety and 
									IsHermitianVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, SesquilinearForm, hf, IsStandardHermitianVariety, false);
		return var;
	end );

#############################################################################
#O HermitianVariety( <pg>,<pol>);
# returns a hermitian variety in pg
InstallMethod( HermitianVariety,
	"for a projective space and a polynomial",
	[IsProjectiveSpace, IsPolynomial],
	function(pg, pol)
		local list,pring,hf,var,ty;
		pring:=PolynomialRing(pg!.basefield, pg!.dimension +1);
		hf:=HermitianFormByPolynomial(pol,pring);
		list:=[pol];
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("HermitianVarietiesFamily"), IsHermitianVariety and 
									IsHermitianVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, SesquilinearForm, hf, IsStandardHermitianVariety, false);
		return var;
	end );


#############################################################################
# View, print methods for hermitian varieties.
##

InstallMethod( ViewObj, 
	"for a hermitian variety",
	[ IsHermitianVariety and IsHermitianVarietyRep ],
	function( var )
		Print("Hermitian Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a hermitian variety",
	[ IsHermitianVariety and IsHermitianVarietyRep ],
	function( var )
		Print("Hermitian Variety in ");
		PrintObj(var!.geometry);
	end );

InstallMethod( Display, 
	"for a hermitian variety",
	[ IsHermitianVariety and IsHermitianVarietyRep ],
	function( var )
		Print("Hermitian Variety in ");
		ViewObj(var!.geometry);
		Print("\n Polynomial: ", DefiningListOfPolynomials(var),"\n");
	end );


#######################################################################
#O QuadraticVariety( <pg>,<pring>,<pol>);
# returns a quadratic variety in pg
InstallMethod( QuadraticVariety,
	"for a projective space, a polynomial ring and a polynomial",
	[IsProjectiveSpace,IsPolynomialRing, IsPolynomial],
	function(pg,pring,pol)
		local qf,list,var,ty;
		qf:=QuadraticFormByPolynomial(pol,pring);
		list:=[pol];
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("QuadraticVarietiesFamily"), IsQuadraticVariety and 
									IsQuadraticVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, QuadraticForm, qf);
		return var;
	end );

#######################################################################
#O QuadraticVariety( <pg>,<pol>);
# returns a quadratic variety in pg
InstallMethod( QuadraticVariety,
	"for a projective space, a polynomial ring and a polynomial",
	[IsProjectiveSpace, IsPolynomial],
	function(pg,pol)
		local qf,pring,list,var,ty;
		pring:=PolynomialRing(pg!.basefield, pg!.dimension +1);
		qf:=QuadraticFormByPolynomial(pol,pring);
		list:=[pol];
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("QuadraticVarietiesFamily"), IsQuadraticVariety and 
									IsQuadraticVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, QuadraticForm, qf);
		return var;
	end );

#############################################################################
#O QuadraticVariety( <n>, <fld>, <type>);
# returns a nondegenerate quadratic variety in PG(n,fld), of a specified type
# when n is odd.
InstallMethod( QuadraticVariety,
	"for a positive integer and a field and a string",
	[IsPosInt, IsField, IsString],
	function(n,fld,type)
		local pg, pring, list, qf, var, ty, ps;
		pg:=PG(n,fld);
		pring:=PolynomialRing(fld,n+1);
		if type = "o" or type = "parabolic" or type ="0" then
			ps := ParabolicQuadric(n,fld);
			if IsOddInt(n) then
				Error("Dimension and type are incompatible.");
			fi;
		elif type = "+" or type = "hyperbolic" or type = "1" then
			ps := HyperbolicQuadric(n,fld);
			if IsEvenInt(n) then
				Error("Dimension and type are incompatible.");
			fi;
		elif  type = "-" or type = "elliptic" or type = "-1" then
			ps := EllipticQuadric(n,fld);
			if IsEvenInt(n) then
				Error("Dimension and type are incompatible.");
			fi;
		fi;
		list:=[EquationForPolarSpace(ps)];
		qf:=SesquilinearForm(ps);
		
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("QuadraticVarietiesFamily"), IsQuadraticVariety and 
									IsQuadraticVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, SesquilinearForm, qf, IsStandardQuadraticVariety, true);
		return var;
	end );


#############################################################################
#O QuadraticVariety( <n>, <fld>);
# returns a nondegenerate quadratic variety in PG(n,fld), where
# we assume here that n is even.
InstallMethod( QuadraticVariety,
	"for an even positive integer and a field",
	[IsPosInt, IsField],
	function(n,fld)
		local pg, pring, list, qf, var, ty, ps;
		if not IsEvenInt(n) then
			Error("The dimension must be even.");
		fi;
		pg:=PG(n,fld);
		pring:=PolynomialRing(fld,n+1);
		ps := ParabolicQuadric(n,fld);
		list:=[EquationForPolarSpace(ps)];
		qf:=SesquilinearForm(ps);
		
		var:=rec( geometry:=pg, polring:=pring, listofpols:=list);
		ty:=NewType( NewFamily("QuadraticVarietiesFamily"), IsQuadraticVariety and 
									IsQuadraticVarietyRep );
		ObjectifyWithAttributes(var,ty,
				DefiningListOfPolynomials, list, SesquilinearForm, qf, IsStandardQuadraticVariety, true);
		return var;
	end );

#
#############################################################################
#O QuadraticVariety( <n>, <q>);
#O QuadraticVariety( <n>, <q>, <type>);
# returns a nondegenerate quadratic variety in PG(n,q)
InstallMethod( QuadraticVariety,
	"for a positive integer and a prime power",
	[IsPosInt, IsPosInt],
	function(n,q)
		return QuadraticVariety(n,GF(q));
	end );

InstallMethod( QuadraticVariety,
	"for a positive integer and a prime power and a string",
	[IsPosInt, IsPosInt, IsString],
	function(n,q,type)
		return QuadraticVariety(n,GF(q),type);
	end );
	
#############################################################################
# View, print methods for quadratic varieties.
##

InstallMethod( ViewObj, 
	"for a quadratic variety",
	[ IsQuadraticVariety and IsQuadraticVarietyRep ],
	function( var )
		Print("Quadratic Variety in ");
		ViewObj(var!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a quadratic algebraic variety",
	[ IsQuadraticVariety and IsQuadraticVarietyRep ],
	function( var )
		Print("Quadratic Variety in ");
		PrintObj(var!.geometry);
	end );
	
InstallMethod( Display, 
	"for a quadratic variety",
	[ IsQuadraticVariety and IsQuadraticVarietyRep ],
	function( var )
		Print("Quadratic Variety in ");
		ViewObj(var!.geometry);
		Print("\n Polynomial: ", DefiningListOfPolynomials(var),"\n");
	end );
	
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
				#if IsStandardHermitianVariety( var ) then
				#	return HermitianPolarSpace( ProjectiveDimension(AmbientSpace(var)), f); #IsStandardHermitianVariety retunrs a no method found error. Commenting out solves the problem. Refinement to be done later.
				#fi; 
				form := HermitianFormByPolynomial(eq,r);
			fi;
			return PolarSpace(form);
		fi;
	end);



#############################################################################
#############################################################################
#############################################################################
### 2. Affine Varieties ###
#############################################################################
#############################################################################
#############################################################################

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
# View, print methods for affine varieties.
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
		PrintObj(var!.geometry);
	end );

InstallMethod( Display, 
	"for an affine variety",
	[ IsAffineVariety and IsAffineVarietyRep ],
	function( var )
		Print("Affine Variety in ");
		ViewObj(var!.geometry);
		Print("\n Defining list of polynomials: ", DefiningListOfPolynomials(var),"\n");
	end );





#############################################################################
#############################################################################
#############################################################################
### 3. Algebraic Varieties ###
#############################################################################
#############################################################################
#############################################################################

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

InstallMethod( PrintObj,
	"for a collections of points of an algebraic variety",
	[ IsPointsOfAlgebraicVariety and IsPointsOfAlgebraicVarietyRep ],
	function( pts )
		Print("Points( ",pts!.variety," )");
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
#O  \in ( <point>, <ptsonvariety> )
# checks if <point> lies on the variety corresponding to <ptsonvariety>
##
InstallMethod( \in,	
	"for an element of an incidence structure and the pointsofalgebraicvariety",
	# 1*SUM_FLAGS+3 increases the ranking for this method
	[IsElementOfIncidenceStructure, IsPointsOfAlgebraicVariety], 1*SUM_FLAGS+3,
	function(point,pts)
		return point in pts!.variety;
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
	

#############################################################################
#############################################################################
#############################################################################
### 4. Segre Varieties ###
#############################################################################
#############################################################################
#############################################################################

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
	range:=Points(SegreVariety(listofspaces));
	
	map:=rec( source:=source, range:=range, map:=segremap );
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


############################################################################
#A Source(<sm>), returns the source of the Segre map
##
InstallMethod( Source,
	"given a Segre map",
	# Note that this can take very long, as it grows very fast. In order to avoid this, 
	# we should install a new Category consisting of tuples of projective points
	[ IsSegreMap ],
	function(sm)
	return Cartesian(sm!.source);
end );

	
#############################################################################
# View, print methods for Segre maps.
##
InstallMethod( ViewObj, 
	"for a Segre map",
	[ IsSegreMap and IsSegreMapRep ],
	function( segremap )
		Print("Segre Map of ");
		ViewObj(segremap!.source);
	end );

InstallMethod( PrintObj, 
	"for a Segre map",
	[ IsSegreMap and IsSegreMapRep ],
	function( segremap )
		Print("Segre Map of ");
		PrintObj(segremap!.source);
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
		PrintObj(var!.geometry);
	end );

#############################################################################
#O  SegreMap( <sv> ), returns the Segre map corresponding with <sv>
##
InstallMethod( SegreMap, 
	"for a Segre variety",
	[IsSegreVariety],
	function(sv)
	  return SegreMap(sv!.inverseimage);
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
		sm:=SegreMap(sv!.inverseimage)!.map;
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
		sm:=SegreMap(sv!.inverseimage)!.map;
		listofpgs:=sv!.inverseimage;
		cart:=Cartesian(List(listofpgs,pg->Points(pg)));
		ptlist:=List(cart,sm);
		return ptlist;
	end);

#############################################################################
#O  Size ( <var> )
# number of points on a Segre variety
##
InstallMethod( Size, 
	"for the set of points of a Segre variety",
	[IsPointsOfSegreVariety],
	function(pts)
		local listofpgs,sv,x;
		sv:=pts!.variety;
		listofpgs:=sv!.inverseimage;
		return Product(List(listofpgs,x->Size(Points(x))));
	end );

####################
#O  ImageElm( <sm>, <x> )
##
InstallOtherMethod( ImageElm, 
	"for a  SegreMap and an element of its source",
	[IsSegreMap, IsList],
	function(sm, x)
		return sm!.map(x); 
	end );

# 
#############################################################################
#O  \^( <x>, <sm> )
##
InstallOtherMethod( \^, 
	"for a  SegreMap and an element of its source",
	[IsList, IsSegreMap],
	function(x, sm)
		return ImageElm(sm,x);
	end );


# 
#############################################################################
#O  ImagesSet( <sm>, <x> )
##
InstallOtherMethod( ImagesSet,
	"for a SegreMap and subset of its source",
	[IsSegreMap, IsList],
	function(sm, x)
		return List(x, t -> t^sm);
	end );


#############################################################################
#############################################################################
#############################################################################
### 5. Veronese Varieties ###
#############################################################################
#############################################################################
#############################################################################

#O  VeroneseMap( <pg> ), returns a function that is the Veronese Map from
#<pg>
##
InstallMethod( VeroneseMap, 
	"for a projective space",
	[IsProjectiveSpace],
	function(pg)
	  local F, dim, func,source,range,map,ty,veronesemap;
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
	
	source:=Points(pg);
	range:=Points(VeroneseVariety(pg));
	
	map:=rec( source:=source, range:=range, map:=func );
	ty:=NewType( NewFamily("VeroneseMapsFamily"), IsVeroneseMap and 
								IsVeroneseMapRep );
	Objectify(ty, map);
	return map;

end );

#############################################################################
# View, print methods for Veronese maps.
##
InstallMethod( ViewObj, 
	"for a Veronese map",
	[ IsVeroneseMap and IsVeroneseMapRep ],
	function( veronesemap )
		Print("Veronese Map of ");
		ViewObj(veronesemap!.source);
	end );

InstallMethod( PrintObj, 
	"for a Veronese map",
	[ IsVeroneseMap and IsVeroneseMapRep ],
	function( veronesemap )
		Print("Veronese Map of ");
		PrintObj(veronesemap!.source);
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
								inverseimage:=pg );
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
		PrintObj(var!.geometry);
  end );
	
#############################################################################
#O  VeroneseMap( <vv> ), returns a function the is the Veronese Map from
# the Veronese variety <vv>
##
InstallMethod( VeroneseMap, 
	"for a Veronese variety",
	[IsVeroneseVariety],
	function(vv)
	  return VeroneseMap(vv!.inverseimage);
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
		vm:=VeroneseMap(vv!.inverseimage)!.map;
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
		vm:=VeroneseMap(vv!.inverseimage)!.map;
		pg:=vv!.inverseimage;
		ptlist:=List(Points(pg),vm);
		return ptlist;
	end);

#############################################################################
#O  Size ( <var> )
# number of points on a Veronese variety
##
InstallMethod( Size, 
	"for the set of points of a Segre variety",
	[IsPointsOfVeroneseVariety],
	function(pts)
		local vv;
		vv:=pts!.variety;
		return Size(Points(vv!.inverseimage));
	end );


#############################################################################
#############################################################################
#############################################################################
### 6. Methods for geometry maps (IsGeometryMap) ###
#############################################################################
#############################################################################
#############################################################################
#O  ImageElm( <gm>, <x> )
##
InstallOtherMethod( ImageElm, 
	"for a  GeometryMap and an element of its source",
	[IsGeometryMap, IsElementOfIncidenceStructure],
	function(gm, x)
		return gm!.map(x); 
	end );


# 
#############################################################################
#O  \^( <x>, <gm> )
##
InstallOtherMethod( \^, 
	"for a  GeometryMap and an element of its source",
	[IsElementOfIncidenceStructure, IsGeometryMap],
	function(x, gm)
		return ImageElm(gm,x);
	end );


# 
#############################################################################
#O  ImagesSet( <gm>, <x> )
##
InstallOtherMethod( ImagesSet,
	"for a GeometryMap and subset of its source",
	[IsGeometryMap, IsElementOfIncidenceStructureCollection],
	function(gm, x)
		return List(x, t -> t^gm);
	end );




############################################################################
#A Source(<gm>), returns the source of the geometry map
##
InstallMethod( Source,
	"given a geometry map",
	[ IsGeometryMap ],
	function(gm)
	return gm!.source;
end );



############################################################################
#A Range(<gm>), returns the range of the geometry map
##
InstallMethod( Range,
	"given a geometry map",
	[ IsGeometryMap ],
	function(gm)
	return gm!.range;
end );


#############################################################################
#############################################################################
#############################################################################
### 7. Grassmann Varieties ###
#############################################################################
#############################################################################
#############################################################################

	
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
    local n,F,pgimage,varmap,func,dim,source,range,map,ty;
    n := pgdomain!.dimension;  ## projective dimension
    F := pgdomain!.basefield;
	
	if n<=2 then
		Error("The dimension of the projective space must be at least 3");
	fi;
 
    if (k <= 0  or k >= n-1) then 
         Error("The dimension of the subspace has to be at least 1 and at most ", n-2);
    fi;

   ## ambient projective space of image has dimension Binomial(n+1,k+1)-1
    dim := Binomial( n+1, k+1 ) - 1;
    pgimage := PG(dim,F); 

    func := function( var )
      local basis,vector,list;
      if ProjectiveDimension(var) <> k then 
         Error("Input must have projective dimension ", k, "\n");
      fi;
      basis := var!.obj;
      list := TransposedMat(basis); 
      vector := List(Combinations([1..n+1], k+1), i -> DeterminantMat( list{i} ));  
      #ConvertToVectorRepNC( vector, F );
      return VectorSpaceToElement(pgimage,vector);
    end;
	
	source:=ElementsOfIncidenceStructure(pgdomain,k+1);
	range:=Points(GrassmannVariety(k,pgdomain)); 
	map:=rec( source:=source, range:=range, map:=func );
	ty:=NewType( NewFamily("GrassmannMapsFamily"), IsGrassmannMap and 
								IsGrassmannMapRep );
	Objectify(ty, map);
	return map;

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

#############################################################################
#O  GrassmannMap ( <subs> )
# for subspaces of a projective space
##
InstallMethod( GrassmannMap, "given collection of varieties of a projectivespace",
    [ IsSubspacesOfProjectiveSpace ],
  function( subspaces )
    return GrassmannMap( subspaces!.type-1, subspaces!.geometry);
  end );

#############################################################################
# View, print methods for Grassmann maps.
##
InstallMethod( ViewObj, 
	"for a Grassmann map",
	[ IsGrassmannMap and IsGrassmannMapRep ],
	function( map )
		Print("Grassmann Map of ");
		ViewObj(map!.source);
	end );

InstallMethod( PrintObj, 
	"for a Grassmann map",
	[ IsGrassmannMap and IsGrassmannMapRep ],
	function( map )
		Print("Grassmann Map of ");
		PrintObj(map!.source);
	end );
	

#############################################################################
#O  GrassmannVariety( <k>,<pg> )
# returns the Grassmann variety from an integer and a projective space
##
InstallMethod( GrassmannVariety, 
	"for an integer and a projective space", 
	[ IsPosInt, IsProjectiveSpace ],

  function( k, pgdomain )
    local n,F,dim,pgimage,r,x,comb,eta,t,pollist,i,j,list,s,icopy,jcopy,js,i1,
			mi,mj,m,si,sj,polset,newpollist,f,gv,var,ty;
    n := pgdomain!.dimension;  ## projective dimension
    F := pgdomain!.basefield;
	
	if n<=2 then
		Error("The dimension of the projective space must be at least 3");
	fi;
 
    if (k <= 0  or k >= n-1) then 
         Error("The dimension of the subspace has to be at least 1 and at most ", n-2);
    fi;

   ## ambient projective space of image has dimension Binomial(n+1,k+1)-1
    dim := Binomial( n+1, k+1 ) - 1;
    pgimage := PG(dim,F);
	r:=PolynomialRing(F,Dimension(pgimage)+1);
	x:=IndeterminatesOfPolynomialRing(r);
	comb:=Combinations([1..n+1],k+1);
	eta:=t->Position(comb,t);
	pollist:=[];
	for i in comb do
		for j in comb do
			list:=[];
			for	s in [1..k+1] do
				icopy:=StructuralCopy(i);
				jcopy:=StructuralCopy(j);
				js:=jcopy[s];
				i1:=icopy[1];
				icopy[1]:=js;
				jcopy[s]:=i1;
				if Size(AsSet(icopy))=k+1 and Size(AsSet(jcopy))=k+1 then
					mi:=Filtered(comb,m->AsSet(m)=AsSet(icopy))[1];
					mj:=Filtered(comb,m->AsSet(m)=AsSet(jcopy))[1];
					si:=SignPerm(MappingPermListList(mi,icopy));
					sj:=SignPerm(MappingPermListList(mj,jcopy));
					Add(list,si*sj*x[eta(mi)]*x[eta(mj)]);
				fi;
			od;
			Add(pollist,x[eta(i)]*x[eta(j)]-Sum(list));
		od;
	od;
	polset:=AsSet(Filtered(pollist,x-> not x = Zero(r)));
	newpollist:=[];
	for f in polset do
		if not -f in newpollist then
			Add(newpollist,f);
		fi;
	od;

	gv:=ProjectiveVariety(pgimage,r,newpollist);
	var:=rec( geometry:=pgimage, polring:=r, listofpols:=newpollist, 
		inverseimage:=ElementsOfIncidenceStructure(PG(n,F),k+1) );
	ty:=NewType( NewFamily("GrassmannVarietiesFamily"), IsGrassmannVariety and 
								IsGrassmannVarietyRep );
	ObjectifyWithAttributes(var,ty,
			DefiningListOfPolynomials, newpollist);
	return var;
	
end );

#############################################################################
#############################################################################
#O  GrassmannVariety( <subspaces> )
# returns the Grassmann variety from a collection of subspaces of a projective space
##
InstallMethod( GrassmannVariety, 
	"for a collection of subspaces of a projective space", 
	[ IsSubspacesOfProjectiveSpace ],
	function( subspaces )
    return GrassmannVariety( subspaces!.type-1, subspaces!.geometry);
end );

#############################################################################

#############################################################################
# View, print methods for Grassmann varieties.
##
InstallMethod( ViewObj, 
	"for a Grassmann variety",
	[ IsGrassmannVariety and IsGrassmannVarietyRep ],
	function( gv )
		Print("Grassmann Variety in ");
		ViewObj(gv!.geometry);
	end );

InstallMethod( PrintObj, 
	"for a Grassmann variety",
	[ IsGrassmannVariety and IsGrassmannVarietyRep ],
	function( gv )
		Print("Grassmann Variety in ");
		PrintObj(gv!.geometry);
	end );



#############################################################################
#O  GrassmannMap( <vv> ), returns a function the is the Grassmann Map from
# the Grassmann variety <vv>
##
InstallMethod( GrassmannMap, 
	"for a Grassmann variety",
	[IsGrassmannVariety],
	function(gv)
	  return GrassmannMap(gv!.inverseimage);
	end );




#############################################################################
#O  PointsOfGrassmannVariety ( <var> )
# returns a object representing all points of a Grassmann variety.
##
InstallMethod( PointsOfGrassmannVariety, 
	"for a Grassmann variety",
	[IsGrassmannVariety and IsGrassmannVarietyRep],
	function(var)
		local pts;
		pts:=rec( 
				geometry:=var!.geometry,
				type:=1,
				variety:=var
				);
		return Objectify(
			NewType( ElementsCollFamily,IsPointsOfGrassmannVariety and
										IsPointsOfGrassmannVarietyRep),
			pts
			);
	end );


#############################################################################
#O  ViewObj method for points of Grassmann variety
##
InstallMethod( ViewObj, 
	"for points of Grassmann variety",
	[ IsPointsOfGrassmannVariety and IsPointsOfGrassmannVarietyRep ],
	function( pts )
		Print("<points of ",pts!.variety,">");
	end );
	 
#############################################################################
#O  Points ( <var> )
# shortcut to PointsOfGrassmannVariety
##
InstallMethod( Points,
	"for a Grassmann variety",
	[IsGrassmannVariety and IsGrassmannVarietyRep],
	function(var)
		return PointsOfGrassmannVariety(var);
	end );

#############################################################################
#O  Iterator ( <ptsr> )
# Iterator for points of a Grassmann variety.
##
InstallMethod( Iterator, 
	"for points of a Grassmann variety", 
	[IsPointsOfGrassmannVariety],
	function(pts)
		local gv,gm,subs,ptlist;
		gv:=pts!.variety;
		gm:=GrassmannMap(gv!.inverseimage)!.map;
		subs:=gv!.inverseimage;
		ptlist:=List(subs,gm);
		return IteratorList(ptlist);
	end );		

#############################################################################
#O  Enumerator ( <ptsr> )
# Enumerator for points of a Grassmann variety.
##
InstallMethod( Enumerator,
	"for points of a Grassmann variety", 
	[IsPointsOfGrassmannVariety],
	function ( pts )
		local gv,gm,subs,ptlist;
		gv:=pts!.variety;
		gm:=GrassmannMap(gv!.inverseimage)!.map;
		subs:=gv!.inverseimage;
		ptlist:=List(subs,gm);
		return ptlist;
	end);

#############################################################################
#O  Size ( <var> )
# number of points on a Grassmann variety
##
InstallMethod( Size, 
	"for the set of points of a Grassmann variety",
	[IsPointsOfGrassmannVariety],
	function(pts)
		local gv;
		gv:=pts!.variety;
		return Size(gv!.inverseimage);
	end );








	
