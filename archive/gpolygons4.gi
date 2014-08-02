Print("godmiljaardedju\n");

#############################################################################
#
#  Kantor Families and associated EGQ's
#
#############################################################################


#############################################################################
# Very particular display methods.
#############################################################################

InstallMethod( ViewObj, 
	"for an element of a Kantor family",
	[ IsElementOfKantorFamily ],
	function( v )
		if v!.type = 1 then Print("<a point of a Kantor family>");
		else Print("<a line of a Kantor family>");
		fi;
	end );

InstallMethod( PrintObj, 
	"for an element of a Kantor family",
	[ IsElementOfKantorFamily ],
	function( v )
		Print(v!.obj);
	end );

#############################################################################
#O  Wrap( <geo>, <type>, <o>  )
# returns the element of <geo> represented by <o>
##
InstallMethod( Wrap, 
	"for an EGQ (Kantor family), and an object",
	[IsElationGQByKantorFamily, IsPosInt, IsPosInt, IsObject],
	function( geo, type, class, o )
		local w;
		w := rec( geo := geo, type := type, class := class, obj := o );
		Objectify( NewType( ElementsOfIncidenceStructureFamily,   #ElementsFamily,
			IsElementOfKantorFamilyRep and IsElementOfKantorFamily ), w );
		return w;
	end );

#############################################################################
#O  \=( <a>, <b>  )
# for thwo elements of a Kantor family.
##
InstallMethod( \=,
	"for two elements of a Kantor family",
	[IsElementOfKantorFamily, IsElementOfKantorFamily], 
	function( a, b ) 
		return a!.obj = b!.obj; 
	end );

#############################################################################
#O  \<( <a>, <b>  )
# for thwo elements of a Kantor family.
##
InstallMethod( \<, 
	"for two elements of a Kantor family",
	[IsElementOfKantorFamily, IsElementOfKantorFamily], 
	function( a, b ) 
		if a!.type <> b!.type then return a!.type < b!.type;
		else
			if a!.class <> b!.class then return a!.class < b!.class;
			else return a!.obj < b!.obj; 
			fi;
		fi;
	end );

#############################################################################
#O  \<( <a>, <b>  )
# for a group and two lists.
##
InstallMethod( IsKantorFamily,
	"for a group and two lists of subgroups",
	[IsGroup, IsList, IsList],
	function( g, f, fstar)
		local flag, a, b, c, ab, astar, tplus1;
		tplus1 := Size(f);
		flag := true;
		if not ForAll([1..Size(fstar)], x -> IsSubgroup(fstar[x], f[x])) then
			Error( "second and third arguments are incompatile");
		fi;
		if not ForAll(fstar, x -> IsSubgroup(g, x)) then
			Error( "elements of second argument are not subgroups of first argument" );
		fi;

		Info(InfoFinInG, 1, "Checking tangency condition...");
    
		## K2 tangency condition
		for a in [1..tplus1-1] do
			for astar in [a+1..tplus1] do
				flag := IsTrivial(Intersection(fstar[astar], f[a]));
				if not flag then 
					Print("Failed tangency condition\n");
					Print(a,"  ",astar,"\n"); 
					return flag;
				fi;
			od;
		od;

		Info(InfoFinInG, 1, "Checking triple condition...");

		## K1 triple condition
		for a in [1..tplus1-2] do
			for b in [a+1..tplus1-1] do
				for c in [b+1..tplus1] do
					ab := DoubleCoset(f[a], One(g), f[b]);
					flag := IsTrivial(Intersection(ab, f[c]));
					if not flag then
						Print("Failed triple condition\n"); 
						Print(a,"  ",b,"  ",c,"\n"); 
						return flag;
					fi;
				od;
			od;
		od;
		return flag;
	end );

#############################################################################
#F  OnKantorFamily
##
InstallGlobalFunction( OnKantorFamily,
  function( v, el )
    local geo, type, class, new; 
    geo := v!.geo;
    type := v!.type;
    class := v!.class;
    if (type = 1 and class = 3) or (type = 2 and class = 2) then 
       return v;
    elif (type = 1 and class = 2) or (type = 2 and class = 1) then
       new := [v!.obj[1], CanonicalRightCosetElement(v!.obj[1],v!.obj[2]*el)];  
    else 
       new := OnRight(v!.obj, el);
    fi;  
    return Wrap(geo, type, class, new); 
  end );

#############################################################################
#O  EGQByKantorFamily(<g>, <f>, <fstar>)
# for a group and two lists.
##
InstallMethod( EGQByKantorFamily, 
	"for a group, a list and a list",
	[IsGroup, IsList, IsList],
	function( g, f, fstar)
    local pts1, pts2, pts3, ls1, ls2, inc, listels,
          x, y, geo, ty, points, lines, pointreps, linereps;
    ## Some checks first.
    ## We do not check if it's a Kantor family though (this can be rather slow)

    if not ForAll([1..Size(fstar)], x -> IsSubgroup(fstar[x], f[x])) then
       Error( "second and third arguments are incompatible");
       return;
    fi;
    if not ForAll(fstar, x -> IsSubgroup(g, x)) then
       Error( "elements of second argument are not subgroups of first argument" );
       return;
    fi;

    inc := function(x, y)
             if x!.type = y!.type then
                return x = y;
             elif x!.type = 1 and y!.type = 2 then
                if x!.class = 1 and y!.class = 1 then
                   return x!.obj*y!.obj[2]^-1 in y!.obj[1];
                elif x!.class = 2 and y!.class = 1 then
                   return IsSubset(x!.obj[1], RightCoset(y!.obj[1], y!.obj[2]*x!.obj[2]^-1));
                elif x!.class = 2 and y!.class = 2 then
                   return x!.obj[1] = y!.obj; 
                elif x!.class = 3 and y!.class = 2 then
                   return true;
                else return false;
                fi;
             else 
                return inc(y, x);
             fi;   
           end;

    geo := rec( pointsobj := [], linesobj := [], incidence := inc );
    ty := NewType( GeometriesFamily, IsElationGQByKantorFamily and IsGeneralisedPolygonRep );  
    Objectify( ty, geo );

    Info(InfoFinInG, 1, "Computing points from Kantor family...");

    ## wrapping
    pts1 := List(g, x -> Wrap(geo,1,1,x));
    pts2 := Set(fstar, b -> Set(RightCosets(g,b),x->[b,CanonicalRightCosetElement(b, Representative(x))]));
    pts2 := Concatenation(pts2);
    pts2 := List(pts2, x -> Wrap(geo,1,2,x));

    Info(InfoFinInG, 1, "Computing lines from Kantor family...");

    ls1 := Set(f, a -> Set(RightCosets(g,a), x -> [a,CanonicalRightCosetElement(a, Representative(x))]));
    ls1 := Concatenation(ls1); 
    ls1 := List(ls1, x -> Wrap(geo,2,1,x)); ## this is a strictly sorted list

    ## symbols (note that we're making incidence easier here)
    ls2 := Set(fstar, x -> Wrap(geo, 2, 2, x)); 
    pts3 := [ Wrap(geo, 1, 3, 0) ];

    points := Concatenation(pts1,pts2,pts3); 
    lines := Concatenation(ls1, ls2);
    pointreps := Concatenation( [pts1[1]], Set(fstar, b -> Wrap(geo,1,2,[b, One(b)])), pts3);
    linereps := Concatenation(Set(f, a -> Wrap(geo,2,1,[a, One(g)])), ls2);
    
    geo!.points := points;
    geo!.lines := lines;
    
	listels := function(gp,i)
		if i = 1 then
			return points;
		else
			return lines;
		fi;
	end;
	
	geo!.listelements := listels;
	
	SetBasePointOfEGQ( geo, pts3[1] );
    SetAmbientSpace(geo, geo);
    SetOrder(geo, [Index(g,fstar[1]), Size(f)-1]);
    SetCollineationAction(g, OnKantorFamily);
    SetElationGroup(geo, g);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    ## Orbit reps:
    SetRepresentativesOfElements(geo, Concatenation(pointreps,linereps));
    SetName(geo,Concatenation("<EGQ of order ",String([Index(g,fstar[1]), Size(f)-1])," and basepoint 0>"));
	return geo;
  end );








#############################################################################
#O  Iterator( <vs>  )
# returns an iterator for the elements of an EGQ defined by a Kantor family.
##
#InstallMethod(Iterator, 
#	"for elements of an EGQ defined by a Kantor family",
#	[IsAllElementsOfKantorFamily],

##  We can do much better here.
##  Perhaps we need to think about implementing enumerators/iterators
##  for Kantor families. One day there might be enumerators for cosets?

#  function( vs )
#    local ps, j, vars;
#    ps := vs!.geometry;
#    j := vs!.type;
#    if j = 1 then 
#       vars := ps!.points;
#       return IteratorList( vars );
#    elif j = 2 then 
#       vars := ps!.lines;
#       return IteratorList( vars );
#    else Error("Element type does not exist"); return;
#    fi;
#  end );

#############################################################################
#O  IsIncident( <vs>  )
# simply uses the incidence relation that is built in in the gp.
##
#InstallMethod( IsIncident, 
#	"for elements of a Kantor family",
#    [IsElementOfKantorFamily, IsElementOfKantorFamily],
#	function( x, y )
#		local inc;
#		inc := x!.geo!.incidence;
#		return inc(x, y);
#	end );



#############################################################################
#
#   q-Clans and EGQ's made from them
#
#############################################################################

#############################################################################
#O  IsAnisotropic( <m>, <f> )
# simply checks if a matrix is anisotropic.
##
InstallMethod( IsAnisotropic, 
	"for a matrix and a finite field",
	[IsFFECollColl,  IsField and IsFinite],
	function( m, f )
		local pairs, o;
		o := Zero(f);
		pairs := Difference(AsList(f^2),[[o,o]]);
		return ForAll(pairs, x -> x * m * x <> o);
	end );

#############################################################################
#O  IsAnisotropic( <clan>, <f> )
# simply checks if all differences of elements in a set of 2 x 2 matrices is are
# anisotropic.
##
InstallMethod( IsqClan, 
	"input are 2x2 matrices", 
    [ IsFFECollCollColl,  IsField and IsFinite],
	function( clan, f )
		return ForAll(Combinations(clan,2), x -> IsAnisotropic(x[1]-x[2], f));
	end );

#############################################################################
#O  qClan( <clan>, <f> )
# returns a qClan object from a suitable list of matrices.
##
InstallMethod( qClan, 
	"for a list of 2x2 matrices",
	[ IsFFECollCollColl, IsField ],
	function( m, f ) 
		local qclan;
		## test to see if it is a qClan
		if not ForAll(Combinations(m, 2), x -> IsAnisotropic(x[1]-x[2], f)) then
			Error("These matrices do not form a q-clan");
		fi;
		qclan := rec( matrices := m, basefield := f );
		Objectify( NewType( qClanFamily, IsqClanObj and IsqClanRep ), qclan );
		return qclan;
	end );

InstallMethod( ViewObj, 
	"for a qClan",
	[ IsqClanObj and IsqClanRep ],
	function( x )
		Print("<q-clan over ",x!.basefield,">");
	end );

InstallMethod( PrintObj, 
	"for a qClan",
	[ IsqClanObj and IsqClanRep ],
	function( x )
		Print("qClan( ", x!.matrices, ", ", x!.basefield , ")");
	end );

#############################################################################
#O  AsList( <clan> )
# returns the matrices defining a qClan
##
InstallOtherMethod( AsList,
	"for a qClan",
	[IsqClanObj and IsqClanRep],
	function( qclan )
		return qclan!.matrices;
	end );

#############################################################################
#O  AsList( <clan> )
# returns the matrices defining a qClan
##
InstallOtherMethod( AsSet, 
	"for a qClan",
	[IsqClanObj and IsqClanRep],
	function( qclan )
		return Set(qclan!.matrices);
	end );

#############################################################################
#O  BaseField( <clan> )
# returns the BaseField of the qClan
##
InstallMethod( BaseField, 
	"for a qClan",
	[IsqClanObj and IsqClanRep],
	function( qclan )
		return qclan!.basefield;
	end );

#############################################################################
#O  BaseField( <clan> )
# checks if the qClan is linear.
##
InstallMethod( IsLinearqClan, 
	"for a qClan",
	[ IsqClanObj ],
	function( qclan )
		local blt;
		blt := BLTSetByqClan( qclan ); 
		return ProjectiveDimension(Span(blt)) = 2;
	end );

#############################################################################
#O  LinearqClan( <clan> )
# returns a linear qClan.
##
InstallMethod( LinearqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan, n;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		n := First(GF(q), t -> not IsZero(t) and LogFFE(t, Z(q)^2) = fail);
		if n = fail then
			Error("Couldn't find nonsquare");
		fi;
		f := t -> 0 * Z(q)^0;
		g := t -> -n * t;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
		clan := qClan(clan, GF(q));
		SetIsLinearqClan(clan, true);
		return clan;
	end );

#############################################################################
#O  FisherThasWalkerKantorBettenqClan( <q> )
# returns the Fisher Thas Walker Kantor Betten qClan
##
InstallMethod( FisherThasWalkerKantorBettenqClan, 
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		if q mod 3 <> 2 then
			Error("q must be congruent to 2 mod (3)");
		fi;
		f := t -> 3/2 * t^2;
		g := t -> 3 * t^3;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
		return qClan(clan, GF(q));
	end );

#############################################################################
#O  KantorMonomialqClan( <q> )
# returns the Kantor Monomial qClan
##
InstallMethod( KantorMonomialqClan, 
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		if not q mod 5 in [2, 3] then
			Error("q must be congruent to 2 mod (3)");
		fi;
		f := t -> 5/2 * t^3;
		g := t -> 5 * t^5;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
	return qClan(clan, GF(q));
end );

#############################################################################
#O  KantorKnuthqClan( <q> )
# returns the Kantor Knuth qClan
##
InstallMethod( KantorKnuthqClan, 
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan, n, sigma;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		if IsPrime(q) then
			Error("q is a prime");
		fi;
		sigma := FrobeniusAutomorphism(GF(q));
		n := First(GF(q), t -> not IsZero(t) and LogFFE(t, Z(q)^2) = fail);
		if n = fail then
			Error("Couldn't find nonsquare");
		fi;
		f := t -> 0 * Z(q)^0;
		g := t -> -n * t^sigma;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
		return qClan(clan, GF(q));
	end );

#############################################################################
#O  FisherqClan( <q> )
# returns the Fisher qClan
##
InstallMethod( FisherqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan, n, zeta, omega, squares, nonsquares, i, z, a, t, j;
		if not IsPrimePowerInt(q) or IsEvenInt(q) then
			Error("Argument must be an odd prime power");
    fi;
	squares := ShallowCopy(AsList(Group(Z(q)^2)));; Add(squares, 0*Z(q));
	nonsquares := Difference(AsList(GF(q)),squares);;
	n := First(nonsquares, t -> t-1 in squares);

	zeta := PrimitiveRoot(GF(q^2));
	omega := zeta^(q+1);
	i := zeta^((q+1)/2);
	z := zeta^(q-1);
	a := (z+z^q)/2;
	clan := [];  
	for t in GF(q) do
	    if t^2-2/(1+a) in squares then 
	       Add(clan, [[t, 0],[0,-omega*t]] * Z(q)^0);
	    fi;
	od;

	for j in [0..(q-1)/2] do
	    Add(clan, [[-(z^(2*j+1)+z^(-2*j))/(z+1), i*(z^(2*j+1)-z^(-2*j))/(z+1)],
	       [i*(z^(2*j+1)-z^(-2*j))/(z+1), -omega*(z^(2*j+1)+z^(-2*j))/(z+1)]] * Z(q)^0 );
	od;

  return qClan(clan, GF(q));
end );

#############################################################################
#O  KantorFamilyByqClan( <clan> )
# returns the Kantor familyt corresponding with the q-Clan <clan>
##
InstallMethod( KantorFamilyByqClan, 
	"for a q-Clan",
    [ IsqClanObj and IsqClanRep ],
	function( clan )
    local g, q, f, i, omega, mat, at, ainf, ainfstar, clanmats,
          ainfgens, centregens, as, astars, k;
    f := clan!.basefield;
    clanmats := clan!.matrices;
    q := Size(f);
    i := One(f); 
    omega := PrimitiveElement(f);
    mat := function(a1,a2,c,b1,b2) 
             return i * [[1, a1, a2,  c], [0,  1,  0, b1],
                         [0,  0,  1, b2], [0,  0,  0,  1]];
           end;
    centregens := [mat(0,0,1,0,0), mat(0,0,omega,0,0)];
    ainfgens := [mat(0,0,0,1,0),mat(0,0,0,0,1),mat(0,0,0,omega,0),mat(0,0,0,0,omega)];
    ainf := Group(ainfgens); 
    ainfstar := Group(Union(ainfgens,centregens));

    at := function( m )
       ## returns generators for Kantor family element defined by q-Clan element m
       local a1, a2, k, gens, bas, zero;
       gens := [];
       bas := AsList(Basis(f));
       zero := Zero(f);
       for a1 in bas do
           k := [a1,zero] * (m+TransposedMat(m));
           Add(gens, mat(a1,zero,[a1,zero]*m*[a1,zero],k[1],k[2]) );
       od;

       for a2 in bas do
           k := [zero,a2] * (m+TransposedMat(m));
           Add(gens, mat(zero,a2,[zero,a2]*m*[zero,a2],k[1],k[2]) );
       od;    
       return gens;
    end;

    g := Group(Union( ainfgens, centregens, at(clanmats[1]) ));
    as := List(clanmats, m -> Group( at(m) ));
    Add(as, ainf);
    astars := List(clanmats, m -> Group(Union(at(m), centregens)));
    Add(astars, ainfstar);
    return [g, as, astars];
  end );

#############################################################################
#O  EGQByqClan( <clan> )
# returns the EGQ constructed from the q-Clan, using the corresponding Kantor family.
##
InstallMethod( EGQByqClan, 
	"for a q-Clan",
	[ IsqClanObj and IsqClanRep ],
	function( clan )
		local kantor;
		kantor := KantorFamilyByqClan( clan );
    
		Info(InfoFinInG, 1, "Computed Kantor family. Now computing EGQ...");
			return EGQByKantorFamily(kantor[1], kantor[2], kantor[3]);
  end );

#############################################################################
#
#	BLT sets and q-Clans.
#
#############################################################################

#############################################################################
#O  BLTSetByqClan( <clan> )
# returns the BLT set corresponding with the q-Clan <clan>
##
InstallMethod( BLTSetByqClan, 
	"for a q-Clan",
	[ IsqClanObj and IsqClanRep ],
	function( clan )
    ##
    ## The q-clan must consist only of symmetric matrices
    ##
    local q, i, f,  blt, m, sesq, c1, c2, change, w, ps;
    f := clan!.basefield;
    q := Size(f);
    i := One(f); 
    blt := List(clan!.matrices, t -> [i, t[2][2], -t[1][2], t[1][1],  t[1][2]^2 -t[1][1]*t[2][2]]);
    Add(blt, [0,0,0,0,1]*i);  ## last point is distinguished point.
      
    ## This BLT-set is in Q(4,q) defined by Gram matrix
    w := PrimitiveRoot(f);
    m := [[0,0,0,0,1],[0,0,0,1,0],[0,0,w^((q+1)/2),0,0],[0,1,0,0,0],[1,0,0,0,0]]*i;
    sesq := BilinearFormByMatrix(m, f);
    ps := PolarSpace( sesq );    
    return List(blt, x -> VectorSpaceToElement(ps, x)); 
end );

#############################################################################
#O  EGQByBLTSet( <blt> )
##
InstallMethod( EGQByBLTSet, 
     "constructs an EGQ from a BLT-set of points of Q(4,q) via the Knarr construction",
     [ IsList ], 
  function( blt )   
   local q4q, f, w3q, duality, w5q, p, pg5, solid, 
         q4qcanonical, iso, bltdual, geo, bas, 
         mat, elations, a, gens, zero, action, b;
   q4q := AmbientGeometry( blt[1] );
   f := q4q!.basefield;
   w3q := SymplecticSpace(3, f);
   duality := NaturalDuality( w3q );
   w5q := SymplecticSpace(5, f);
   p := VectorSpaceToElement(w5q, [1,0,0,0,0,0] * One(f));
   pg5 := AmbientSpace( w5q );
   solid := VectorSpaceToElement(pg5, [[1,0,0,0,0,1],[0,0,1,0,0,0],
                                       [0,0,0,1,0,0],[0,0,0,0,1,0]]*One(f));
   q4qcanonical := Range(duality)!.geometry;                 
   iso := IsomorphismPolarSpaces(q4q, q4qcanonical);
   bltdual := PreImagesSet(duality, ImagesSet(iso, blt));

   Info(InfoFinInG, 1, "Now embedding dual BLT-set into W(5,q)...");

   geo := EGQByBLTSet( bltdual, p, solid);

   ## Now we construct the elation group. See Maska Law's Thesis for details 
   ## (we have a different form though, so we need to apply a base change).

   Info(InfoFinInG, 1, "Computing elation group...");

   mat := function(a,b,c,d,e)
            local m;
            m := IdentityMat(6, f);
            m[6]{[1..5]} := [e,d,c,-b,-a];
            m[2][1] := a; m[3][1] := b; m[4][1] := c; m[5][1] := d;
            return m;
          end;
   bas := AsList(Basis(f));
   gens := [];
   zero := Zero(f);
   for a in bas do
       Add(gens, mat(a,zero,zero,zero,zero) );
       Add(gens, mat(zero,a,zero,zero,zero) );
       Add(gens, mat(zero,zero,a,zero,zero) );
       Add(gens, mat(zero,zero,zero,a,zero) );
       Add(gens, mat(zero,zero,zero,zero,a) );
   od;
   ## base change 
   b := [ [ 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1 ], 
          [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ], 
          [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0 ] ];
   gens := List(gens, t-> b*t*b^-1);
   gens := List(gens, t -> CollineationOfProjectiveSpace(t,f));
   elations := Group( gens ); 
   SetElationGroup( geo, elations );

   action := function(el, x)
               return Wrap( geo, el!.type, OnProjSubspaces(el!.obj, x));
             end;
   SetCollineationAction( elations, action );
   return geo;
 end );

#############################################################################
#O  FlockGQByqClan( <clan> )
# returns the BLT set corresponding with the q-Clan <clan>
# not documented yet.
##
InstallMethod( FlockGQByqClan, [ IsqClanObj ],
 function( qclan )
  local f, q, mat, form, w5, p, blt, x, perp, pperp, pg5, a, bas, gens, zero, elations, action,
        projpoints, gqpoints, gqlines, gqpoints2, gqlines2, res, geo, ty, points, lines, clan,
		pgammasp, stabp, stabblt, hom, omega, imgs;
  f := qclan!.basefield;
  clan := qclan!.matrices; 
  q := Size(f);
  if not IsOddInt(q) then 
       Error("Invalid input"); return;
  fi;

  mat := [[0,0,0,0,0,1],[0,0,0,0,1,0],[0,0,0,1,0,0],
          [0,0,-1,0,0,0],[0,-1,0,0,0,0],[-1,0,0,0,0,0]] * Z(q)^0;
  form := BilinearFormByMatrix(mat, GF(q));
  w5 := PolarSpace( form );
  p := VectorSpaceToElement(w5, [1,0,0,0,0,0] * Z(q)^0);

  blt := [ VectorSpaceToElement(w5, [[1,0,0,0,0,0], [0,0,0,1,0,0],[0,0,0,0,1,0]]*One(f)) ];
  for x in clan do
      Add(blt, VectorSpaceToElement(w5, [[1,0,0,0,0,0], [0,1,0,x[1][2],x[1][1],0], [0,0,1,x[2][2],x[1][2],0]] * One(f)));
  od;
    Info(InfoFinInG, 1, "Making flock GQ...");

  perp := PolarMap(w5);;
  pperp := perp(p);
  pg5 := AmbientSpace( w5 );;
  projpoints := Points(pg5);;

	Info(InfoFinInG, 1, "...points outside of perp of P...");

  gqpoints := Filtered(projpoints, x -> not x in pperp);;
  Add(gqpoints, p);

  gqlines := ShallowCopy(blt);;
  gqpoints2 := [];;

    Info(InfoFinInG, 1, "...lines contained in some BLT-set element...");

  for x in gqlines do
      res := ShadowOfElement(pg5, x, 2);
      res := Filtered(res, t -> not p in t);
      Append(gqpoints2, res);
  od;
  gqpoints2 := Set(gqpoints2);;
  gqlines2 := [];;
  
    Info(InfoFinInG, 1, "...planes meeting some BLT-set element in a line...");
  for x in gqpoints2 do 
      res := ShadowOfFlag(pg5, [x,perp(x)], 3);
      res := Filtered(res, t -> not p in t);  
      Append(gqlines2, res); 
  od;

    Info(InfoFinInG, 1, "...sorting the points and lines...");  

  points := SortedList( Concatenation(gqpoints, gqpoints2) );;
  lines := SortedList( Concatenation(gqlines, gqlines2) );;

  geo := rec( points := points, lines := lines, incidence := IsIncident);;

  ty := NewType( GeometriesFamily, IsElationGQ and IsGeneralisedPolygonRep);
  Objectify( ty, geo );
  SetBasePointOfEGQ( geo, Wrap(geo, 1, p) );  
  SetAmbientSpace(geo, w5);
  SetOrder(geo, [q^2, q]);
  SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);

  Info(InfoFinInG, 1, "Computing collineation group in PGammaSp(6,q)...");
  pgammasp := CollineationGroup( w5 );
  stabp := SetwiseStabilizer(pgammasp, OnProjSubspaces, [p])!.setstab;

  Info(InfoFinInG, 1, "..computed stabiliser of P");

  ## compute the stabiliser of the BLT-set differently...

  hom := ActionHomomorphism(stabp, AsList(Planes(p)), OnProjSubspaces, "surjective"); 
  omega := HomeEnumerator(UnderlyingExternalSet(hom));;
  imgs := Filtered([1..Size(omega)], x -> omega[x] in blt);;
  stabblt := Stabilizer(Image(hom), imgs, OnSets);
  gens := GeneratorsOfGroup(stabblt);
  gens := List(gens, x -> PreImagesRepresentative(hom, x));
  stabblt := GroupWithGenerators(gens);

  Info(InfoFinInG, 1, "..computed stabiliser of BLT set");

###  stabblt := SetwiseStabilizer(stabp, OnProjSubspaces, blt)!.setstab;  

  SetCollineationGroup( geo, stabblt );
  action := function(el, x)
               return Wrap( geo, el!.type, OnProjSubspaces(el!.obj, x));
             end;
  SetCollineationAction( stabblt, action );

  ## Now we construct the elation group. See Maska Law's Thesis for details 

  Info(InfoFinInG, 1, "Computing elation group...");
  mat := function(a,b,c,d,e)
            local m;
            m := IdentityMat(6, f);
            m[6]{[1..5]} := [e,d,c,-b,-a];
            m[2][1] := a; m[3][1] := b; m[4][1] := c; m[5][1] := d;
            return m;
         end;
  bas := AsList(Basis(f));
  gens := [];
  zero := Zero(f);
  for a in bas do
       Add(gens, mat(a,zero,zero,zero,zero) );
       Add(gens, mat(zero,a,zero,zero,zero) );
       Add(gens, mat(zero,zero,a,zero,zero) );
       Add(gens, mat(zero,zero,zero,a,zero) );
       Add(gens, mat(zero,zero,zero,zero,a) );
  od;
  gens := List(gens, t -> CollineationOfProjectiveSpace(t,f));
  elations := SubgroupNC( stabblt, gens ); 
  SetElationGroup( geo, elations );
  SetCollineationAction( elations, action );

  return geo;
end );

#############################################################################
#O  EGQByBLTSet( <clan> )
# not documented yet.
##
InstallMethod( EGQByBLTSet, 
         "constructs an EGQ from a BLT-set of lines of W(3,q) via the Knarr construction",
     [IsList, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],

  function( blt, p, solid)
   ## The point p is a point of W(5,q).
   ## "solid" is a 3-space contained in P^perp
   ## blt is a BLT-set of lines of W(3,q)

   local w3q, f, q, w5q, perp, pperp, res, x, pg5, gqpoints, gqpoints2,
         projpoints, gqlines, gqlines2, em, blt2, pis, info,
         geo, points, lines, ty;

   w3q := blt[1]!.geo;
   f := w3q!.basefield;
   q := Size(f);
   w5q := SymplecticSpace(5, f);
   perp := PolarMap(w5q);
   pperp := perp(p);
   
   ## check everything is kosher
   if not solid in pperp then
      Error("Solid is not contained in perp of point");
   fi;
   if p in solid then
      Error("Chosen point is contained in the chosen solid");
   fi;

   pg5 := AmbientSpace( w5q );
   projpoints := ElementsOfIncidenceStructure(pg5, 1);
   
   Info(InfoFinInG, 1, "Computing points(1) of Knarr construction...");
   
   gqpoints := Filtered(projpoints, x -> not x in pperp);;
   Add(gqpoints, p);

   em := NaturalEmbeddingBySubspace(w3q, w5q, solid);
   blt2 := List(blt,t->t^em);

   Info(InfoFinInG, 1, "Computing lines(1) of Knarr construction...");
  
   pis := List(blt2, l -> Span(p, l));
   gqlines := pis;
   gqpoints2 := [];

   Info(InfoFinInG, 1, "Computing points(2) of Knarr construction...");

   for x in pis do
       res := ShadowOfElement(pg5, x, 2);
       res := Filtered(res, t -> not p in t);
       Append(gqpoints2, res);
   od;
   gqpoints2 := Set(gqpoints2); 

   Info(InfoFinInG, 1, "Computing lines(2) of Knarr construction... please wait");

   gqlines2 := [];
   info := InfoLevel( InfoFinInG );
   SetInfoLevel(InfoFinInG, 0);

   for x in gqpoints2 do
       res := Planes(w5q, x);
       res := Filtered(res, t -> not t in pperp);
       Append(gqlines2, res); 
   od;
   SetInfoLevel(InfoFinInG, info);
 
   points := Concatenation(gqpoints, gqpoints2);
   lines := Concatenation(gqlines, gqlines2);

   geo := rec( points := points, lines := lines, 
                incidence := IsIncident);
   ty := NewType( GeometriesFamily, IsElationGQ and IsGeneralisedPolygonRep);
   Objectify( ty, geo );

   SetBasePointOfEGQ( geo, Wrap(geo, 1, p) );  
   SetAmbientSpace(geo, geo);
   SetOrder(geo, [q^2, q]);
   SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
   return geo;
 end );
