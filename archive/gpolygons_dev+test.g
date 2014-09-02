ps := EllipticQuadric(5,3);
pts := Set(Points(ps));
lines := Set(Lines(ps));
inc := \*:

gp := GeneralisedPolygonByElements(pts,lines,inc);




gp := SplitCayleyHexagon(3);

shadpoint := function( pt )
	local planevec, flag, plane, f;
	f := BaseField( pt );
	planevec := SplitCayleyPointToPlane( pt );
	plane := VectorSpaceToElement(PG(6,f),planevec);
	flag := FlagOfIncidenceStructure(PG(6,f),[pt,plane]);
	return List(ShadowOfFlag(PG(6,f),flag,2),x->Wrap(pt!.geo,2,Unwrap(x)));
end;

shadline := function( l )
	return List(Points(ElementToElement(AmbientSpace(l),l)),x->Wrap(l!.geo,1,x!.obj));
end;

  shadpoint := function( pt )
        return List(vn{Adjacency(graph,Position(vn,pt!.obj))},x->Wrap(gp,2,x));
    end;

    shadline := function( line )
        return List(vn{Adjacency(graph,Position(vn,line!.obj))},x->Wrap(gp,1,x));
    end;


#############################################################################
#O  ElementsOfIncidenceStructure( <gp>, <j> )
# returns the elements of <gp> of type <j>. <gp> is an EGQ by Kanto Family
##
InstallMethod( ElementsOfIncidenceStructure, 
	"for a an EGB by Kantor Family and a positive integer",
	[IsElationGQByKantorFamily, IsPosInt],
	function( gp, j )
		local s, t;
		if j in [1,2] then 
			s := Order(gp)[j]; t := Order(gp)[3-j];
		else 
			Error("Incorrect type value");
		fi;
		return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfKantorFamily and
                                IsAllElementsOfGeneralisedPolygonRep),
				rec( geometry := gp, type := j, size := (1+s)*(1+s*t) )
						);
	end );

#############################################################################
#O  ElementsOfIncidenceStructure( <gp>, <j> )
# returns the elements of <gp> of type <j>. <gp> is an generalised hexagon.
##
InstallMethod( ElementsOfIncidenceStructure, 
	"for a generalised hexagon and a positive integer",
	[IsGeneralisedHexagon and IsGeneralisedPolygonRep, IsPosInt],
	function( gp, j )	
		local s, t, sz;
		if j in [1,2] then 
			s := Order(gp)[j]; t := Order(gp)[3-j];
		else 
			Error("Incorrect type value");
		fi;
		return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfGeneralisedHexagon and
                                IsAllElementsOfGeneralisedPolygonRep),
			rec( geometry := gp, type := j, size := (1+s)*(1+s*t+s^2*t^2) )
						);
	end );


q := 3;
ps_hex := AmbientPolarSpace(SplitCayleyHexagon(q));
ps := ParabolicQuadric(6,q);
form := QuadraticForm(ps_hex);
mat := BaseChangeToCanonical(form);
List(Points(ps),x->VectorSpaceToElement(ps_hex,x!.obj*mat));
List(Lines(ps),x->VectorSpaceToElement(ps_hex,Unpack(x!.obj)*mat));

List(Points(ps_hex),x->VectorSpaceToElement(ps,x!.obj*mat^-1));




























InstallMethod( CollineationGroup,
	"for a generalised hexagon",
	[ IsGeneralisedHexagon ],
  function( hexagon )
    local f, q, pps, frob, sigma, m, mp, ml, nonzerof, nonzeroq, w, 
          gens, newgens, x, coll, orblen, hom, domain, rep;

    if Size(Set(Order(hexagon))) > 1 then
       Info(InfoFinInG, 1, "for Twisted Triality Hexagon");
       f := hexagon!.basefield;
       ## field must be GF(q^3);
       q := RootInt(Size(f), 3);
	 if not q^3 = Size(f) then
         Error("Field order must be a cube of a prime power");
       fi;
       pps := Characteristic(f);
       frob := FrobeniusAutomorphism(f);
       sigma := frob^LogInt(q,pps);    ## automorphism of order 3

	 # The generators of 3D4(q) were taken from Hendrik 
    	 # Van Maldeghem's book: "Generalized Polygons".


       m:=[[ 0, 0, 0, 0, 0, 1, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 1, 0],
           [ 0, 0, 0, 0, 1, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 1],   
           [ 0, 1, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 1, 0, 0, 0, 0, 0],
           [ 1, 0, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 0, 1, 0, 0, 0, 0]]*One(f);  
       ConvertToMatrixRep(m, f);
       mp:=d->[[1,  0,  0,  0,  0,  d,  0,  0],  
               [0,  1,  0,  0, -d,  0,  0,  0],  
               [0,  0,  1,  0,  0,  0,  0,  0],  
               [0,  0,  -d^sigma,  1,  0,  0,  0,  0],
               [0,  0,  0,  0,  1,  0,  0,  0],  
               [0,  0,  0,  0,  0,  1,  0,  0],  
               [0,0,d^sigma*d^(sigma^2),-d^(sigma^2),0,0,1,d^sigma],
               [0,0,d^(sigma^2),0,0,0,0,1]]*One(f);   
       ml:=d->[[1, -d,  0,  0,  0,  0,  0,  0],  
               [0,  1,  0,  0,  0,  0,  0,  0],  
               [0,  0,  1,  0,  0,  0,  0,  0],  
               [0,  0,  0,  1,  0,  0,  0,  0],  
               [0,  0,  0,  0,  1,  0,  0,  0],  
               [0,  0,  0,  0,  d,  1,  0,  0],  
               [0,  0,  0,  0,  0,  0,  1,  0],
               [0,  0,  0,  0,  0,  0,  0,  1]]*One(f);

       nonzerof := AsList(f){[2..Size(f)]};
       nonzeroq := AsList(GF(q)){[2..q]};

       gens := Union([m], List(nonzerof, mp), List(nonzeroq, ml));
       for x in gens do
           ConvertToMatrixRep(x, f);
       od;
       newgens := List(gens, x -> CollineationOfProjectiveSpace(x,f));
       coll := GroupWithGenerators(newgens);

       Info(InfoFinInG, 1, "Computing nice monomorphism...");
       orblen := (q+1)*(q^8+q^4+1);
       rep := RepresentativesOfElements(hexagon)[2];
	   domain := Orb(coll, rep, OnProjSubspaces, 
                    rec(orbsizelimit := orblen, hashlen := 2*orblen, storenumbers := true));
       Enumerate(domain);
       Info(InfoFinInG, 1, "Found permutation domain...");
	   hom := OrbActionHomomorphism(coll, domain);   
		
       #hom := NiceMonomorphismByOrbit(coll, rep,
	   #            OnProjSubspaces, orblen );
	 
 	   SetIsBijective(hom, true);
       ## for some reason, hom has not stored a prefun

	   SetNiceObject(coll, Image(hom) );
       SetNiceMonomorphism(coll, hom );
       SetCollineationAction(coll, OnProjSubspaces);
       SetName(coll, Concatenation("4D_3(",String(q),")") );
   else 
       Info(InfoFinInG, 1, "for Split Cayley Hexagon");

      # The generators of G2(q) were taken from Hendrik 
      # van Maldeghem's book: "Generalized Polygons".
      # Lines with ** are where there are mistake's in
      # Hendrik's book (see Alan Offer's thesis).
      
      f := hexagon!.basefield;
      q := Size(f);
      if IsOddInt(Size(f)) then
         m:=[[ 0, 0, 0, 0, 0, 1, 0],
             [ 0, 0, 0, 0, 0, 0, 1],
             [ 0, 0, 0, 0, 1, 0, 0],
             [ 0, 0, 0, -1, 0, 0, 0],               ## **
             [ 0, 1, 0, 0, 0, 0, 0],
             [ 0, 0, 1, 0, 0, 0, 0],
             [ 1, 0, 0, 0, 0, 0, 0]]*One(f);  
         ConvertToMatrixRep(m, f);
         mp:=d->[[1,  0,  0,  0,  0,  d,  0],  
             [0,  1,  0,  0, -d,  0,  0],  
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  -2*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  -d,  0,  0,  1]]*One(f);   ## **
         ml:=d->[[1, -d,  0,  0,  0,  0,  0],  
             [0,  1,  0,  0,  0,  0,  0],  
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  0,  1,  0,  0,  0],  
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  d,  1,  0],  
             [0,  0,  0,  0,  0,  0,  1]]*One(f);
  
         nonzerof := AsList(f){[2..Size(f)]};
         gens := Union([m], List(nonzerof, mp), List(nonzerof, ml));
         for x in gens do
            ConvertToMatrixRep(x, f);
         od;
         frob := FrobeniusAutomorphism(f); 
         newgens := List(gens, x -> CollineationOfProjectiveSpace(x, f));  
         if not IsPrimeInt(Size(f)) then 
            Add(newgens, CollineationOfProjectiveSpace( IdentityMat(7,f), frob, f )); 
         fi; 
         coll := GroupWithGenerators(newgens);
       else
          ## Here we embed the hexagon in W(5,q)
          m:=[[ 0, 0, 0, 0, 1, 0],
              [ 0, 0, 0, 0, 0, 1],
              [ 0, 0, 0, 1, 0, 0],
              [ 0, 1, 0, 0, 0, 0],
              [ 0, 0, 1, 0, 0, 0],
              [ 1, 0, 0, 0, 0, 0]]*One(f);  
            ConvertToMatrixRep( m );
          mp:=d->[[1,  0,  0,  0,  d,  0],  
              [0,  1,  0,  d,  0,  0],  
              [0,  0,  1,  0,  0,  0],  
              [0,  0,  0,  1,  0,  0],  
              [0,  0,  0,  0,  1,  0],  
              [0,  0,d^2,  0,  0,  1]]*One(f);  
          ml:=d->[[1,  d,  0,  0,  0,  0],  
              [0,  1,  0,  0,  0,  0],  
              [0,  0,  1,  0,  0,  0],  
              [0,  0,  0,  1,  0,  0],  
              [0,  0,  0,  d,  1,  0],  
              [0,  0,  0,  0,  0,  1]]*One(f);
          nonzerof := AsList(f){[2..Size(f)]};
          gens := Union([m], List(nonzerof,mp), List(nonzerof,ml));

          for x in gens do
             ConvertToMatrixRep(x,f);
          od;
          frob := FrobeniusAutomorphism(f); 
          newgens := List(gens, x -> CollineationOfProjectiveSpace(x, f));  
          if not IsPrimeInt(Size(f)) then 
             Add(newgens, CollineationOfProjectiveSpace( IdentityMat(6,f), frob, f )); 
          fi;
          coll := GroupWithGenerators(newgens);

       fi; 

       Info(InfoFinInG, 1, "Computing nice monomorphism...");
       orblen := (q+1)*(q^4+q^2+1);
       rep := RepresentativesOfElements(hexagon)[1];
       domain := Orb(coll, rep, OnProjSubspaces, 
                  rec(orbsizelimit := orblen, hashlen := 2*orblen, storenumbers := true));
       Enumerate(domain);
       Info(InfoFinInG, 1, "Found permutation domain...");
	 hom := OrbActionHomomorphism(coll, domain);    
 	 SetIsBijective(hom, true);
	 SetNiceObject(coll, Image(hom) );
       SetNiceMonomorphism(coll, hom );

       SetCollineationAction(coll, OnProjSubspaces);
       if IsPrime(Size(f)) then
          SetName(coll, Concatenation("G_2(",String(Size(f)),")") );
       else
          SetName(coll, Concatenation("G_2(",String(Size(f)),").", String(Order(frob))) );
       fi;

   fi;
   return coll;
  end );




span := function(p,q)
local class, obj, geo, list, S, coset, r;
if not p!.geo = q!.geo then
    Error("<p> and <q> should belong to the same ambient geometry");
fi;
if p = q then
    return fail;
fi;
if p!.class = 1 and q!.class = 1 then
    S := First(f,x->p!.obj*q!.obj^-1 in x);
    if S = fail then
        return fail;
    else
        coset := RightCoset(S,p!.obj);
        r := CanonicalRightCosetElement(S,Representative(coset));
        return Wrap(p!.geo,2,1,[S,r]);
    fi;
elif p!.class = 1 and q!.class = 2 then
    if p!.obj in RightCoset(q!.obj[1],q!.obj[2]) then
        S := First(f,x->IsSubgroup(q!.obj[1],x));
        coset := RightCoset(S,p!.obj);
        r := CanonicalRightCosetElement(S,Representative(coset));
        return Wrap(p!.geo,2,1,[S,r]);
    else
        return fail;
    fi;
elif p!.class = 1 and q!.class = 3 then
    return fail;
elif p!.class = 2 and q!.class = 2 then
    if p!.obj[1] = q!.obj[1] then
        return Wrap(p!.geo,2,2,p!.obj[1]);
    else
        return fail;
    fi;
elif p!.class = 2 and q!.class = 3 then
    return Wrap(p!.geo,2,2,p!.obj[1]);
else
    return span(q,p);
fi;
end;

meet := function(l,m)
local class, obj, geo, Sl, Sm, S, coset, r, prod, g, h;
if not l!.geo = m!.geo then
    Error("<l> and <m> should belong to the same ambient geometry");
fi;
if l = m then
    return fail;
fi;
if l!.class = 1 and m!.class = 1 then
    Sl := l!.obj[1];
    Sm := m!.obj[1];
    if Sl = Sm then
        S := First(fstar, x->IsSubgroup(x,Sl));
        coset := RightCoset(S,l!.obj[2]);
        r := CanonicalRightCosetElement(S,Representative(coset));
        return Wrap(l!.geo,1,2,[S,r]);
    else
        prod := Group(Concatenation(GeneratorsOfGroup(Sl),GeneratorsOfGroup(Sm)));
        g := l!.obj[2];
        h := m!.obj[2];
        if not g*h^-1 in prod then
            return fail;
        else
			r := Intersection(RightCoset(Sl,g),RightCoset(Sm,h));
			return Wrap(l!.geo,1,1,r[1]);
        fi;
    fi;
elif l!.class = 1 and m!.class = 2 then
    if IsSubgroup(m!.obj,l!.obj[1]) then
        coset := RightCoset(m!.obj,l!.obj[2]);
        r := CanonicalRightCosetElement(m!.obj,Representative(coset));
        return Wrap(l!.geo,1,2,[m!.obj,r]);
    else
        return fail;
    fi;
elif l!.class = 2 and m!.class = 2 then
    return Wrap(l!.geo,1,3,0);
else
	return meet(m,l);
fi;
end;



#############################################################################
#O  Span( <x>, <y>  )
# return the line spanned by <x> and <y>, if they span a line at all.
##
#InstallMethod( Span,
#    "for two elements of a generalised polygon",
#    [ IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon ],
#    function( x, y )
#        local graph, vn, el, i, j, span;
#        if not x!.type = 1 and y!.type = 1 then
#            Error("<x> and <y> must be points of a generalised polygon");
#        elif not x!.geo = y!.geo then
#            Error("<x> and <y> must belong to the same generalised polygon");
#        fi;
#        graph := IncidenceGraphOfGeneralisedPolygon(x!.geo);
#        vn := VertexNames(graph);
#        if HasGraphWithUnderlyingObjectsAsVertices(x!.geo) then
#            i := Position(vn,x!.obj);
#            j := Position(vn,y!.obj);
#            el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
#            if not Length(el) = 0 then
#                span := vn{el};
#                return Wrap(x!.geo,2,span[1]);
#            else
#                Info(InfoFinInG, 1, "<x> and <y> do not span a line of gp");
#                return fail;
#            fi;
#        else
#            i := Position(vn,x);
#            j := Position(vn,y);
#            el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
#            if not Length(el) = 0 then
#                span := vn{el};
#                return span[1];
#            else
#                Info(InfoFinInG, 1, "<x> and <y> do not span a line of gp");
#                return fail;
#            fi;
#        fi;
#    end );


#############################################################################
#O  Meet( <x>, <y>  )
# return the line spanned by <x> and <y>, if they span a line at all.
##
#InstallMethod( Meet,
#    "for two elements of a generalised polygon",
#    [ IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon ],
#    function( x, y )
#        local graph, vn, el, i, j, meet;
#        if not x!.type = 2 and y!.type = 2 then
#            Error("<x> and <y> must be lines of a generalised polygon");
#        elif not x!.geo = y!.geo then
#            Error("<x> and <y> must belong to the same generalised polygon");
#        fi;
#        graph := IncidenceGraphOfGeneralisedPolygon(x!.geo);
#        vn := VertexNames(graph);
#        if HasGraphWithUnderlyingObjectsAsVertices(x!.geo) then
#            i := Position(vn,x!.obj);
#            j := Position(vn,y!.obj);
#            el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
#            if not Length(el) = 0 then
#                meet := vn{el};
#                return Wrap(x!.geo,2,meet   [1]);
#            else
#                Info(InfoFinInG, 1, "<x> and <y> do meet in a common point of gp");
#                return fail;
#            fi;
#        else
#            i := Position(vn,x);
#            j := Position(vn,y);
#            el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
#            if not Length(el) = 0 then
#                meet := vn{el};
#                return meet[1];
#            else
#                Info(InfoFinInG, 1, "<x> and <y> do meet in a common point of gp");
#                return fail;
#            fi;
#        fi;
#    end );



q := 3;
o := One(GF(q));
pg := PG(20,q);
vec := List([1..21],x->Zero(GF(q)));
vec1 := ShallowCopy(vec);
vec1[7] := o;
vec1[16] := o;
vec2 := ShallowCopy(vec);
vec2[3] := o;
vec2[21] := o;
vec3 := ShallowCopy(vec);
vec3[12] := o;
vec3[19] := o;
vec4 := ShallowCopy(vec);
vec4[1] := o;
vec4[18] := o;
vec5 := ShallowCopy(vec);
vec5[2] := o;
vec5[17] := -o;
vec6 := ShallowCopy(vec);
vec6[8] := o;
vec6[20] := -o;
hyp1 := HyperplaneByDualCoordinates(pg,vec1);
hyp2 := HyperplaneByDualCoordinates(pg,vec2);
hyp3 := HyperplaneByDualCoordinates(pg,vec3);
hyp4 := HyperplaneByDualCoordinates(pg,vec4);
hyp5 := HyperplaneByDualCoordinates(pg,vec5);
hyp6 := HyperplaneByDualCoordinates(pg,vec6);
sub := Meet([hyp1,hyp2,hyp3,hyp4,hyp5,hyp6]);

gram := List([1..7],x->List([1..7],y->Zero(GF(q))));
gram[1][5] := o;
gram[2][6] := o;
gram[3][7] := o;
gram[4][4] := -o;
form := QuadraticFormByMatrix(gram,GF(q));
ps := PolarSpace(form);
lines := AsList(Lines(ps));

gras := GrassmannMap(1,PG(6,q));
hqlines := Filtered(lines,x->x^gras in sub);;
hqpts := AsList(Points(ps));;
group := CollineationGroup(ps);
stab := FiningSetwiseStabiliser(group,hqlines);

gp := GeneralisedPolygonByElements(Set(hqpts),Set(hqlines),\*,stab,OnProjSubspaces);
CollineationGroup(gp);
time; #few minutes for q=5.

hq := SplitCayleyHexagon(q);
lines := List(Lines(hq));;
gras := GrassmannMap(1,PG(6,q));
pts := List(lines,x->x^gras);;

#HAll plane of order q^2.

q := 3;
pg1 := PG(1,q^2);
em := NaturalEmbeddingByFieldReduction(pg1,GF(q));
spread := List(Points(pg1),x->x^em);
klein := KleinCorrespondence(q);
ps := AmbientGeometry(Range(klein));
plane := Span([spread[1]^klein,spread[2]^klein,spread[3]^klein]);
conic := Filtered(Points(plane),x->x in ps);
plane2 := plane^PolarityOfProjectiveSpace(ps);
conic2 := Filtered(Points(plane2),x->x in ps);
regulus := List(conic,x->PreImageElm(klein,x));
switch := List(conic2,x->PreImageElm(klein,x));
hall_spread := Union(Difference(spread,regulus),switch);
pg := PG(4,q);
inf := HyperplaneByDualCoordinates(pg,[1,0,0,0,0]*Z(q)^0);
em2 := NaturalEmbeddingBySubspace(PG(3,q),pg,inf);
inf_pts := List(hall_spread,x->x^em2);
stab1 := FiningStabiliser(CollineationGroup(pg),inf);
stab2 := FiningSetwiseStabiliser(stab1,inf_pts);
affine_pts := Filtered(Points(pg),x->not x in inf);;
pts := Union(affine_pts,inf_pts);;
affine_lines := Union(List(inf_pts,x->Filtered(Planes(x),y->not y in inf)));;
lines := Union(affine_lines,[inf]);;
gp := GeneralisedPolygonByElements(pts,lines,\*,stab2,OnProjSubspaces);
CollineationGroup(gp); #pm 90 minutes on mac book air.


# an octagon?

ps := SymplecticSpace(3,2);
pts := List(Points(ps));
lines := List(Lines(ps));
flags := Union(List(pts,x->List(Lines(x),y->[x,y])));
inc := function(x,y)
if x = y then
    return true;
elif IsList(x) then
    return y in x;
else
    return x in y;
fi;
end;
apts := Union(pts,lines);
gp := GeneralisedPolygonByElements(apts,flags,inc);

ps := SymplecticSpace(3,2);
pts := List(Points(ps));
lines := List(Lines(ps));
flags := Union(List(pts,x->List(Lines(x),y->FlagOfIncidenceStructure(ps,[x,y]))));
inc := function(x,y)
if x = y then
    return true;
elif IsFlagOfIncidenceStructure(x) and IsElementOfIncidenceStructure(y) then
    return IsIncident(x,y);
elif IsElementOfIncidenceStructure(x) and IsElementOfIncidenceStructure(y) then
    return false;
elif IsFlagOfIncidenceStructure(x) and IsFlagOfIncidenceStructure(y) then   
    return false;
else 
    return inc(y,x);
fi;
end;
apts := Union(pts,lines);
gp := GeneralisedPolygonByElements(apts,flags,inc);


##### voorbeeldje Philippe

twos:=[];
Orbit(Group((1,2,3,4)),[2,0,0,0], Permuted);
Perform(last, function(i) Append(twos,[i, -1*i]); end);

cell:=function(v)
local n, cellset;
 cellset:=[];
 for n in Difference(twos, [v,-1*v]) do
   Add(cellset, (n+v)/2);
 od;
 return cellset;
end;

eles:=List(twos, cell);;
type1:=Union(eles));;
type1:= List(type1, v -> [v]);;

tetra:=Tuples([0,1],4);;
tcells:=List(tetra, t -> twos{[t[1]+1,3+t[2],5+t[3],7+t[4]]});;

List(tcells, tc -> List(Combinations(tc,2), e-> Sum(e)/2));;
Append(eles, last);;

prefaces:=List(Combinations(eles,2), Intersection);;
type3:=Filtered(prefaces, p -> Size(p)=3);;
Append(eles, Union(List(type3, f -> Combinations(f,2))));;
Append(eles, type3);;
Append(eles, type1);;

therelation:= function(x,y)
 return (IsSubset(x,y) or IsSubset(y,x));
end;

thetype:=function(x)
 return Dimension(VectorSpace(Rationals, x));
end;

struc:=IncidenceStructure(eles, therelation, thetype, [1,2,3,4]);

# Now I have a (combinatorial) incidence structure

ElementsOfIncidenceStructure(struc, 3);;
f:=Random(last);
ShadowOfElement(struc,f,1);
ShadowOfElement(struc,f,2);
e:=Random(last);;
ShadowOfElement(struc,f,3); # shadow on its own type is itself of course
f = Random(last);
ShadowOfElement(struc,f,4);

flag:=FlagOfIncidenceStructure(struc, [f,e]);
Type(flag);
ShadowOfFlag(struc,flag,1);
g := Random(last);
IsIncident(g, flag);
ShadowOfFlag(struc,flag,2);
last = Filtered(ElementsOfFlag(flag), e -> Type(e)=2);
AmbientGeometry(flag);
struc = last;

IsIncident(ElementsOfFlag(flag)[1], ElementsOfFlag(flag)[2]);

resi:=ResidueOfFlag(flag);

List(Cartesian(ElementsOfIncidenceStructure(resi,1),
 ElementsOfIncidenceStructure(resi,2)), x -> IsIncident(x[1],x[2]));  

# Great! We have a generalized digon

emptyflag:=FlagOfIncidenceStructure(struc,[]);
ElementsOfFlag(emptyflag);
AmbientGeometry(emptyflag);
Type(emptyflag);
Rank(emptyflag);
Rank(flag);

gr:=IncidenceGraph(resi);;
LocalInfo(gr, 1);

#Try all residues
combis:=Combinations([1..4],2);;
for pair in combis do
  fl:=[Random(ElementsOfIncidenceStructure(struc,pair[1]))];
  Add(fl, Random(ShadowOfElement(struc,fl[1], pair[2])));
  res:=ResidueOfFlag(FlagOfIncidenceStructure(struc,fl));
  ig:=IncidenceGraph(res);
  np:=NrElementsOfIncidenceStructure(res,1);
  locp:=LocalInfo(ig,1);
  locl:=LocalInfo(ig,np+1); # first line
  Print("edge ", pair,": g = ", locp.localGirth/2,", dp = ", 
locp.localDiameter, ", dl = ", locl.localDiameter, "\n");
od;


gamma:=IncidenceGraph(struc);;
aut:=AutomorphismGroup(gamma); 

pabs:=[Stabilizer(aut, 1)];
Append(pabs, List([1..Rank(struc)-1], i -> Stabilizer(aut, Sum([1..i],
j -> NrElementsOfIncidenceStructure(struc, j))+1)));;

cos:=CosetGeometry(aut, pabs);;
diag:=DiagramOfGeometry(cos);;

DrawDiagram(diag, "rara");

Exec("gv rara.ps");

# To show how generic we are...

pp:=PG(2,5);
ig:=IncidenceGraph(pp);; # Could make a much faster method here since we know the group
LocalInfo(ig,1); # a point
LocalInfo(ig,32); # a line

# Generalised triangle!

gp:=SplitCayleyHexagon(5);  
incgr:=IncidenceGraph(gp);;  # Have a coffee or two...
LocalInfo(incgr, 1);  
NrElementsOfIncidenceStructure(gp,1);
LocalInfo(incgr, 3907);

