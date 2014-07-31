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

