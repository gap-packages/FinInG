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
