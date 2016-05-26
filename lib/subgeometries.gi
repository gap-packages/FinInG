#############################################################################
##
##  subgeometries.gi        FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2016	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for subgeometries of projective spaces.
##
#############################################################################

Print(", subgeometries\c");

InstallMethod( ViewObj,
    "for a subgeometry of a projective space",
    [ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
    function(pg)
    Print("Subgeometry PG(",pg!.dimension,", ",Size(pg!.basefield),") of ",ViewString(pg!.ambientspace));
end );

InstallMethod( IsFrameOfProjectiveSpace,
    "for a list of points",
    [ IsList ],
    function(list)
    local pg, coll, base, n, i;
    coll := Collected(List(list,x->AmbientSpace(x)));
    if Length(coll) > 1 then
        Error("all elements in <list> lie in the same projective space");
    else
        pg := coll[1][1];
    fi;
    coll := Collected(List(list,x->Type(x)));
    if Length(coll) > 1 then
        Error("all elements in <list> must be points");
    elif coll[1][1] <> 1 then
        Error("all elements in <list> must be points");
    fi;
    n := Length(list);
    if n <> ProjectiveDimension(pg) + 2 then
        Error("<list> does not contain the correct number of points");
    fi;
    for i in [1..n] do
        base := list{Difference([1..n],[i])};
        if not Span(base) = pg then
            Error("<list> is not a frame");
        fi;
    od;
    return true;
    end );

#note for the next two methods: it may look strange to put a vectorspace over the big field as vectorspace. But
#this makes sure that incidence can be tested between elements of the subgeometry and the ambient geometry.

InstallMethod( CanonicalSubgeometryOfProjectiveSpace,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsPosInt],
    function(pg,q)
    local geo, subpg, d, frame, ty, em, sigma, h, p, frob;
    d := ProjectiveDimension(pg);
    subpg := ProjectiveSpace(d,q);
    frame := StandardFrame(pg);
    em := NaturalEmbeddingBySubfield(subpg,pg);
    p := Characteristic(basefield);
    h := Log(Size(basefield)),p);

    geo := rec(dimension := d, basefield := GF(q), ambientspace := pg, isomorphicsubgeometry := subpg, frame := frame,
        embedding := em, vectorspace := FullRowSpace(BaseField(pg), d+1) );
    ty := NewType( SubgeometriesFamily,
                  IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep );
    Objectify( ty, geo );
    SetIsCanonicalSubgeometryOfProjectiveSpace(geo, true);
    SetAmbientSpace(geo, pg);
    #SetRankAttr(geo,d);
    return geo;
    end );

InstallMethod( SubgeometryOfProjectiveSpaceByFrame,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsList, IsPosInt],
    function(pg,frame,q)
    local geo, subpg, d, ty, matrix, proj, n, i, vecs, basis, coefs, em;
    if not IsFrameOfProjectiveSpace(frame) then
        return("<frame> must be a frame of <pg>");
    fi;
    d := ProjectiveDimension(pg);
    subpg := ProjectiveSpace(d,q);
    n:=Size(frame)-1;
    vecs:=List(frame,x->Coordinates(x));
    basis:=Basis(UnderlyingVectorSpace(Span(frame)),vecs{[1..n]});
    coefs:=Coefficients(basis,vecs[n+1]);
    matrix := List([1..n],i->coefs[i]*vecs[i]);
    proj := CollineationOfProjectiveSpace(matrix,BaseField(pg));
    em := NaturalEmbeddingBySubfield(subpg,pg);
    geo := rec(dimension := d, basefield := GF(q), ambientspace := pg, isomorphicsubgeometry := subpg, frame := ShallowCopy(frame),
        embedding := em, projectivity := proj, vectorspace := FullRowSpace(BaseField(pg), d+1) );
    ty := NewType( SubgeometriesFamily,
                  IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep );
    Objectify( ty, geo );
    SetIsCanonicalSubgeometryOfProjectiveSpace(geo, false);
    SetAmbientSpace(geo, pg);
    #SetRankAttr(geo,d);
    return geo;
    end );

InstallMethod( Rank,
	"for a projective space",
	[ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
	ps -> ps!.dimension
	);

InstallMethod( BaseField, 
	"for an element of a projective space", 
	[IsSubspaceOfProjectiveSpace],
	sub -> AmbientSpace(sub)!.basefield );

InstallMethod( ElementsOfIncidenceStructure, 
	"for a projective space and an integer",
	[IsSubgeometryOfProjectiveSpace, IsPosInt],
	function( ps, j )
		local r;
		r := Rank(ps);
		if j > r then
			Error("<ps> has no elements of type <j>");
		else
			return Objectify(
			NewType( ElementsCollFamily, IsSubspacesOfSubgeometryOfProjectiveSpace and IsSubspacesOfSubgeometryOfProjectiveSpaceRep ),
				rec( geometry := ps,
					type := j,
					size := Size(Subspaces(ps!.isomorphicsubgeometry!.vectorspace, j))
					)
					);
		fi;
	end);

InstallMethod(Iterator,
	"for subspaces of a projective space",
	[ IsSubspacesOfSubgeometryOfProjectiveSpace ],
	function( vs )
		local sub, isomorphicsubgeometry, canonicalelements, j, em, proj, map;
		sub := vs!.geometry;
        j := vs!.type;
        isomorphicsubgeometry := sub!.isomorphicsubgeometry;
        canonicalelements := ElementsOfIncidenceStructure(isomorphicsubgeometry,j);
        em := sub!.embedding;
        if IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            map := x->Wrap(sub,j,UnderlyingObject(x^em));
        else
            proj := sub!.projectivity;
            map := x->Wrap(sub,j,UnderlyingObject((x^em)^proj));
        fi;
        return IteratorByFunctions(
            rec(
			NextIterator := function(iter)
                local element;
                element := NextIterator(iter!.S);
                return map(element);
                end,

            IsDoneIterator := function(iter)
              return IsDoneIterator(iter!.S);
            end,

            ShallowCopy := function(iter)
              return rec(
                S := ShallowCopy(iter!.S)
                );
            end,
            S := Iterator(canonicalelements)
          ));
	end);

InstallMethod( Wrap, 
	"for a projective space and an object",
	[ IsSubgeometryOfProjectiveSpace, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and
			IsElementOfIncidenceStructureRep and IsSubspaceOfSubgeometryOfProjectiveSpace ), w );
		return w;
	end );

InstallMethod( VectorSpaceToElement,
	"for a sub geometry of a projective space and an object",
	[ IsSubgeometryOfProjectiveSpace, IsObject],
	function( sub, obj )
        local ambient, element, proj, subfield;
        ambient := sub!.ambientspace;
        element := VectorSpaceToElement(ambient,obj);
        if not IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            proj := sub!.projectivity;
            element := element^(proj^(-1));
        fi;
        subfield := BaseField(sub);
        if not ForAll( Flat( element!.obj ), i -> i in subfield) then
            Error( "<obj> does not determine an element in <sub>");
        else
            return Wrap(sub,element!.type,UnderlyingObject(element));
        fi;
    end);

InstallMethod( \in, 
	"for an element of a projective space and a subgeometry of a projective space",
	[IsElementOfLieGeometry, IsSubgeometryOfProjectiveSpace],
	function( element, sub )
        local subfield, proj;
        if AmbientSpace(element) <> AmbientSpace(sub) then
            Error("ambient space of <el> differs from <pg>");
        fi;
        if not IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            proj := sub!.projectivity;
            element := element^(proj^(-1));
        fi;
        subfield := BaseField(sub);
        return ForAll( Flat( element!.obj ), i -> i in subfield);
	end );

InstallMethod( ExtendElementOfSubgeometry,
	"for an element of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace ],
	element -> VectorSpaceToElement(AmbientSpace(element),Unpack(UnderlyingObject(element))));

InstallMethod( Span,
	"for two elements of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace,  IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
    local z,w,pg,span;
    pg := AmbientSpace(x);
    if not pg = AmbientSpace(y) then
        Error("ambient spaces of <x> and <y> differ");
    fi;
    z := Embed(pg,x);
    w := Embed(pg,y);
    span := Span(z,w); #we know already that span belongs to the subgeometry. So we may use wrap to avoid checking this again.
    #now we check whether the subgeometries are the same. If not, we just return span.
    if x!.geo = y!.geo then
        return Wrap(x!.geo,span!.type,UnderlyingObject(span));
    else
        return span;
    fi;
    end );

InstallMethod( Meet,
	"for two elements of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace,  IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
    local z,w,pg,meet;
    pg := AmbientSpace(x);
    if not pg = AmbientSpace(y) then
        Error("ambient spaces of <x> and <y> differ");
    fi;
    z := Embed(pg,x); #I am happy to use Embed ;-)
    w := Embed(pg,y);
    span := Meet(z,w); #we know already that span belongs to the subgeometry. So we may use wrap to avoid checking this again.
    #now we check whether the subgeometries are the same. If not, we just return span.
    if x!.geo = y!.geo then
        return Wrap(x!.geo,span!.type,UnderlyingObject(span));
    else
        return span;
    fi;
    end );

Install


