##############################################################################
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

#############################################################################
# already some general comments.
# SubgeometryOfProjectiveSpaceByFrame: creates the user defined sub geometry
# starting from a frame. sub!.projectivity: projectivity mapping the canonical sub
# geometry onto the user defined one.
# sub!.sigma: collineation fixing all elements of the sub geometry.
#
#############################################################################


Print(", subgeometries\c");

InstallMethod( ViewObj,
    "for a subgeometry of a projective space",
    [ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
    function(pg)
        Print("Subgeometry PG(",pg!.dimension,", ",Size(pg!.subfield),") of ",ViewString(pg!.ambientspace));
end );

InstallMethod( ViewString,
    "for a subgeometry of projective space",
    [ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
    function( pg )
    return Concatenation("Subgeometry PG(",String(pg!.dimension),", ",String(Size(pg!.subfield)),") of ",ViewString(pg!.ambientspace));
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
    [ IsProjectiveSpace, IsField and IsFinite],
    function(pg,subfield)
    local geo, subpg, d, frame, ty, em, sigma, h, t, p, frob, q;
    d := ProjectiveDimension(pg);
    q := Size(subfield);
    p := Characteristic(GF(q));
    h := Log(q,p);
    t := Log(Size(pg!.basefield),p);
    if not t mod h = 0 then
        Error(" <subfield> is not a subfield of the base field of <pg>");
    fi;
    subpg := ProjectiveSpace(d,subfield);
    frame := StandardFrame(pg);
    em := NaturalEmbeddingBySubfield(subpg,pg);
    frob := FrobeniusAutomorphism(BaseField(pg))^h;
    sigma := CollineationOfProjectiveSpace(pg,frob);
    geo := rec(dimension := d, basefield := pg!.basefield, subfield := GF(q), ambientspace := pg, isomorphicsubgeometry := subpg, frame := frame,
        embedding := em, vectorspace := FullRowSpace(pg!.basefield, d+1), sigma := sigma );
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
    [ IsProjectiveSpace, IsList, IsField and IsFinite],
    function(pg,frame,subfield)
    local geo, subpg, d, ty, matrix, proj, n, i, vecs, basis, coefs, em, sigma, h, t, p, frob, q, can;
    if not IsFrameOfProjectiveSpace(frame) then
        return("<frame> must be a frame of <pg>");
    fi;
    d := ProjectiveDimension(pg);
    q := Size(subfield);
    p := Characteristic(GF(q));
    h := Log(q,p);
    t := Log(Size(pg!.basefield),p);
    if not t mod h = 0 then
        Error(" <subfield> is not a subfield of the base field of <pg>");
    fi;
    subpg := ProjectiveSpace(d,subfield);
    frob := FrobeniusAutomorphism(BaseField(pg))^h;
    if ForAll(frame,y->ForAll(Flat(y!.obj),x->x in subfield)=true) then
        can := true;
        sigma := CollineationOfProjectiveSpace(pg,frob);
    else
        can := false;
        n:=Size(frame)-1;
        vecs:=List(frame,x->Coordinates(x));
        basis:=Basis(UnderlyingVectorSpace(Span(frame)),vecs{[1..n]});
        coefs:=Coefficients(basis,vecs[n+1]);
        matrix := List([1..n],i->coefs[i]*vecs[i]);
        proj := CollineationOfProjectiveSpace(matrix,BaseField(pg));
        sigma := proj^(-1)*CollineationOfProjectiveSpace(pg,frob)*proj;
    fi;
    em := NaturalEmbeddingBySubfield(subpg,pg);
    geo := rec(dimension := d, basefield := pg!.basefield, subfield := GF(q), ambientspace := pg, isomorphicsubgeometry := subpg,
        frame := ShallowCopy(frame), embedding := em, vectorspace := FullRowSpace(pg!.basefield, d+1), sigma := sigma );
    if not can then
        geo.projectivity := proj;
    fi;
    ty := NewType( SubgeometriesFamily,
                  IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, pg);
    SetIsCanonicalSubgeometryOfProjectiveSpace(geo, can);
    #SetRankAttr(geo,d);
    return geo;
    end );

InstallMethod( \=,
    "for two subgeometries of a projective space",
	[ IsSubgeometryOfProjectiveSpace, IsSubgeometryOfProjectiveSpaceRep ],
    function(sub1,sub2)
    local proj1, proj2, res, mat;
    if not AmbientSpace(sub1) = AmbientSpace(sub2) then
        return false;
    elif IsCanonicalSubgeometryOfProjectiveSpace(sub1) = false or IsCanonicalSubgeometryOfProjectiveSpace(sub2) = false then
        proj1 := sub1!.projectivity;
        proj2 := sub2!.projectivity;
        res := proj1 * proj2^(-1);
        mat := MatrixOfCollineation(res);
        return ForAll(Flat(mat), x->x in sub1!.subfield);
    else
        return true;
    fi;
    end );

InstallMethod( \=, 
	"for two projective spaces",
	[IsSubgeometryOfProjectiveSpace, IsProjectiveSpace],
	function(pg1,pg2);
		return false;
	end );

InstallMethod( \=, 
	"for two projective spaces",
	[IsProjectiveSpace, IsSubgeometryOfProjectiveSpace],
	function(pg1,pg2);
		return false;
	end );

InstallMethod( CanonicalSubgeometryOfProjectiveSpace,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsPosInt],
    function(pg,q)
        return CanonicalSubgeometryOfProjectiveSpace(pg,GF(q));
    end );

InstallMethod( SubgeometryOfProjectiveSpaceByFrame,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsList, IsPosInt],
    function(pg,frame,q)
        return SubgeometryOfProjectiveSpaceByFrame(pg,frame,GF(q));
    end );

InstallMethod( Rank,
	"for a projective space",
	[ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
	ps -> ps!.dimension
	);

InstallMethod( BaseField, 
	"for an element of a projective space", 
	[IsSubspaceOfProjectiveSpace],
	sub -> sub!.basefield );

InstallMethod( SubFieldOFSubGeometry,
	"for a subgeometry of a projective space",
	[ IsSubgeometryOfProjectiveSpace ],
	sub -> sub!.subfield );

InstallMethod( UnderlyingVectorSpace,
	"for a subspace of subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace ],
    function(el)
        Error(" <el> is a subspace of a subgeometry of a projective space");
    end );

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

InstallMethod( \in,
	"for two subspaces of a subgeometry",
	[IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
    if x!.geo = y!.geo then
        return ExtendElementOfSubgeometry(x) in ExtendElementOfSubgeometry(y);
    else
        Error("<x> and <y> do not belong to the same geometry");
    fi;
    end );

InstallMethod( \in,
	"for a subspace of a subgeometry and a subspace of a projective space",
	[IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

InstallMethod( \in,
	"for a subspace of a projective space and a subspace of a subgeometry",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

InstallMethod( \in,
	"for an element of a subgeometry of a projective space and a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace, IsProjectiveSpace],
	function( element, ps )
        if element!.geo = ps then
            return true;
        else
            Error(" <element> is not an element of <ps>");
        fi;
    end );

InstallMethod( \in,
	"for an element of a subgeometry of a projective space and a projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubgeometryOfProjectiveSpace],
	function( element, ps )
       return element!.geo = ps;
    end );

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
		Objectify( NewType( SoSoPSFamily, IsElementOfIncidenceStructure and
			IsElementOfIncidenceStructureRep and IsSubspaceOfSubgeometryOfProjectiveSpace ), w );
		return w;
	end );

InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a matrix as plist",
	[IsSubgeometryOfProjectiveSpace, IsPlistRep and IsMatrix],
	function( geom, v )
    return VectorSpaceToElementForSubgeometries(geom,v);
    end );

InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a CMatRep",
	[ IsSubgeometryOfProjectiveSpace, IsCMatRep],
	function( geom, v )
	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
	end );

InstallMethod( VectorSpaceToElement, 
	"for a subgeometry of a projective space and a compressed GF(2)-matrix",
	[IsSubgeometryOfProjectiveSpace, IsGF2MatrixRep],
	function( geom, v )
	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
	end );

InstallMethod( VectorSpaceToElement, 
	"for a subgeometry of a projective space and a compressed basis of a vector subspace",
	[IsSubgeometryOfProjectiveSpace, Is8BitMatrixRep],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
    end );

InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a row vector as cvec",
	[IsSubgeometryOfProjectiveSpace, IsCVecRep],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
    end );

InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a row vector",
	[IsSubgeometryOfProjectiveSpace, IsRowVector],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, v);
    end );

InstallMethod( VectorSpaceToElement, 
	"for a subgeometry of a projective space and an 8-bit vector",
	[IsSubgeometryOfProjectiveSpace, Is8BitVectorRep],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
    end );

InstallMethod( VectorSpaceToElementForSubgeometries,
	"for a sub geometry of a projective space and an object",
	[ IsSubgeometryOfProjectiveSpace, IsObject],
	function( sub, obj )
        local ambient, element, newelement, proj, subfield;
        ambient := sub!.ambientspace;
        element := VectorSpaceToElement(ambient,obj);
        if not IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            proj := sub!.projectivity;
            newelement := element^(proj^(-1));
        else
            newelement := element;
        fi;
        subfield := sub!.subfield;
        if not ForAll( Flat( newelement!.obj ), i -> i in subfield) then
            Error( "<obj> does not determine an element in <sub>");
        else
            return Wrap(sub,element!.type,UnderlyingObject(element));
        fi;
    end);

InstallMethod( ExtendElementOfSubgeometry,
	"for an element of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace ],
	element -> VectorSpaceToElement(AmbientSpace(element),Unpack(UnderlyingObject(element))));

InstallMethod( Span,
	"for two elements of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace,  IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
    local z,w,pg,span;
    if x!.geo = y!.geo then
        pg := AmbientSpace(x);
        z := Embed(pg,x);
        w := Embed(pg,y);
        span := Span(z,w);
        if (ProjectiveDimension(span) = ProjectiveDimension(x!.geo)) then
            return x!.geo;
        else
            return Wrap(x!.geo,span!.type,UnderlyingObject(span));
        fi;
    else
        Error( "<x> and <y> do not belong to the same geometry" );
    fi;
    end );

InstallMethod( Span, "for a homogeneous list of subspaces of a subgeometry of a projective space",
	[ IsHomogeneousList and IsSubspaceOfSubgeometryOfProjectiveSpaceCollection ],
	function( l )  
		local list, span, pg, same;
		# first we check that all items in the list belong to the same ambient geometry
		if Length(l)=0 then 
			return [];
		elif not Size(AsDuplicateFreeList(List(l,x->AmbientGeometry(x))))=1 then
			Error("The elements in the list do not belong to the same ambient geometry");
		else
            pg := AmbientSpace(l[1]);
            list := List(l,x->Embed(pg,x));
            span := Span(list);
            same := Size(AsDuplicateFreeList(List(l,x->AmbientGeometry(x))))=1;
            if (ProjectiveDimension(span) = ProjectiveDimension(l[1]!.geo)) then
                return l[1]!.geo;
            else
                return Wrap(l[1]!.geo,span!.type,UnderlyingObject(span));
            fi;
        fi;
    end );

InstallMethod( Span,
	"for a subspace of a projective space and a subspace of a subgeometry",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

InstallMethod( Span,
	"for a subspace of a subgeometry and a subspace of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

InstallMethod( Span,
	"for an element of a subgeometry of a projective space and a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace, IsProjectiveSpace],
	function( element, ps )
        if element!.geo = ps then
            return ps;
        else
            Error( "<element> does not belong to <ps>");
        fi;
    end );

InstallMethod( Span,
	"for a projective space and an element of a subgeometry of a projective space",
	[ IsProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
	function( ps, element )
        if element!.geo = ps then
            return ps;
        else
            Error( "<element> does not belong to <ps>");
        fi;
    end );

InstallMethod( Meet,
	"for two elements of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace,  IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
    local z,w,pg,meet;
    if x!.geo = y!.geo then
        pg := AmbientSpace(x);
        z := Embed(pg,x);
        w := Embed(pg,y);
        meet := Meet(z,w);
        if IsEmptySubspace(meet) then
            return EmptySubspace(x!.geo);
        else
            return Wrap(x!.geo,meet!.type,UnderlyingObject(meet));
        fi;
    else
        Error( "<x> and <y> do not belong to the same geometry" );
    fi;
    end );

InstallMethod( Meet,
    "for a homogeneous list of subspaces of a subgeometry of a projective space",
	[ IsHomogeneousList and IsSubspaceOfSubgeometryOfProjectiveSpaceCollection ],
	function( l )  
		local list, meet, pg, same;
		# first we check that all items in the list belong to the same ambient geometry
		if Length(l)=0 then 
			return [];
		elif not Size(AsDuplicateFreeList(List(l,x->AmbientGeometry(x))))=1 then
			Error("The elements in the list do not belong to the same ambient geometry");
		else
            pg := AmbientSpace(l[1]);
            list := List(l,x->Embed(pg,x));
            meet := Meet(list);
            same := Size(AsDuplicateFreeList(List(l,x->AmbientGeometry(x))))=1;
            if IsEmptySubspace(meet) then
                return EmptySubspace(l[1]!.geo);
            else
                return Wrap(l[1]!.geo,meet!.type,UnderlyingObject(meet));
            fi;
        fi;
    end );

InstallMethod( Meet,
	"for a subspace of a projective space and a subspace of a subgeometry",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

InstallMethod( Meet,
	"for a subspace of a subgeometry and a subspace of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

InstallMethod( Meet,
	"for an element of a subgeometry of a projective space and a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace, IsProjectiveSpace],
	function( element, ps )
        if element!.geo = ps then
            return element;
        else
            Error( "<element> does not belong to <ps>");
        fi;
    end );

InstallMethod( Meet,
	"for a projective space and an element of a subgeometry of a projective space",
	[ IsProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
	function( ps, element )
        if element!.geo = ps then
            return element;
        else
            Error( "<element> does not belong to <ps>");
        fi;
    end );

#use sigma to compute the intersection of an element of the ambein
InstallMethod( Meet,
    "for a subgeometry of a projectice space and a subspace of the ambient space",
    [ IsSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function( sub, el)
        local sigma,meet;
        if not AmbientSpace(sub) = AmbientGeometry(el) then
            Error( "the ambient space of <sub> is not the ambient geometry of <el>");
        else
            sigma := sub!.sigma;
            meet := Meet(el,el^sigma);
            if not IsEmptySubspace(meet) then
                return Wrap(sub,meet!.type,meet!.obj);
            else
                return EmptySubspace(sub);
            fi;
        fi;
    end );

InstallMethod( FlagOfIncidenceStructure,
	"for a projective space and list of subspaces of the projective space",
	[ IsSubgeometryOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpaceCollection ],
	function(ps,els)
		local list,i,test,type,flag;
		list := Set(ShallowCopy(els));
		if Length(list) > Rank(ps) then
		  Error("A flag can contain at most Rank(<ps>) elements");
		fi;
        test := List(list,x->AmbientGeometry(x));
        if not ForAll(test,x->x=ps) then
            Error("not all elements have <ps> as ambient space");
        fi;
        test := Set(List([1..Length(list)-1],i -> IsIncident(list[i],list[i+1])));
		if (test <> [ true ] and test <> []) then
		  Error("<els> do not determine a flag");
		fi;
		flag := rec(geo := ps, types := List(list,x->x!.type), els := list, vectorspace := ps!.vectorspace );
		ObjectifyWithAttributes(flag, IsFlagOfPSType, IsEmptyFlag, false, RankAttr, Size(list) );
		return flag;
	end);

InstallMethod( ShadowOfElement, 
	"for a projective space, an element, and an integer",
	[IsSubgeometryOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace, IsPosInt],
	# returns the shadow of an element v as a record containing the projective space (geometry), 
	# the type j of the elements (type), the element v (parentflag), and some extra information
	# useful to compute with the shadows, e.g. iterator
	function( ps, v, j )
		local localinner, localouter, localfactorspace, tocanonical, vs, vcanonical;
        if not AmbientGeometry(v) = ps then
            Error("<v> is not a subspace of <ps>");
        fi;
        if j > ps!.dimension then
            Error("<ps> has no elements of type <j>");
        fi;
        if not IsCanonicalSubgeometryOfProjectiveSpace(ps) then
            tocanonical := (ps!.projectivity)^(-1);
            vcanonical := v^tocanonical;
        else
            vcanonical := v;
        fi;
        vs := ps!.isomorphicsubgeometry!.vectorspace;
        if j < v!.type then
			localinner := [];
			localouter := Unpack(vcanonical!.obj);
		elif j = v!.type then
            tocanonical := (ps!.proj)^(-1);
			localinner := Unpack(vcanonical!.obj);
			localouter := localinner;
		else
			localinner := Unpack(vcanonical!.obj);
			localouter := BasisVectors(Basis(vs));
		fi;
    	if IsVector(localinner) and not IsMatrix(localinner) then
			localinner := [localinner]; 
		fi;
		if IsVector(localouter) and not IsMatrix(localouter) then
			localouter := [localouter]; 
		fi;
		localfactorspace := Subspace(vs,
		BaseSteinitzVectors(localouter, localinner).factorspace);
		return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
							IsShadowSubspacesOfSubgeometryOfProjectiveSpace and
							IsShadowSubspacesOfSubgeometryOfProjectiveSpaceRep),
							rec( geometry := ps,
									type := j,
									inner := localinner,
									outer := localouter,
									factorspace := localfactorspace,
									parentflag := FlagOfIncidenceStructure(ps,[v]),
									size := Size(Subspaces(localfactorspace))
								)
						);
	end);

InstallMethod( Iterator, 
	"for shadow subspaces of a projective space",
	[IsShadowSubspacesOfSubgeometryOfProjectiveSpace and IsShadowSubspacesOfSubgeometryOfProjectiveSpaceRep ],
    function( vs )
        local j, d, F, act, sub, proj;
        sub := vs!.geometry;
        j := vs!.type;
        d := sub!.dimension;
        F := sub!.basefield;
        if IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            act := x->x;
        else
            proj := sub!.projectivity;
        #this should be just the action of the projectivity on x, see OnProjSubspacesNoFrob, we will take over
        #the code from there, OnProjSubspacesNoFrob expect that both arguments are cvec/cmat. Also note that
        #x is always a matrix.
            act := function(x)
                local mat;
                    mat := OnSubspacesByCanonicalBasis(x,Unpack(proj!.mat));
                        TriangulizeMat(mat);
                        return mat;
            end;
        fi;
        return IteratorByFunctions( rec(
        NextIterator := function(iter)
            local mat;
            mat := NextIterator(iter!.S);
            mat := MutableCopyMat(Concatenation(
                BasisVectors(Basis(mat)),
                    iter!.innermat
                ));
            return VectorSpaceToElement(sub,act(mat));
            end,
            IsDoneIterator := function(iter)
            return IsDoneIterator(iter!.S);
            end,
            ShallowCopy := function(iter)
            return rec(
                innermat := iter!.innermat,
                S := ShallowCopy(iter!.S)
            );
            end,
            innermat := vs!.inner,
            S := Iterator(Subspaces(vs!.factorspace,j-Size(vs!.inner)))
        ));
    end);
