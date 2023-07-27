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
##  Copyright 2020	Colorado State University, Fort Collins
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
# starting from a frame. sub!.projectivity: projectivity mapping the canonical
# sub geometry onto the user defined one.
# sub!.sigma: collineation fixing all elements of the sub geometry.
#
#############################################################################

## View methods
#############################################################################
#O  ViewObj( <pg> )
##
InstallMethod( ViewObj,
    "for a subgeometry of a projective space",
    [ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
    function(pg)
        Print("Subgeometry PG(",pg!.dimension,", ",Size(pg!.subfield),") of ",ViewString(pg!.ambientspace));
end );

#############################################################################
#O  ViewString( <pg> )
##
InstallMethod( ViewString,
    "for a subgeometry of projective space",
    [ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
    function( pg )
    return Concatenation("Subgeometry PG(",String(pg!.dimension),", ",String(Size(pg!.subfield)),") of ",ViewString(pg!.ambientspace));
end );

## operation to check whether a list of points is a frame of the ambient geometry
# of the points.
#############################################################################
#O  IsFrameOfProjectiveSpace( <list> )
##  change 22/6: IsSomething should return true or false!
InstallMethod( IsFrameOfProjectiveSpace,
    "for a list of points",
    [ IsList ],
    function(list)
    local pg, coll, base, n, i;
    coll:=DuplicateFreeList(List(list,x->AmbientGeometry(x)));
#    coll := Collected(List(list,x->AmbientGeometry(x))); #changed AmbientSpace into AmbientGeometry.
    if Length(coll) > 1 then
        #Error("all elements in <list> lie in the same projective space");
        return false;
    else
#       pg := coll[1][1];
        pg:= coll[1];
    fi;
    coll := Collected(List(list,x->Type(x)));
    if Length(coll) > 1 then
        #Error("all elements in <list> must be points");
        return false;
    elif coll[1][1] <> 1 then
        #Error("all elements in <list> must be points");
        return false;
    fi;
    n := Length(list);
    if n <> ProjectiveDimension(pg) + 2 then
        #Error("<list> does not contain the correct number of points");
        return false;
    fi;
    for i in [1..n] do
        base := list{Difference([1..n],[i])};
        if not Span(base) = pg then
            #Error("<list> is not a frame");
            return false;
        fi;
    od;
    return true;
    end );

#############################################################################
#O  RandomFrameOfProjectiveSpace( <pg> )
#   This method returns a random frame of a projective space
##
InstallMethod( RandomFrameOfProjectiveSpace,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace ],
    function(pg)
    local pts,mat,final,gauge,d,field;
    d := ProjectiveDimension(pg)+1;
    field := BaseField(pg);
    mat := RandomInvertibleMat(d,field);
    pts := List(mat,x->VectorSpaceToElement(pg,x));
    # TODO: start with the standard frame and choose a random projectivity (#ml 18 May 2020)
    gauge := Random(Points(pg));
    while not IsFrameOfProjectiveSpace(Union(pts,[gauge])) do
        gauge := Random(Points(pg));
    od;
    pts := Union(pts,[gauge]);
    return Set(pts);
    end );

## Constructor methods for subgeometries of projective spaces.
# note for the next two methods: it may look strange to put a vectorspace over
# the big field as vectorspace. But this makes sure that incidence can be
# tested between elements of the subgeometry and the ambient geometry.

#############################################################################
#O  CanonicalSubgeometryOfProjectiveSpace( <pg>, <subfield> )
#   This method requires a projective space and a subfield.
# small change on 12/9/16: we added eventually a field .projectivity in the
# returned geometry. From time to time this is useful, e.g. when comparing
# a canonical subgeometry with an arbitrary one, see e.g. code for \=
##
InstallMethod( CanonicalSubgeometryOfProjectiveSpace,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsField and IsFinite],
    function(pg,subfield)
    local geo, subpg, d, frame, ty, em, sigma, h, t, frob, proj;
    if IsSubgeometryOfProjectiveSpace(pg) then
        Error("recursive construction of subgeometries not (yet) possible");
    fi;
    d := ProjectiveDimension(pg);
    h := DegreeOverPrimeField(subfield);
    t := DegreeOverPrimeField(BaseField(pg));
    if not t mod h = 0 then
        Error(" <subfield> is not a subfield of the base field of <pg>");
    elif t = h then #We may consider to return an error here.
        return pg;
    fi;
    subpg := ProjectiveSpace(d,subfield);
    frame := StandardFrame(pg);
    em := NaturalEmbeddingBySubfield(subpg,pg);
    frob := FrobeniusAutomorphism(BaseField(pg))^h;
    sigma := CollineationOfProjectiveSpace(pg,frob);
    proj := CollineationOfProjectiveSpace(IdentityMat(d+1,pg!.basefield),pg!.basefield);
    geo := rec(dimension := d, basefield := pg!.basefield, subfield := subfield, ambientspace := pg, isomorphicsubgeometry := subpg, frame := frame,
        embedding := em, vectorspace := FullRowSpace(pg!.basefield, d+1), sigma := sigma, projectivity := proj );
    ty := NewType( SubgeometriesFamily,
                  IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep );
    Objectify( ty, geo );
    SetIsCanonicalSubgeometryOfProjectiveSpace(geo, true);
    SetAmbientSpace(geo, pg);
    SetDefiningFrameOfSubgeometry(geo,frame);
    #SetRankAttr(geo,d);
    return geo;
    end );

#############################################################################
#O  SubgeometryOfProjectiveSpaceByFrame( <pg>, <frame>, <subfield> )
#   It is checked whether <frame> is a frame. This operation requires
#   a projective space, a frame of this space, and a subfield.
##
InstallMethod( SubgeometryOfProjectiveSpaceByFrame,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsList, IsField and IsFinite],
    function(pg,frame,subfield)
    local geo, subpg, d, ty, matrix, proj, n, i, vecs, basis, coefs, em, sigma, h, t, frob, can;
    if not IsFrameOfProjectiveSpace(frame) then
        Error(" <frame> must be a frame of <pg>");
    fi;
     if IsSubgeometryOfProjectiveSpace(pg) then
        Error("recursive construction of subgeometries not (yet) possible");
    fi;
    d := ProjectiveDimension(pg);
    h := DegreeOverPrimeField(subfield);
    t := DegreeOverPrimeField(BaseField(pg));
    if not t mod h = 0 then
        Error(" <subfield> is not a subfield of the base field of <pg>");
    elif t = h then #We may consider to return an error here.
        return pg;
    fi;
    subpg := ProjectiveSpace(d,subfield);
    frob := FrobeniusAutomorphism(BaseField(pg))^h;
    if ForAll(frame,y->ForAll(Flat(y!.obj),x->x in subfield)=true) then
        can := true;
        proj := CollineationOfProjectiveSpace(IdentityMat(d+1	,pg!.basefield),pg!.basefield);
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
    geo := rec(dimension := d, basefield := pg!.basefield, subfield := subfield, ambientspace := pg, isomorphicsubgeometry := subpg,
        frame := ShallowCopy(frame), embedding := em, vectorspace := FullRowSpace(pg!.basefield, d+1), sigma := sigma,
        projectivity := proj );
    ty := NewType( SubgeometriesFamily,
                  IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, pg);
    SetIsCanonicalSubgeometryOfProjectiveSpace(geo, can);
    SetDefiningFrameOfSubgeometry(geo,frame);
    #SetRankAttr(geo,d);
    return geo;
    end );

#############################################################################
#O  CanonicalSubgeometryOfProjectiveSpace( <pg>, <q> )
#   shortcut to CanonicalSubgeometryOfProjectiveSpace( <pg>, <GF(<q>))
##
InstallMethod( CanonicalSubgeometryOfProjectiveSpace,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsPosInt],
    function(pg,q)
        return CanonicalSubgeometryOfProjectiveSpace(pg,GF(q));
    end );

#############################################################################
#O  SubgeometryOfProjectiveSpaceByFrame( <pg>, <frame>, <q> )
#   shortcut to SubgeometryOfProjectiveSpaceByFrame( <pg>, <frame>, <GF(<q>))
##
InstallMethod( SubgeometryOfProjectiveSpaceByFrame,
    "for a projective space, and a prime power",
    [ IsProjectiveSpace, IsList, IsPosInt],
    function(pg,frame,q)
        return SubgeometryOfProjectiveSpaceByFrame(pg,frame,GF(q));
    end );

# Basic methods for subgeometries.
#############################################################################

#############################################################################
#O  \=( <sub1>, <sub2> )
# test equality of subgeometries.
# To test equality of two subgeometries, we check whether
# (0) if there ambient space equals, if not, return false
# (1) if (0)=true, then, test equality of their subfields, if not, return false
# (2) if (1)=true, test if one of them is not canonical, if so, check whether they
#      are equal if p*q^1, p, s their respective projectivities,
#      is a collineation over the subfield,
#      note 12/9/16: checking whether all elements of mat belong to the subfield, is
#      too restrictive: we must now first divded mat by the first non-zero element!
# (3) if (2)=false, then both are canonical, and return true.
##
InstallMethod( \=,
    "for two subgeometries of a projective space",
	[ IsSubgeometryOfProjectiveSpace, IsSubgeometryOfProjectiveSpaceRep ],
    function(sub1,sub2)
    local proj1, proj2, res, mat, nz;
    if not AmbientSpace(sub1) = AmbientSpace(sub2) then
        return false;
    elif not sub1!.subfield = sub2!.subfield then
        return false;
    elif IsCanonicalSubgeometryOfProjectiveSpace(sub1) = false or IsCanonicalSubgeometryOfProjectiveSpace(sub2) = false then
        proj1 := sub1!.projectivity;
        proj2 := sub2!.projectivity;
        res := proj1 * proj2^(-1);
        mat := MatrixOfCollineation(res);
        #return ForAll(Flat(mat), x->x in sub1!.subfield); # too restrictive!
        nz := First(Flat(mat),x->x <> Zero(sub1!.basefield));
        return ForAll(Flat(mat), x->x/nz in sub1!.subfield);
    else
        return true;
    fi;
    end );

# The next two methods are necessary, since a subgeometry of a projective space
# is also a projective space. Not installing these methods, would invoke the
# method for \= for two projective spaces if one of the arguments is a subgeometry
# and the other one a projective space not a subgeometry, with an incorrect answer.
#############################################################################
#O  \=( <pg1>, <pg2> )
##
InstallMethod( \=,
	"for two projective spaces",
	[IsSubgeometryOfProjectiveSpace, IsProjectiveSpace],
	function(pg1,pg2);
		return false;
	end );

#############################################################################
#O  \=( <pg1>, <pg2> )
##
InstallMethod( \=,
	"for two projective spaces",
	[IsProjectiveSpace, IsSubgeometryOfProjectiveSpace],
	function(pg1,pg2);
		return false;
	end );

#############################################################################
#O  Rank( <ps> )
# Rank of a subgeometry
##
InstallMethod( Rank,
	"for a projective space",
	[ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
	ps -> ps!.dimension
	);

# Note that UnderlyingVectorSpace and BaseField are installed for projective
# spaces and hence, are applicable to subgeometries. See introductory comments
# at top of file, for the "definition" of underlying vectorspace and base field.
#############################################################################
#O  SubfieldOfSubgeometry( <sub> )
# Subfield of subgeometry.
##
InstallMethod( SubfieldOfSubgeometry,
	"for a subgeometry of a projective space",
	[ IsSubgeometryOfProjectiveSpace ],
	sub -> sub!.subfield );

# NEW 31/5/2020 jdb
#############################################################################
#A  StandardFrame( <sub> )
# if the dimension of subgeometry <sub> is n and <sub> is the canonical subgeometry,
# then StandardFrame returns a list of points of <sub> with coordinates
# (1,0,...0), (0,1,0,...,0), ..., (0,...,0,1) and (1,1,...,1). In case <sub> is not the
# canonical subgeometry, the image of these points under sub!.projectivity is returned.
# note that, due to the way we construct subgeometries of projective space,
# this is nothing else than the meet of the points of the defining frame with sub
##
InstallMethod( StandardFrame, 
	"for a subgeometry of a projective space",
	[ IsSubgeometryOfProjectiveSpace ],
	function( sub )
        return List(sub!.frame,x->Meet(sub,x));
	end );

#############################################################################
#A CollineationFixingSubgeometry
# For a subgeometry PG(n,q) in PG(n,q^2), we would call this the Baer involution.
# This is the collineation of PGammaL(n+1,q^t) fixing the subgeometry point wise.
# Its order is t.
##
InstallMethod( CollineationFixingSubgeometry,
	"for a subgeometry of a projective space",
	[ IsSubgeometryOfProjectiveSpace ],
    function(pg)
        return pg!.sigma;
    end );

# Constructor operations for elements and basic operations for elements.
#############################################################################

#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# An almost classical method for Wrap, with <geo> a subgeometry. Note that
# Wrap is not a user operation.
##
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

#############################################################################
#O  VectorSpaceToElementForSubgeometries( <sub>, <obj> )
# Note that a VectorSpaceToElement method must be created for every Lie geometry
# This is a helper operation to simplify the different VectorSpaceToElement
# methods for particular objects.
# It is checked whether the projectivity maps the element of the ambient projective
# space based on <obj> back to and element of the canonical subgeometry. If so,
# the element of the subgeometry based on <obj> is returned.
##
InstallMethod( VectorSpaceToElementForSubgeometries,
	"for a sub geometry of a projective space and an object",
	[ IsSubgeometryOfProjectiveSpace, IsObject],
	function( sub, obj )
        local ambient, element, newelement, proj, subfield, newobj, elements;
   		if IsZero(obj) then
        return EmptySubspace(sub);
      fi;
      ambient := sub!.ambientspace;
      element := VectorSpaceToElement(ambient,obj);
      if element = ambient then
          # newobj := Filtered(obj,x->not IsZero(x));
          # elements := List(newobj,x->VectorSpaceToElement(sub,x));
          return sub;
      # else
      #     elements := [element];
      fi;
      if not IsCanonicalSubgeometryOfProjectiveSpace(sub) then
          proj := sub!.projectivity;
          newelement := element^(proj^(-1));
      else
          newelement := element;
      fi;
      subfield := sub!.subfield;
      if not ForAll(Flat(newelement!.obj),a->a in subfield) then
      #if not ForAll( Flat( List(newelements,x->x!.obj ) ), i -> i in subfield) then
          Error( "<obj> does not determine an element in <sub>");
      # elif element = ambient then
      #     return sub;
      else
          return Wrap(sub,element!.type,UnderlyingObject(element));
      fi;
    end);

# Now follow the different methods for VectorSpaceToElement for objects
# of particular type.
#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a matrix as plist",
	[IsSubgeometryOfProjectiveSpace, IsPlistRep and IsMatrix],
	function( geom, v )
    return VectorSpaceToElementForSubgeometries(geom,v);
    end );

#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a CMatRep",
	[ IsSubgeometryOfProjectiveSpace, IsCMatRep],
	function( geom, v )
	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
	end );

#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a compressed GF(2)-matrix",
	[IsSubgeometryOfProjectiveSpace, IsGF2MatrixRep],
	function( geom, v )
	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
	end );

#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a compressed basis of a vector subspace",
	[IsSubgeometryOfProjectiveSpace, Is8BitMatrixRep],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
    end );

#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a row vector as cvec",
	[IsSubgeometryOfProjectiveSpace, IsCVecRep],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
    end );

#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and a row vector",
	[IsSubgeometryOfProjectiveSpace, IsRowVector],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, v);
    end );

#############################################################################
#O  VectorSpaceToElement( <sub>, <v> )
##
InstallMethod( VectorSpaceToElement,
	"for a subgeometry of a projective space and an 8-bit vector",
	[IsSubgeometryOfProjectiveSpace, Is8BitVectorRep],
	function( geom, v )
  	return VectorSpaceToElementForSubgeometries(geom, Unpack(v));
    end );

#############################################################################
#O \=
# Equality of two subspaces of a subgeometry
# Note that this is a test of mathematical equality: the ambient geometry
# must be equal (so not IsIdenticalObj), and
# the underlying object must be equal. Recall that due to norming the underlying
# object, this is garantueed for two equal subspaces.
##
InstallMethod( \=,
	"for two subspace of subgeometry a projective space",
	[IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
	function(p1,p2);
		return (p1!.obj = p2!.obj) and (p1!.geo = p2!.geo);
	end );

#############################################################################
#O \=
# Equality of a subspace of a subgeometry and a subspace of a projective space
# returns always false.
##
InstallMethod( \=,
	"for a subspace of subgeometry a projective space and a subspace of a projective space",
	[IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	function(p1,p2);
		return false;
	end );

#############################################################################
#O \=
# Equality of a subspace of a subgeometry and a subspace of a projective space
# returns always false.
##
InstallMethod( \=,
	"for a subspace of subgeometry a projective space and a subspace of a projective space",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
	function(p1,p2);
		return false;
	end );

#############################################################################
#O UnderlyingVectorSpace
# This operation is defined for subspaces of a projective space. However,
# for subspaces of a subgeometry, which are also subspaces of projective
# spaces, this is mathematically senseless. We have to think about this,
# but currently, we return an error.
##
InstallMethod( UnderlyingVectorSpace,
	"for a subspace of subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace ],
    function(el)
        Error(" <el> is a subspace of a subgeometry of a projective space");
    end );

#InstallMethod( BaseField,
#	"for an element of a projective space",
#	[IsSubspaceOfProjectiveSpace],
#	sub -> sub!.basefield );

#############################################################################
#O ElementsOfIncidenceStructure
# Classical operation for Lie geometries, here for subgeometries of projective
# spaces. Note that for this method, we rely on the isomorphic subgeometry.
##
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

#############################################################################
#O ExtendElementOfSubgeometry
# Particular method for subspaces of subgeometries: extend a subspace of a
# subgeometry to a subspace of the ambient projective space.
##
InstallMethod( ExtendElementOfSubgeometry,
	"for an element of a subgeometry of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace ],
	element -> VectorSpaceToElement(AmbientSpace(element),Unpack(UnderlyingObject(element))));


# For elements of Lie geometries, incidence is based on set theoretic containment.
# therefore, a method of \in must be present. Note that we define several
# methods that return Errors if the (sub)geometries are different.
# For Lie geometries, \in is also applicable on subspaces of subgeometries
# and projective spaces.
#############################################################################
#O \in We use ExtendElementOfSubgeometry. Clearly, if the extension of an element
# is contained in the extension of another element, the first element is contained
# in the second in the subgeometry.
##
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

#############################################################################
#O \in for a subspace of a subgeometry and a subspace of a projective space.
##
InstallMethod( \in,
	"for a subspace of a subgeometry and a subspace of a projective space",
	[IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

#############################################################################
#O \in for a subspace of a projective space and a subspace of a subgeometry.
##
InstallMethod( \in,
	"for a subspace of a projective space and a subspace of a subgeometry",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

#############################################################################
#O \in for a subspace of a subgeometry of a projective space and a projective
# space. This is somehow tricky: subgeometries also belong to IsProjectiveSpace
# so the element!.geo = can really occur. The consequence is also that
# an element of a subgeometry is not in the ambient projective space. This
# complies with the basic philosophy of subgeometries.
##
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

#############################################################################
#O \in( <element>, <ps> )
# for a subspace of a projective space and a subgeometry.
# This is tricky as well: subspaces of subgeometries also belong to
# IsSubspaceOfProjectiveSpace so the element!.geo = ps can occur if the
# element is really a subspace of a subgeometry.
##
InstallMethod( \in,
	"for an element of a subgeometry of a projective space and a projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubgeometryOfProjectiveSpace],
	function( element, ps )
       return element!.geo = ps;
    end );

#############################################################################
#O  Random( <subs> )
# returns a random subspace out of the collection of subspaces of given dimension
# of a subgeometry of a projective space. Note that Random has its own method
# for a collection of subspaces of a given dimension of a projective space.
# We use the isomorphic subgeometry for the method here.
##
InstallMethod( Random,
	"for a collection of subspaces of a subgeometry of a projective space",
	[ IsSubspacesOfSubgeometryOfProjectiveSpace ],
    # chooses a random element out of the collection of subspaces of given
    # dimension of a subgeometry of a projective space
	function( subs )
		local d, pg, w, isom, em, el;
		## the underlying projective space
		pg := subs!.geometry;
        isom := pg!.isomorphicsubgeometry;
        em := pg!.embedding;
		if not IsInt(subs!.type) then
			Error("The subspaces of the collection need to have the same dimension");
        fi;
		## the common type of elements of subs
		d := subs!.type;
        el := Random(ElementsOfIncidenceStructure(isom,d))^em;
        if not IsCanonicalSubgeometryOfProjectiveSpace(pg) then
            el := el^pg!.projectivity;
        fi;
        return Wrap(pg,d,UnderlyingObject(el));
        end );

# Span/Meet operations. Note the basic philosophy. We allow a span of two
# subspaces of a subgeometry only if their ambient geometry is the same. So
# having the same ambient space is not sufficient. This is slightly different
# than for e.g. polar spaces.

#############################################################################
#O Span( <x>, <y> )
# for two elements of a subgeometry. We use Embed, then compute the
# span in the ambient space, and then rewrap the result as a subspace of
# the subgeometry. We are sure that if the ambient geometries of
# both subspaces are the same, the result is correct. It is checked whether
# this is true for the input.
##
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

#############################################################################
#O Span( <l> )
# for a homogeneous lis of subspaces of a subgeometry. Note that the necessary
# checks are built in. We rely again on the Span in the ambient space.
# Compare this method with the method for Span for a homogeneous list of
# subspaces of a projective space.
##
InstallMethod( Span,
    "for a homogeneous list of subspaces of a subgeometry of a projective space",
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

#############################################################################
#O Span( <x>, <y> )
# for a subspace of subgeometry and a subspace of a projective space. Simply
# produces an error. But we need to install this method, if not, the method
# for Span for two subspaces of a projective space would return a result which
# makes, in our philosophy of subgeometries, no sense.
##
InstallMethod( Span,
	"for a subspace of a projective space and a subspace of a subgeometry",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

#############################################################################
#O Span( <x>, <y> )
# see comment on previous Span method
##
InstallMethod( Span,
	"for a subspace of a subgeometry and a subspace of a projective space",
	[ IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
        Error("<x> and <y> do not belong to the same geometry");
    end );

#############################################################################
#O Span( <element>, <ps> )
# returns <ps> if it is the ambient geometry of element.
##
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

#############################################################################
#O Span( <ps>, <element> )
# returns <ps> if it is the ambient geometry of element.
##
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

#############################################################################
#O Meet( <x>, <y> )
# for two elements of a subgeometry. We use Embed, then compute the
# meet in the ambient space, and then rewrap the result as a subspace of
# the subgeometry. We are sure that if the ambient geometries of
# both subspaces are the same, the result is correct. It is checked whether
# this is true for the input.
##
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

#############################################################################
#O Meet( <l> )
# for a homogeneous lis of subspaces of a subgeometry. Note that the necessary
# checks are built in. We rely again on the Meet in the ambient space.
# Compare this method with the method for Meet for a homogeneous list of
# subspaces of a projective space.
##
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
            # same := Size(AsDuplicateFreeList(List(l,x->AmbientGeometry(x))))=1;
            if IsEmptySubspace(meet) then
                return EmptySubspace(l[1]!.geo);
            else
                return Wrap(l[1]!.geo,meet!.type,UnderlyingObject(meet));
            fi;
        fi;
    end );

# we might consider to allow slightly more here: e.g. meet of a subspace of
# of subgeometry with a subspace of the ambient space.
#############################################################################
#O Meet( <x>, <y> )
# for a subspace of subgeometry and a subspace of a projective space. Simply
# produces an error. But we need to install this method, if not, the method
# for Meet for two subspaces of a projective space would return a result which
# makes, in our philosophy of subgeometries, no sense.
##
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

#############################################################################
#O Meet( <element>, <ps> )
# returns <element> if <ps> is the ambient geometry of element.
##
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

#############################################################################
#O Meet( <ps>, <element> )
# returns <element> if <ps> is the ambient geometry of element.
##
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

#############################################################################
#O Meet( <sub>, <el> )
# for a subgeometry and a subspace of a projective space.
##
#use sigma to compute the intersection of an element of the ambient space
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

#############################################################################
#O Meet( <sub>, <el> )
# for a subspace of a projective space and a subgeometry
##
InstallMethod( Meet,
	"for a projective space and an element of a subgeometry of a projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubgeometryOfProjectiveSpace],
	function( el, sub )
        return Meet(sub,el);
    end );

#  flags and shadows

#############################################################################
#O FlagOfIncidenceStructure( <ps>, <els> )
# returns the flag of the subgeometry <ps> with elements in <els>.
# It is checked first whether the elements in <els> belong to the
# same ambient geometry. The method checks whether the input really determines a flag.
# Note that the filter for the first argument is IsProjectiveSpace. So this method
# is applicable for a projective space that is not a subgeometry and a list
# of elements of a subgeometry, but will give an appropriate error.
# Using IsSubgeometryOfProjectiveSpace would force us to install another method
# just producing an error saying that the elements do not belong to <ps>
# if called with the first argument a projective space that is not a subgeometry.
# Installing no such method in that case would result in a no method found error.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a projective space and list of subspaces of the projective space",
	[ IsProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpaceCollection ],
	function(ps,els)
		local list,i,test,type,flag;
		list := Set(ShallowCopy(els));
		if Length(list) > Rank(ps) then
		  Error("A flag can contain at most Rank(<ps>) elements");
		fi;
        test := List(list,x->AmbientGeometry(x));
        if not ForAll(test,x->x=ps) then
            Error("not all elements have <ps> as ambient geometry");
        fi;
        test := Set(List([1..Length(list)-1],i -> IsIncident(list[i],list[i+1])));
		if (test <> [ true ] and test <> []) then
		  Error("<els> do not determine a flag");
		fi;
		flag := rec(geo := ps, types := List(list,x->x!.type), els := list, vectorspace := ps!.vectorspace );
		ObjectifyWithAttributes(flag, IsFlagsOfSgOPSType, IsEmptyFlag, false, RankAttr, Size(list) );
		return flag;
	end);

#############################################################################
#O ShadowOfElement( <ps>, <v>, <j> )
# returns the shadow of an element v as a record containing the subgeometry <ps>,
# the type j of the elements (type), the element v (parentflag), and
# some extra information useful to compute with the shadows, e.g. iterator
# This method relies on the isomorphic subgeometry of <ps>
# Note: we use IsProjectiveSpace, see also note at FlagOfIncidenceStructure
##
InstallMethod( ShadowOfElement,
	"for a projective space, an element of a subgeometry, and an integer",
	[IsProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace, IsPosInt],
	# returns the shadow of an element v as a record containing the projective space (geometry),
	# the type j of the elements (type), the element v (parentflag), and some extra information
	# useful to compute with the shadows, e.g. iterator
	function( ps, v, j )
		local localinner, localouter, localfactorspace, tocanonical, vs, vcanonical;
        if not AmbientGeometry(v) = ps then
            Error("<ps> is not the ambient geometry of <v>");
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

#############################################################################
#O  ShadowOfFlag( <ps>, <flag>, <j> )
# returns the shadow elements of <flag>, i.e. the elements of <ps> of type <j>
# incident with all elements of <flag>.
# returns the shadow of a flag as a record containing the geometry,
# the type j of the elements (type), the flag (parentflag), and some extra information
# useful to compute with the shadows, e.g. iterator
# This method relies on the isomorphic subgeometry of <ps>
# Note: we use IsProjectiveSpace, see also note at FlagOfIncidenceStructure
##
InstallMethod( ShadowOfFlag,
	"for a a projective space, a flag of a subgeometry of a projective space, and an integer",
	[IsProjectiveSpace, IsFlagOfSubgeometryOfProjectiveSpace, IsPosInt],
	function( ps, flag, j )
    local localinner, localouter, localfactorspace, v, smallertypes, biggertypes, ceiling, floor, canels, vs;
    if not flag!.geo = ps then
        Error("<flag> is not a flag of <ps>");
    fi;
    if j > ps!.dimension then
        Error("<ps> has no elements of type <j>");
    fi;
    #empty flag - return all subspaces of the right type
    if IsEmptyFlag(flag) then
      return ElementsOfIncidenceStructure(ps, j);
    fi;

    # find the element in the flag of highest type less than j, and the subspace
    # in the flag of lowest type more than j.

	#listoftypes:=List(flag,x->x!.type);
	smallertypes:=Filtered(flag!.types,t->t <= j);
	biggertypes:=Filtered(flag!.types,t->t >= j);
    vs := ps!.isomorphicsubgeometry!.vectorspace;
    if not IsCanonicalSubgeometryOfProjectiveSpace(ps) then
           canels := List(flag!.els,x->x^(ps!.projectivity^(-1)));
        else
           canels := List(flag!.els);
    fi;

	if smallertypes=[] then
		localinner := [];
		ceiling:=Minimum(biggertypes);
		localouter:= canels[Position(flag!.types,ceiling)];
	elif biggertypes=[] then
		localouter:=BasisVectors(Basis(vs));
		floor:=Maximum(smallertypes);
		localinner:= canels[Position(flag!.types,floor)];
	else
		floor:=Maximum(smallertypes);
		ceiling:=Minimum(biggertypes);
		localinner:= canels[Position(flag!.types,floor)];
		localouter:= canels[Position(flag!.types,ceiling)];
	fi;
	if not smallertypes = [] then
		if localinner!.type = 1 then
			localinner:=[Unpack(localinner!.obj)]; #here is the cmat change
		else
			localinner:=Unpack(localinner!.obj);
		fi;
	fi;
    if not biggertypes = [] then
		if localouter!.type = 1 then
			localouter := [Unpack(localouter!.obj)];
        else
			localouter := Unpack(localouter!.obj);
        fi;
	fi;
    localfactorspace := Subspace(vs,
		BaseSteinitzVectors(localouter, localinner).factorspace);
    return Objectify(
		NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
							IsShadowSubspacesOfSubgeometryOfProjectiveSpace and
							IsShadowSubspacesOfSubgeometryOfProjectiveSpaceRep),
        rec(
          geometry := ps,
          type := j,
          inner := localinner,
          outer := localouter,
          factorspace := localfactorspace,
		  parentflag := flag,
          size := Size(Subspaces(localfactorspace)) #this causes a problem when localfactorspace consists of cvec/cmat.
        )
      );
	end);

#############################################################################
#O Iterator: the classical iterator, relying completely on the isomorphic
# subgeometry and using the projectivity.
##
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


#############################################################################
#O Iterator: the classical iterator for shadows, relying completely on
# the isomorphic subgeometry and using the projectivity.
##
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

## groups and actions.

InstallMethod( CollineationGroup,
	"for a subgeometry of a projective space",
	[ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
	function( sub )
		local coll,d,f,frob,g,newgens,q,s,pow,h,baer;
		f := sub!.subfield;
		q := Size(f);
		d := ProjectiveDimension(sub);
		if d <= -1 then
			Error("The dimension of the projective spaces needs to be at least 0");
		fi;
		g := GL(d+1,f);
		frob := FrobeniusAutomorphism(sub!.basefield); #frobenius automorphism of big field
        h := DegreeOverPrimeField(BaseField(sub));
        newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]);
        #baer := frob^Length(FactorsInt(q)); #this is precisely the Baer collineation for the canonical subgeometry.
        #Add(newgens,[One(g),baer]);
        # new paradigm: CollineationGroup(sub) will not consider the embedding,
        # if q is not prime, the frobenius automorphism of GF(q) is also a collineation.
        # note that we add the frobenius automorphism of the basefield (not the subfield).
		#if not IsPrime(q) then #if q is not prime, there is a frobenius automorphism.
		#	Add(newgens,[One(g),frob]);
        #    s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)) * Order(frob);
		#else
        #    Add(newgens,[One(g),baer]);
        #    s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)) * Order(baer);
        #fi;
        Add(newgens,[One(g),frob]);
		s := Size(CollineationGroup(PG(d,q)))*h;
        newgens := ProjElsWithFrob(newgens,sub!.basefield); #using sub!.basefield as second argument makes sure that
        # ProjElsWithFrob returns elements in the collineation group of the ambient projective space.
        if not IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            newgens := List(newgens,x->sub!.projectivity^(-1)*x*sub!.projectivity);
        fi;
		coll := GroupWithGenerators(newgens);
		#pow := LogInt(q, Characteristic(f)); #order of frobenius of subfield!
		#s := pow * q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1))*Order(baer); #hard coded order!
		if not IsPrime(q) then
			SetName( coll, Concatenation("The FinInG collineation group PGammaL(",String(d+1),",",String(q),") of ",ViewString(sub)) );
		else
			SetName( coll, Concatenation("The FinInG collineation group PGL(",String(d+1),",",String(q),") of ",ViewString(sub)) );
			# Remark that in the prime case, PGL is returned as a FinInG collineation group with associated automorphism F^0.
		fi;
		SetSize( coll, s );
        # only for making generalised polygons section more generic:
        if d = 2 then
            SetCollineationAction(coll,OnProjSubspacesOfSubgeometryNC);
        fi;
        SetDefaultGeometry(coll,sub);
        SetParent(coll,CollineationGroup(AmbientSpace(sub)));
		return coll;
	end );


##
InstallMethod( ProjectivityGroup,
	"for a subgeometry of a projective space",
	[ IsSubgeometryOfProjectiveSpace and IsSubgeometryOfProjectiveSpaceRep ],
	function( sub )
		local d,f,frob,g,newgens,q,s,baer,coll;
		f := sub!.subfield;
		q := Size(f);
		d := ProjectiveDimension(sub);
		if d <= -1 then
			Error("The dimension of the projective spaces needs to be at least 0");
		fi;
		g := GL(d+1,f);
		frob := FrobeniusAutomorphism(sub!.basefield); #frobenius automorphism of big field
		newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]);
        #baer := frob^Length(FactorsInt(q)); #this is precisely the Baer collineation for the canonical subgeometry.
        #Add(newgens,[One(g),baer]);
		newgens := ProjElsWithFrob(newgens,sub!.basefield); #using sub!.basefield as second argument makes sure that
        # ProjElsWithFrob returns elements in the collineation group of the ambient projective space.
        if not IsCanonicalSubgeometryOfProjectiveSpace(sub) then
            newgens := List(newgens,x->sub!.projectivity^(-1)*x*sub!.projectivity);
        fi;
		coll := GroupWithGenerators(newgens);
        SetName( coll, Concatenation("The FinInG projectivity group PGL(",String(d+1),",",String(q),") of ",ViewString(sub)) );
        #s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)) * Order(baer);
		s := Size(ProjectivityGroup(PG(d,f)));
        SetSize( coll, s );
        # only for making generalised polygons section more generic:
        if d = 2 then
            SetCollineationAction(coll,OnProjSubspacesOfSubgeometryNC);
        fi;
        SetDefaultGeometry(coll,sub);
		return coll;
	end );


InstallMethod( NiceMonomorphism,
	"for a projective group of a subgeometry",
	[IsProjectiveGroupWithFrob and HasDefaultGeometry],
	50,
	function( pg )
	local hom, dom,bf, geom;
	Info(InfoFinInG,4,"Using NiceMonomorphism for proj. group (feasible)");
    geom := DefaultGeometry(pg);
	bf := SubfieldOfSubgeometry(geom);
    #dom := List(MakeAllProjectivePoints( bf, Dimension(pg) - 1),x->OnProjPointsWithFrob(x,geom!.projectivity));
    dom := AsList(Points(geom));
    #The use of FINING.Fast seems deprecated 25/5/2020.
    #if FINING.Fast then
    hom := NiceMonomorphismByDomain( pg, dom, OnProjSubspacesOfSubgeometryNC );
    #else
    #   hom := ActionHomomorphism(pg, dom, OnProjSubspacesOfSubgeometriesNC, "surjective");
    #   SetIsBijective(hom, true);
    #fi;
    return hom;
	end );


InstallGlobalFunction( OnProjSubspacesOfSubgeometryNC,
  function( var, el )
    local amb,geo,newvar;
    geo := var!.geo;
    if var!.type = 1 then
        newvar := OnProjPointsWithFrob(var!.obj,el);
    else
        newvar := OnProjSubspacesWithFrob(var!.obj,el);
    fi;
    return Wrap(geo,var!.type,newvar);
  end );

InstallGlobalFunction( OnProjSubspacesOfSubgeometry,
  function( var, el )
    local amb,geo,newvar,newel,baer;
    geo := var!.geo;
    if var!.type = 1 then
        newvar := OnProjPointsWithFrob(var!.obj,el);
    else
        newvar := OnProjSubspacesWithFrob(var!.obj,el);
    fi;
    newel := Wrap(AmbientSpace(geo),var!.type,newvar);
    baer := geo!.sigma;
    if newel^baer = newel then
        return Wrap(geo,var!.type,newvar);
    else
        return newel;
    fi;
  end );

InstallOtherMethod( \^,
	"for an element of an incidence structure and a projective semilinear element",
	[IsSubspaceOfSubgeometryOfProjectiveSpace, IsProjGrpElWithFrob],
	function(x, em)
		return OnProjSubspacesOfSubgeometry(x,em);
	end );

InstallGlobalFunction( OnSubgeometryOfProjectiveSpace,
  function( sub, el )
    local frame;
    frame := List(sub!.frame,x->x^el);
    return SubgeometryOfProjectiveSpaceByFrame(AmbientSpace(sub),frame,SubfieldOfSubgeometry(sub));
end );
