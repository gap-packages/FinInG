TrialitySplitCayleyPoint := function(el)
# el is a point of H(q)
    local z, hyps, q, y, pg;
    q := Size(BaseField(el));
    y := Unpack(UnderlyingObject(el));
    y[8] := -y[4]; 
    pg := AmbientSpace(el);
    z := [];
    z[1] := [0,y[3],-y[2],y[5],y[8],0,0,0]*Z(q)^0;
    z[2] := [-y[3],0,y[1],y[6],0,y[8],0,0]*Z(q)^0;
    z[3] := [y[2],-y[1],0,y[7],0,0,y[8],0]*Z(q)^0;
    z[4] := [0,0,0,-y[4],y[1],y[2],y[3],0]*Z(q)^0;
    z[5] := [y[4],0,0,0,0,y[7],-y[6],y[1]]*Z(q)^0;
    z[6] := [0,y[4],0,0,-y[7],0,y[5],y[2]]*Z(q)^0;
    z[7] := [0,0,y[4],0,y[6],-y[5],0,y[3]]*Z(q)^0;
    z[8] := [y[5],y[6],y[7],0,0,0,0,-y[8]]*Z(q)^0;
    z := Filtered(z,x->not IsZero(x));
    hyps := List(z,x->HyperplaneByDualCoordinates(pg,x));
    return Meet(hyps);
end;

ZeroPointToOnePointsSpaceByTriality := function(el)
# el is a point of T(q,q^3)
    local z, hyps, y, pg, spacevec, f, frob, n;
    f := BaseField(el);
    frob := FrobeniusAutomorphism(f);
    if not Order(frob)=3 then
        Error("No triality possible");
    fi;
    n := Zero(f);
    y := Unpack(UnderlyingObject(el))^frob;
    #y[8] := -y[4];
    pg := AmbientSpace(el);
    z := [];
    z[1] := [n,y[3],-y[2],y[5],y[8],n,n,n];
    z[2] := [-y[3],n,y[1],y[6],n,y[8],n,n];
    z[3] := [y[2],-y[1],n,y[7],n,n,y[8],n];
    z[4] := [n,n,n,-y[4],y[1],y[2],y[3],n];
    z[5] := [y[4],n,n,n,n,y[7],-y[6],y[1]];
    z[6] := [n,y[4],n,n,-y[7],n,y[5],y[2]];
    z[7] := [n,n,y[4],n,y[6],-y[5],n,y[3]];
    z[8] := [y[5],y[6],y[7],n,n,n,n,-y[8]];
    z := Filtered(z,x->not IsZero(x));
	spacevec := NullspaceMat(TransposedMat(z));
    return VectorSpaceToElement(pg,spacevec);
end;


SplitCayleyPointToPlane := function(el)
    local z, hyps, q, y, pg, planevec, basishyp, hyp, bs, invbasis, vec;
    q := Size(BaseField(el));
    y := Unpack(UnderlyingObject(el));
    y[8] := -y[4];
    pg := AmbientSpace(el);
    z := [];
    z[1] := [0,y[3],-y[2],y[5],y[8],0,0,0]*Z(q)^0;
    z[2] := [-y[3],0,y[1],y[6],0,y[8],0,0]*Z(q)^0;
    z[3] := [y[2],-y[1],0,y[7],0,0,y[8],0]*Z(q)^0;
    z[4] := [0,0,0,-y[4],y[1],y[2],y[3],0]*Z(q)^0;
    z[5] := [y[4],0,0,0,0,y[7],-y[6],y[1]]*Z(q)^0;
    z[6] := [0,y[4],0,0,-y[7],0,y[5],y[2]]*Z(q)^0;
    z[7] := [0,0,y[4],0,y[6],-y[5],0,y[3]]*Z(q)^0;
    z[8] := [y[5],y[6],y[7],0,0,0,0,-y[8]]*Z(q)^0;
    z := Filtered(z,x->not IsZero(x));
    hyp := [0,0,0,1,0,0,0,1]*Z(q)^0;
	Add(z,[0,0,0,1,0,0,0,1]*Z(q)^0);
	planevec := NullspaceMat(TransposedMat(z));
	basishyp := NullspaceMat(TransposedMat([hyp]));
	bs := BaseSteinitzVectors(Basis(GF(q)^8), basishyp);
	invbasis := Inverse(Concatenation(bs!.subspace, bs!.factorspace));
	vec := planevec*invbasis;
	return VectorSpaceToElement(pg,vec{[1..3]}{[1..7]}); #vec will be a basis of a plane -> 3 rows.
end;

onespaces1 := List(pts,x->OnePointToSpace(x));
onespaces2 := List(pts,x->OnePointToSpaceVec(x));

q := 5;
hq := SplitCayleyHexagon(q);
ps := AmbientPolarSpace(hq);
pts6 := AsList(Points(ps));
#planes6 := List(pts6,x->OnePointToPlane(x));
planes6 := List(pts6,x->SplitCayleyPointToPlane(x));


flags := [];
for i in [1..Length(pts6)] do
flags[i] := FlagOfIncidenceStructure(PG(6,q),[pts6[i],planes6[i]]);
od;
shads6 := List(flags,x->ShadowOfFlag(PG(6,q),x,2));
lines6 := List(shads6,x->List(x));
lines6 := Union(lines6);

group := FiningSetwiseStabiliser(CollineationGroup(ps),lines6);

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group,Union(pts6,lines6),OnProjSubspaces,rel);;
Diameter(graph);
Girth(graph);

#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the element in <geom> determined
# by the rowvector <v>. <geom> is a generalised hexagon, so an ambient polar space
# ps is available. A point of geom is necessary a point of ps, but for T(q^3,q) we need
# to check whether the point of Q+(7,q) is absolute.
##
InstallMethod( VectorSpaceToElement,
    "for a generalised hexagon and a row vector",
    [ IsLieGeometry and IsGeneralisedHexagon, IsRowVector ],
    function(geom,vec)
    local x,y, ps, el;
    ps := AmbientPolarSpace(geom);
    #first check wheter vec makes a point of ps
    el := VectorSpaceToElement(ps,vec);
    if IsEmptySubspace(el) then
        return el;
    fi;
    if IsParabolicQuadric(ps) then
        return Wrap(geom, 1, UnderlyingObject(el));
    elif IsHyperbolicQuadric(ps) then
        if el in ZeroPointToOnePointsSpaceByTriality(el) then
            return Wrap(geom, 1, UnderlyingObject(el));
        else
            Error(" <vec> represents a point of the ambient polar space not absolute with relation to its triality");
        fi;
    elif IsSymplecticSpace(ps) then
        return Wrap(geom, 1, UnderlyingObject(el));
    fi;
end );

    
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. <geom> is a generalised hexagong, so an ambient polar space
# ps is available. A point of geom is necessary a point of ps, but for T(q^3,q) we need
# to check whether the point of Q+(7,q) is absolute.
##
InstallMethod( VectorSpaceToElement,
    "for a generalised hexagon and a row vector",
    [ IsLieGeometry and IsGeneralisedHexagon, IsMatrix ],
    function(geom,vec)
    local x,y, ps, el, p1, p2, pg, mat;
    ps := AmbientPolarSpace(geom);
    #first check wheter vec makes a point of ps
    el := VectorSpaceToElement(ps,vec);
    if ProjectiveDimension(el) = 0 then
        return VectorSpaceToElement(geom,UnderlyingObject(el));
    elif ProjectiveDimension(el) <> 1 then
        Error(" <mat> does not represent a point or line of <geom>");
    fi;
    if not ProjectiveDimension(el) = 1 then
        Error("<vec> does not determine a line of <geom>");
    fi;
    if IsEmptySubspace(el) then
        return el;
    fi;
    mat := UnderlyingObject(el);
    pg := AmbientSpace(el);
    p1 := VectorSpaceToElement(pg,mat[1]);
    p2 := VectorSpaceToElement(pg,mat[2]);

    if IsParabolicQuadric(ps) then
        if p1 in SplitCayleyPointToPlane(p2) then
            return Wrap(geom, 2, UnderlyingObject(el));
        else
            Error("<vec> does not represent a line of <geom>");
        fi;
    elif IsHyperbolicQuadric(ps) then
        if p1 in ZeroPointToOnePointsSpaceByTriality(p1) and 
                p2 in ZeroPointToOnePointsSpaceByTriality(p2) and
                p1 in ZeroPointToOnePointsSpaceByTriality(p2)
        then
            return Wrap(geom, 2, UnderlyingObject(el));
        else
            Error("<mat> does not represent a line of <geom>");
        fi;
    elif IsSymplecticSpace(ps) then
        Print("I beg you for some patience");
    fi;
end );





