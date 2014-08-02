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

OnePointToTwoPointsSpaceByTriality := function(el)
# el is a point of T(q,q^3)
    local y, hyps, n, z, pg, f, frob;
    f := BaseField(el);
    frob := FrobeniusAutomorphism(f);
    if not Order(frob)=3 then
        Error("No triality possible");
    fi;
    n := Zero(f);
    z := Unpack(UnderlyingObject(el))^frob;
    #z[8] := -z[4];
    pg := AmbientSpace(el);
    y := [];
    y[1] := [n,-z[3],z[2],n,z[4],n,n,z[5]];
    y[2] := [z[3],n,-z[1],n,n,z[4],n,z[6]];
    y[3] := [-z[2],z[1],n,n,n,n,z[4],z[7]];
    y[4] := [z[5],z[6],z[7],-z[4],n,n,n,n];
    y[5] := [z[8],n,n,z[1],n,-z[7],z[6],n];
    y[6] := [n,z[8],n,z[2],z[7],n,-z[5],n];
    y[7] := [n,n,z[8],z[3],-z[6],z[5],n,n];
    y[8] := [n,n,n,n,z[1],z[2],z[3],-z[8]];
    y := Filtered(y,x->not IsZero(x));
    hyps := List(y,x->HyperplaneByDualCoordinates(pg,x));
    return Meet(hyps);
end;

OnePointToTwoPointsSpaceByTriality := function(el)
# el is a point of T(q,q^3)
    local y, spacevec, n, z, pg, f, frob;
    f := BaseField(el);
    frob := FrobeniusAutomorphism(f);
    if not Order(frob)=3 then
        Error("No triality possible");
    fi;
    n := Zero(f);
    z := Unpack(UnderlyingObject(el))^frob;
    pg := AmbientSpace(el);
    y := [];
    y[1] := [n,-z[3],z[2],n,z[4],n,n,z[5]];
    y[2] := [z[3],n,-z[1],n,n,z[4],n,z[6]];
    y[3] := [-z[2],z[1],n,n,n,n,z[4],z[7]];
    y[4] := [z[5],z[6],z[7],-z[4],n,n,n,n];
    y[5] := [z[8],n,n,z[1],n,-z[7],z[6],n];
    y[6] := [n,z[8],n,z[2],z[7],n,-z[5],n];
    y[7] := [n,n,z[8],z[3],-z[6],z[5],n,n];
    y[8] := [n,n,n,n,z[1],z[2],z[3],-z[8]];
    y := Filtered(y,x->not IsZero(x));
	spacevec := NullspaceMat(TransposedMat(y));
    return VectorSpaceToElement(pg,spacevec);
end;



g := CollineationOfProjectiveSpace(IdentityMat(8,GF(q^3)),frob,GF(q^3));

p1 := pts[1];
planes := List(pts,x->Meet(ZeroPointToOnePointsSpaceByTriality(x),OnePointToTwoPointsSpaceByTriality(x^g)));;

List([1..Length(pts)],i->List(lines_pts[i],y->y in planes[i]));

# now the function that gives the plane of Q+(7,q) containing the q+1 lines through a point.

TwistedTrialityHexagonPointToPlaneByTwoTimesTriality := function(elvec,frob,f)
# elvec represents a point of T(q,q^3)
    local z, hyps, y, pg, spacevec1, spacevec2, n, vec;
	n := Zero(f);
    #y := Unpack(UnderlyingObject(el))^frob;
    #y[8] := -y[4];
    y := elvec^frob;
	pg := PG(7,f);
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
	spacevec1 := NullspaceMat(TransposedMat(z));
    z := y^frob;
    y := [];
    y[1] := [n,-z[3],z[2],n,z[4],n,n,z[5]];
    y[2] := [z[3],n,-z[1],n,n,z[4],n,z[6]];
    y[3] := [-z[2],z[1],n,n,n,n,z[4],z[7]];
    y[4] := [z[5],z[6],z[7],-z[4],n,n,n,n];
    y[5] := [z[8],n,n,z[1],n,-z[7],z[6],n];
    y[6] := [n,z[8],n,z[2],z[7],n,-z[5],n];
    y[7] := [n,n,z[8],z[3],-z[6],z[5],n,n];
    y[8] := [n,n,n,n,z[1],z[2],z[3],-z[8]];
    y := Filtered(y,x->not IsZero(x));
	spacevec2 := NullspaceMat(TransposedMat(y));
	vec :=  SumIntersectionMat(spacevec1, spacevec2)[2];
	return VectorSpaceToElement(pg,vec);
end;

f := GF(8);
frob := FrobeniusAutomorphism(f);
planes := List(pts,x->TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(Unpack(x!.obj),frob,f));;

planes := List(pts,x->VectorSpaceToElement(PG(7,8),TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(x!.obj,frob,f)));;

List([1..Length(pts)],i->List(lines_pts[i],y->y in planes[i]));


planespts := List(planes,x->List(Points(x)));;
numbers := List(planespts,x->Length(Intersection(x,q6qpts)));;  
numberssub := List(planespts,x->Length(Intersection(x,q6qsubpts)));;  

ZeroPointToOnePointsSpaceByTriality := function(elvec,frob,f)
# elvec represents a point of T(q,q^3)
    local z, hyps, y, pg, spacevec, n;
    n := Zero(f);
    y := Unpack(UnderlyingObject(el))^frob;
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
    return spacevec
end;


SplitCayleyPointToPlane5 := function(el)
    local z, hyps, q, y, pg, planevec, basishyp, hyp, bs, invbasis, vec, w;
    q := Size(BaseField(el));
    w := Unpack(UnderlyingObject(el));
    y := w{[1..3]};
    y{[5..7]} := w{[4..6]};
    y[4] := (y[1]*y[5]+y[2]*y[6]+y[3]*y[7])^(q/2);
    y[8] := -y[4];
    pg := PG(5,q);
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
	return VectorSpaceToElement(pg,vec{[1..3]}{[1,2,3,5,6,7]}); #vec will be a basis of a plane -> 3 rows.
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
planes6 := List(pts6,x->SplitCayleyPointToPlane(x));;
planes6 := List(pts6,x->SplitCayleyPointToPlane5(x));;

pg := PG(6,q);
pg := PG(5,q);

flags := [];
for i in [1..Length(pts6)] do
flags[i] := FlagOfIncidenceStructure(pg,[pts6[i],planes6[i]]);
od;
shads6 := List(flags,x->ShadowOfFlag(pg,x,2));;
lines6 := List(shads6,x->List(x));;
lines6 := Union(lines6);;

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

### new tests
### 1.
q := 2;

hq := SplitCayleyHexagon(q);
pts := List(Points(hq));;
ps := AmbientPolarSpace(hq);
Collected(List(pts,x->x in ps));
lines := Set(Lines(hq));;
Collected(List(lines,x->x in ps));
lines2 := Union(List(pts,x->List(Lines(x))));;
lines = lines2;

group1 := CollineationGroup(hq);
group2 := FiningSetwiseStabiliser(CollineationGroup(ps),lines2);

group1 = group2;

ptsu := List(pts,x->ElementToElement(ps,x));;
linesu := List(lines,x->ElementToElement(ps,x));;

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group2,Union(ptsu,linesu),OnProjSubspaces,rel);;
Diameter(graph);
Girth(graph);


#### 2
q := 3;

ps := ParabolicQuadric(6,q);
hq := SplitCayleyHexagon(ps);
pts := List(Points(hq));;
Collected(List(pts,x->x in ps));
lines := Set(Lines(hq));;
Collected(List(lines,x->x in ps));
lines2 := Union(List(pts,x->List(Lines(x))));;
lines = lines2;

group1 := CollineationGroup(hq);
group2 := FiningSetwiseStabiliser(CollineationGroup(ps),lines2);

group1 = group2;

ptsu := List(pts,x->ElementToElement(ps,x));;
linesu := List(lines,x->ElementToElement(ps,x));;

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group1,Union(ptsu,linesu),OnProjSubspaces,rel);;
Diameter(graph);
Girth(graph);



### 2bis
mat := IdentityMat(7,GF(q));
form := QuadraticFormByMatrix(mat,GF(q));
ps := PolarSpace(form);
hq := SplitCayleyHexagon(ps);
pts := List(Points(hq));;
Collected(List(pts,x->x in ps));
lines := Set(Lines(hq));;
Collected(List(lines,x->x in ps));
lines2 := Union(List(pts,x->List(Lines(x))));;
lines = lines2;

group1 := CollineationGroup(hq);
group2 := FiningSetwiseStabiliser(CollineationGroup(ps),lines2);

group1 = group2;

ptsu := List(pts,x->ElementToElement(ps,x));;
linesu := List(lines,x->ElementToElement(ps,x));;

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group1,Union(ptsu,linesu),OnProjSubspaces,rel);;
Diameter(graph);
Girth(graph);

### 3.
ps := SymplecticSpace(5,q);
hq := SplitCayleyHexagon(ps);
pts := List(Points(hq));;
Collected(List(pts,x->x in ps));
lines := Set(Lines(hq));;
Collected(List(lines,x->x in ps));
lines2 := Union(List(pts,x->List(Lines(x))));;
lines = lines2;

group1 := CollineationGroup(hq);
group2 := FiningSetwiseStabiliser(CollineationGroup(ps),lines2);

group1 = group2;

ptsu := List(pts,x->ElementToElement(ps,x));;
linesu := List(lines,x->ElementToElement(ps,x));;

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group2,Union(ptsu,linesu),OnProjSubspaces,rel);;
Diameter(graph);
Girth(graph);

### 4.
mat := IdentityMat(6,GF(q)){[6,5,4,3,2,1]};
form := BilinearFormByMatrix(mat,GF(q));
ps := PolarSpace(form);
hq := SplitCayleyHexagon(ps);
pts := List(Points(hq));;
Collected(List(pts,x->x in ps));
lines := Set(Lines(hq));;
Collected(List(lines,x->x in ps));
lines2 := Union(List(pts,x->List(Lines(x))));;
lines = lines2;

group1 := CollineationGroup(hq);
group2 := FiningSetwiseStabiliser(CollineationGroup(ps),lines2);

group1 = group2;

ptsu := List(pts,x->ElementToElement(ps,x));;
linesu := List(lines,x->ElementToElement(ps,x));;

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group1,Union(ptsu,linesu),OnProjSubspaces,rel);;
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


     mp:=d->[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],  
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  2*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  d,  0,  0,  1]]*One(f);  ## **

     ml:=d->[[1, -d,  0,  0,  0,  0,  0],
             [0,  1,  0,  0,  0,  0,  0],  
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  0,  1,  0,  0,  0],  
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  d,  1,  0],  
             [0,  0,  0,  0,  0,  0,  1]]*One(f);

#hvm boek: (ok for q=3).

     mp:=d->[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],  
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  2*d,  0,  0,  1]]*One(f);  ## **

#try (ok for q=4)

     mp:=d->[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  2*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  d,  0,  0,  1]]*One(f);  ## **

#try (ok for q=3,4)

     mp:=d->[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  4*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  -d,  0,  0,  1]]*One(f);  ## **


#try (ok for q=5)

     mp:=d->[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  8*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  -d,  0,  0,  1]]*One(f);  ## **

#try (ok for q=3,4,5,7)

     mp:=d->[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],
             [0,  0,  1,  0,  0,  0,  0],  
             [0,  0,  -2*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],  
             [0,  0,  0,  0,  0,  1,  0],  
             [0,  0,d^2,  -d,  0,  0,  1]]*One(f);  ## **


nonzerof := Filtered(f,x->not IsZero(x));
mats := List(nonzerof,x->mp(x));
gens := List(mats,x->CollineationOfProjectiveSpace(PG(6,f),x));
List(gens,x->x in group);
mats := List(nonzerof,x->ml(x));
gens := List(mats,x->CollineationOfProjectiveSpace(PG(6,f),x));
List(gens,x->x in group);


####### new tests of SplitCayleyHexagon

q := 5;
hq := SplitCayleyHexagon(q);
pts6 := AsList(Points(hq));;
lines1 := Union(List(pts6,x->List(Lines(x))));;
lines2 := Set(Lines(hq));;
lines1 = lines2;



#planes6 := List(pts6,x->OnePointToPlane(x));
planes6 := List(pts6,x->SplitCayleyPointToPlane(x));;
planes6 := List(pts6,x->SplitCayleyPointToPlane5(x));;

pg := PG(6,q);
pg := PG(5,q);

flags := [];
for i in [1..Length(pts6)] do
flags[i] := FlagOfIncidenceStructure(pg,[pts6[i],planes6[i]]);
od;
shads6 := List(flags,x->ShadowOfFlag(pg,x,2));;
lines6 := List(shads6,x->List(x));;
lines6 := Union(lines6);;

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


######### twisted triality :-) ##########

lines_pts := List(pts,x->Filtered(lines,y->x*y));;
spans := List(lines_pts,x->Span(x));;
frob := FrobeniusAutomorphism(GF(q^3));
g := CollineationOfProjectiveSpace(IdentityMat(8,GF(q^3)),frob,GF(q^3));
List(spans,x->Number(Points(x),y->y in ZeroPointToOnePointsSpaceByTriality(y)));






#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. <geom> is a generalised hexagong, so an ambient polar space
# ps is available. A point of geom is necessary a point of ps, but for T(q^3,q) we need
# to check whether the point of Q+(7,q) is absolute.
##
InstallMethod( VectorSpaceToElement,
    "for a generalised hexagon and a row vector",
    [ IsLieGeometry and IsGeneralisedHexagon, IsPlistRep ],
    function(geom,vec)
		local x,y, ps, el, p1, p2, pg, mat, form, y, frob, f;
		## when v is empty... 
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;

		x := MutableCopyMat(v);
		TriangulizeMat(x);
		## dimension should be correct
		if Length(v[1]) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## Remove zero rows. It is possible the the user
		## has inputted a matrix which does not have full rank
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		if i = n then
			return EmptySubspace(geom);
		fi;
		x := x{[1..n-i]};
		if Length(x) > Rank(geom) then
			Error("<v> does not represent any element of <geom>");
		fi;
		# here we are sure that x produces a point or line of AmbientSpace(geom);
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		if Length(x) = 1 then
			if ps!.dimension = 5 then
				return Wrap(geom, 1, y[1] );
			elif ps!.dimension = 6 then
				form := QuadraticForm(ps);
				if not IsTotallySingularSubspace(QuadraticForm(geom),x) then
					Error("<x> does not generate an element of <geom>");
				fi;
				return Wrap(geom, 1, y[1] );
			else
				form := QuadraticForm(ps);
				if not IsTotallySingularSubspace(QuadraticForm(geom),x) then
					Error("<x> does not generate an element of <geom>");
				fi;
				f := ps!.basefield;
				frob := FrobeniusAutomorphism(f);
				ZeroPointToOnePointsSpaceByTriality(y,frob,f)
				
	
	
	ps := AmbientPolarSpace(geom);
    #first check wheter vec makes a point of ps
	if ps!.dimension <> 5 then
		form := QuadraticForm(ps);
		if not IsTotallySingularSubspace(QuadraticForm(geom),x) then
			Error("<x> does not generate an element of <geom>");
		fi;
	else
		form := SesquilinearForm(ps);
		if not IsTotallyIsotropicSubspace(SesquilinearForm(geom),x) then
				Error("<x> does not generate an element of <geom>");
		fi;

	el := VectorSpaceToElement(ps,vec); #this might produce an error already. refine the message later.
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
        if p1 in Wrap(pg,3,SplitCayleyPointToPlane(p2)) then
            return Wrap(geom, 2, UnderlyingObject(el));
        else
            Error("<vec> does not represent a line of <geom>");
        fi;
    #elif IsHyperbolicQuadric(ps) then
    #    if p1 in ZeroPointToOnePointsSpaceByTriality(p1) and 
    #            p2 in ZeroPointToOnePointsSpaceByTriality(p2) and
    #            p1 in ZeroPointToOnePointsSpaceByTriality(p2)
    #    then
    #        return Wrap(geom, 2, UnderlyingObject(el));
    #    else
    #        Error("<mat> does not represent a line of <geom>");
    #    fi;
    elif IsSymplecticSpace(ps) then
        if p1 in Wrap(pg,3,SplitCayleyPointToPlane5(p2)) then
            return Wrap(geom, 2, UnderlyingObject(el));
        else
            Error("<vec> does not represent a line of <geom>");
        fi;
    fi;
end );
