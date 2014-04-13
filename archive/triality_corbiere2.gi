OnePointToSpace := function(el)
    local z, hyps, q, y, pg;
    q := Size(BaseField(el));
    y := Unpack(UnderlyingObject(el));
    #y[8] := -y[4];
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

TwoPointToSpace := function(el)
    local y, hyps, q, z, pg;
    q := Size(BaseField(el));
    z := Unpack(UnderlyingObject(el));
    #z[8] := -z[4];
    pg := AmbientSpace(el);
    y := [];
    y[1] := [0,-z[3],z[2],0,z[4],0,0,z[5]]*Z(q)^0;
    y[2] := [z[3],0,-z[1],0,0,z[4],0,z[6]]*Z(q)^0;
    y[3] := [-z[2],z[1],0,0,0,0,z[4],z[7]]*Z(q)^0;
    y[4] := [z[5],z[6],z[7],-z[4],0,0,0,0]*Z(q)^0;
    y[5] := [z[8],0,0,z[1],0,-z[7],z[6],0]*Z(q)^0;
    y[6] := [0,z[8],0,z[2],z[7],0,-z[5],0]*Z(q)^0;
    y[7] := [0,0,z[8],z[3],-z[6],z[5],0,0]*Z(q)^0;
    y[8] := [0,0,0,0,z[1],z[2],z[3],-z[8]]*Z(q)^0;
    y := Filtered(y,x->not IsZero(x));
    hyps := List(y,x->HyperplaneByDualCoordinates(pg,x));
    return Meet(hyps);
end;

mat := NullMat(8,8,GF(q));
mat[1][5] := Z(q)^0;
mat[2][6] := Z(q)^0;
mat[3][7] := Z(q)^0;
mat[4][8] := Z(q)^0;
form := QuadraticFormByMatrix(mat,GF(q));
ps := PolarSpace(form);
pts := AsList(Points(ps));

absolute1 := Filtered(pts,x->x in OnePointToSpace(x));
absolute2 := Filtered(pts,x->x in TwoPointToSpace(x));
hyp := Span(absolute1);
planes := List(absolute1,x->Meet(OnePointToSpace(x),TwoPointToSpace(x)));
List(planes,x->x in hyp);

q6q := ParabolicQuadric(6,q);
em := NaturalEmbeddingBySubspace(q6q,ps,hyp);
pts6 := List(absolute1,x->PreImageElm(em,x));
planes6 := List(planes,x->PreImageElm(em,x));

flags := [];
for i in [1..Length(pts6)] do
flags[i] := FlagOfIncidenceStructure(PG(6,q),[pts6[i],planes6[i]]);
od;
shads6 := List(flags,x->ShadowOfFlag(PG(6,q),x,2));
lines6 := List(shads6,x->List(x));
lines6 := Union(lines6);

group := FiningSetwiseStabiliser(CollineationGroup(q6q),lines6);

rel := function(x,y)
if x!.type=y!.type then
return false;
else
return x * y;
fi;
end;

graph := Graph(group,Union(pts6,lines6),OnProjSubspaces,rel);
Diameter(graph);
Girth(graph);

q := 2;
h := TwistedTrialityHexagon(2^3);
pts := AsList(Points(h));
ps := AmbientPolarSpace(h);
ptsps := AsList(Points(ps)); #240 s

Triality7 := function(el,frob)
    local z, hyps, q, y, pg;
    q := Size(BaseField(el));
    y := Unpack(UnderlyingObject(el))^frob;
    #y[8] := -y[4];
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

test := Filtered(ptsps,x->x in Triality7(x,frob));; # 645269 ms



