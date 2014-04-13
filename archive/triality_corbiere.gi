one_point_to_space := function(y,q)
local z,hyps;
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
hyps := List(z,x->HyperplaneByDualCoordinates(PG(7,q),x));
return Meet(hyps);
end;

two_point_to_space := function(z,q)
local y,hyps;
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
hyps := List(y,x->HyperplaneByDualCoordinates(PG(7,q),x));
return Meet(hyps);
end;

OnePointToSpace := function(pg,y)
    local z, hyps,q;
    q := Size(BaseField(pg));
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

TwoPointToSpace := function(pg,z)
    local y,hyps, q;
    q := Size(BaseField(pg));
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

planes7 := List(pts,x->Meet(OnePointToSpace(x),TwoPointToSpace(x)));

hyp := HyperplaneByDualCoordinates(PG(7,q),[0,0,0,1,0,0,0,-1]*Z(q)^0);

mat7 := [ [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0 ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ] ];
qform := QuadraticFormByMatrix(mat7,GF(q));
ps7 := PolarSpace(qform);

planes7 := List(planes7,x->ElementToElement(ps7,x));

em := NaturalEmbeddingBySubspace(ps,ps7,hyp);

List(planes7,x->PreImageElm(em,x));

planes6 := List(planes7,x->VectorSpaceToElement(PG(6,3),truncate(UnderlyingObject(x))));

hexlinespg := List(lines,x->ElementToElement(PG(6,3),x));

flags := [];
for i in [1..Size(pts)] do
    flags[i] := FlagOfIncidenceStructure(PG(6,3),[pts[i],planes6[i]]);
od;

shads := List(flags,x->List(ShadowOfFlag(PG(6,3),x,2)));

List(shads,x->Intersection(x,hexlinespg));

OnePointVecToSpaceVec := function(y,q)
    local z, mat;
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
    return NullspaceMat(TransposedMat(z));
end;

TwoPointVecToSpaceVec := function(z,q)
    local y;
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
    return NullspaceMat(TransposedMat(y));
end;

truncate := function(mat)
    return List(mat,x->x{[1..7]});
end;

embed_vec := function(cvec)
    local vec;
    vec := Unpack(cvec);
    vec[8] := -vec[4];
    return vec;
end;

OnePointToSplitCayleyPlane := function(el)
    local pg, q, y, z, int, meet, list;
    pg := AmbientSpace(el);
    q := Size(BaseField(pg));
    y := Unpack(UnderlyingObject(el));
    y[8] := -y[4];
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
	list := NullspaceMat(TransposedMat(z));
    int := IdentityMat(8,GF(q)){[1..7]};
    int[4][8] := -Z(q)^0;
    meet := SumIntersectionMat(list, int)[2];
    return VectorSpaceToElement(pg,List(meet,x->x{[1..7]}));
end;

pts := List(Points(hex));
lines := List(Lines(hex));
ps := AmbientPolarSpace(hex);
pg := AmbientSpace(ps);
planes := List(pts,x->OnePointToSplitCayleyPlane(x));
shads1 := List(pts,x->Filtered(lines,y->x in y));;
shads1 := List(shads1,x->Set(List(x,y->ElementToElement(pg,y))));
flags := List(pts,x->FlagOfIncidenceStructure(pg,[x,OnePointToSplitCayleyPlane(x)]));
shads2 := List(flags,x->Set(List(ShadowOfFlag(pg,x,2))));


