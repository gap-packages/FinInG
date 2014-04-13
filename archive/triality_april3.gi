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
planes6 := List(pts6,x->OnePointToPlane(x));

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
