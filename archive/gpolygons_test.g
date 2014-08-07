q := 5;
ps := ParabolicQuadric(4,q);
ps := EllipticQuadric(5,q);
pts := Set(AsList(Points(ps)));;
lines := Set(AsList(Lines(ps)));;
inc := \*;
group := CollineationGroup(ps);
gp := GeneralisedPolygonByElements(pts,lines,inc);
gp := GeneralisedPolygonByElements(pts,lines,inc,group, \^);
graph := IncidenceGraphOfGeneralisedPolygon(gp);;
vn := VertexNames(graph);;

p := Random(Points(gp));
test1 := Set(List(Lines(p)));;
test2 := Set(List(vn{Adjacency(graph,Position(vn,Unwrap(p)))},x->Wrap(gp,2,x)));;


#### our best friend T_2(0)
conic := Set(Points(ParabolicQuadric(2,q)));
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(conic,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
phi := PolarityOfProjectiveSpace(ParabolicQuadric(2,q));
tangents := List(conic,x->x^phi);
lines := List(tangents,x->x^em);
planes := List(lines,x->Filtered(Planes(x),y->not y in hyp));
points2 := Union(planes);
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
linesb := Set(O);
pts := Union(points1,points2,points3);
lns := Union(linesa,linesb);
inc := \*;
gp := GeneralisedPolygonByElements(pts,lns,inc,stab,\^);

###### our best friend again #####
q := 5;
conic := Set(Points(ParabolicQuadric(2,q)));
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(conic,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
tangents := List(conic,x->TangentSpace(x)^em);
planes := List(tangents,x->Filtered(Planes(x),y->not y in hyp));
points2 := Union(planes);
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
linesb := Set(O);
pts := Union(points1,points2,points3);
lns := Union(linesa,linesb);
inc := \*;
gp := GeneralisedPolygonByElements(pts,lns,inc,stab,\^);

###### our best friend again and again#####
q := 8;
f := GF(q);
vecs := Union(List(f,t->List(f,s->[One(f),t,t^4])));
Add(vecs,[0,0,1]*One(f));
oval := List(vecs,x->VectorSpaceToElement(PG(2,q),x));;
tangents := Union(List(oval,x->Filtered(Lines(x),x->Number(oval,y->y in x)=1)));;
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(oval,x->x^em);
tangents := List(tangents,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
tangents := List(conic,x->TangentSpace(x)^em);
planes := List(tangents,x->Filtered(Planes(x),y->not y in hyp));
points2 := Union(planes);
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
linesb := Set(O);
pts := Union(points1,points2,points3);
lns := Union(linesa,linesb);
inc := \*;
gp := GeneralisedPolygonByElements(pts,lns,inc,stab,\^);
time;

###### T_2(O)* #####
q := 4;
conic := ParabolicQuadric(2,q);
nucleus := NucleusOfParabolicQuadric(conic);
hyperoval := Union(List(Points(conic)),[nucleus]);
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(hyperoval,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points := Set(Filtered(Points(pg),x->not x in hyp));;
lines := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
inc := \*;
gp := GeneralisedPolygonByElements(points,lines,inc);
gp := GeneralisedPolygonByElements(points,lines,inc,stab,\^);


InstallMethod( GeneralisedPolygonByBlocks,
    "for a homogeneous list",
    [ IsHomogeneousList ],
    function( blocks )
        local pts, gp, ty, i, graph, sz, adj, girth, shadpoint, shadline, s, t, dist, vn, 
		listels, objs;
        pts := Union(blocks);
        s := Size(blocks[1]) - 1;
        if not ForAll(blocks, b -> Size(b) = s + 1 ) then
            Error("Not every block has size ", s + 1);
        fi;
        
        i := function( x, y )
        if IsSet( x!.obj ) and not IsSet( y!.obj ) then
            return y!.obj in x!.obj;
        elif IsSet( y!.obj ) and not IsSet( x!.obj ) then
            return x!.obj in y!.obj;
        else
            return false;
        fi;
        end;
        
        sz := Size(pts);
        
        adj := function(x,y)
             if x <= sz and y > sz then
                return x in blocks[y-sz];
             elif y <= sz and x > sz then
                return y in blocks[x-sz];
             else
                return false;
             fi;
        end;

        graph := Graph(Group(()), [1..sz+Size(blocks)], OnPoints, adj );
        girth := Girth(graph);

        if IsBipartite(graph) then
            if not girth = 2*Diameter(graph) then
                Error("<blocks> are not defining a generalised polygon");
            fi;
        else
            Error("<blocks are not defining a generalised polygon");
        fi;
        
        if girth = 6 then
            ty := NewType( GeometriesFamily, IsProjectivePlane and IsGeneralisedPolygonRep );
        elif girth = 8 then
            ty := NewType( GeometriesFamily, IsGeneralisedQuadrangle and IsGeneralisedPolygonRep );
        elif girth = 12 then
            ty := NewType( GeometriesFamily, IsGeneralisedHexagon and IsGeneralisedPolygonRep );
        elif girth = 16 then
            ty := NewType( GeometriesFamily, IsGeneralisedOctagon and IsGeneralisedPolygonRep );
        else
            Error("<blocks> do not define a thick finite generalised polygon");
        fi;
        
        listels := function( geom, i )
			if i = 1 then
				return List(pts,x->Wrap(geom,i,x));
			else
				return List(blocks,x->Wrap(geom,i,x));
			fi;
		end;
				
		vn := VertexNames(graph);
		
        shadpoint := function( pt )
            return List(Filtered(blocks,x->pt!.obj in x),y->Wrap(pt!.geo,2,y));
        end;
        
        shadline := function( line )
            return List(line!.obj,x->Wrap(line!.geo,1,x));
        end;
		
		#we have the graph now, the following is efficient.
		t := Length(Adjacency(graph,1)); # number of linbes on a point minus 1.

        dist := function( el1, el2 )
			if el1!.type=1 and el2!.type=1 then
				return Distance(graph,Position(pts,el1!.obj),Position(pts,el2!.obj));
			elif el1!.type=1 and el2!.type=2 then
				return Distance(graph,Position(pts,el1!.obj),sz+Position(blocks,el2!.obj));
			elif el1!.type=2 and el2!.type=1 then
				return Distance(graph,sz+Position(blocks,el1!.obj),Position(pts,el2!.obj));
			else 
				return Distance(graph,sz+Position(blocks,el1!.obj),sz+Position(blocks,el2!.obj));
			fi;
        end;

        gp := rec( pointsobj := pts, linesobj := blocks, incidence := i, listelements := listels, 
					shadowofpoint := shadpoint, shadowofline := shadline, distance := dist );

        Objectify( ty, gp );
        SetTypesOfElementsOfIncidenceStructure(gp, ["point","line"]);
        SetOrder(gp, [s, t]);
        SetRankAttr(gp, 2);
        Setter( IncidenceGraphOfGeneralisedPolygonAttr )( gp, graph );
        return gp;
  end );

############


#############################################################################
#O  CollineationGroup( <gp> )
###
InstallMethod( CollineationGroup, 
    "for a generalised polygon",
    [ IsElationGQ and IsGeneralisedPolygonRep ],
    function( gp )
        local graph, aut, act, stab, coll, ptsn;
        graph := IncidenceGraphOfGeneralisedPolygon( gp );
        aut := AutomorphismGroup( graph );
        ptsn := Set(gp!.pointsobj,x->Position(VertexNames(graph),x));
        stab := Stabilizer(aut, ptsn, OnSets);
        coll := Action(stab, ptsn, OnPoints);
		act := function(el,g)
			local src,img;
			if el!.type = 1 then
				src := Position(VertexNames(graph),el); #change wrt generic function which would be el!.obj
				img := src^g;
				return VertexNames(graph)[img];
			elif el!.type = 2 then
				src := Position(VertexNames(graph),el); #change wrt generic funciton ... 
				img := src^g;
				return VertexNames(graph)[img];
			fi;
		end;
        SetCollineationAction( coll, act );
		return coll;
    end );
############



gp := egq;
coll := CollineationGroup(gp);
p := Random(Points(gp));
l := Random(Lines(gp));
g := Random(coll);
act := CollineationAction(coll);
act(p,g);
act(l,g);


#### nice blt example.
next example: q=5 is feasable and shows interesting timing differences.
q := 5;
qc := LinearqClan(q);
egq1 := EGQByqClan(qc);
time;
coll1 := CollineationGroup(egq1);
time;
Order(coll1);
Order(CollineationGroup(HermitianPolarSpace(3,q^2)));
blt1 := BLTSetByqClan(qc);
time;
egq2 := EGQByBLTSet(blt1);
time;
coll2 := CollineationGroup(egq2);

next example: q=5 is feasable in second test case. Interesting non classical GQ of order 25,5
q := 5;
qc := FisherThasWalkerKantorBettenqClan(q);
egq1 := EGQByqClan(qc);
time;
coll1 := CollineationGroup(egq1);
time;
Order(coll1);
Order(CollineationGroup(HermitianPolarSpace(3,q^2)));
blt1 := BLTSetByqClan(qc);
time;
egq2 := EGQByBLTSet(blt1);
time;
coll2 := CollineationGroup(egq2);

q := 5;
qc := FisherqClan(q);
egq1 := EGQByqClan(qc);
time;
coll1 := CollineationGroup(egq1);
time;
Order(coll1);
Order(CollineationGroup(HermitianPolarSpace(3,q^2)));
blt1 := BLTSetByqClan(qc);
time;
egq2 := EGQByBLTSet(blt1);
time;
coll2 := CollineationGroup(egq2);


########### distances in GHs.

q := 3;
hq := SplitCayleyHexagon(q);
#ps := ParabolicQuadric(6,q);
#ps := SymplecticSpace(5,q);
#hq := SplitCayleyHexagon(ps);
#ps := HyperbolicQuadric(7,2^3);
#hq := TwistedTrialityHexagon(2^3);
#hq := TwistedTrialityHexagon(ps);

coll := CollineationGroup(hq);
graph := IncidenceGraphOfGeneralisedPolygon(hq);;
vn := VertexNames(graph);;
p := Random(Points(hq));
pn := Position(vn,p);
two := Filtered(Points(hq),x->DistanceBetweenElements(p,x)=2);;
four := Filtered(Points(hq),x->DistanceBetweenElements(p,x)=4);;
six := Filtered(Points(hq),x->DistanceBetweenElements(p,x)=6);;
twon := Filtered(Vertices(graph),x->Distance(graph,pn,x)=2);;
fourn := Filtered(Vertices(graph),x->Distance(graph,pn,x)=4);;
sixn := Filtered(Vertices(graph),x->Distance(graph,pn,x)=6);;
Length(two)=Length(twon);
Length(four)=Length(fourn);
Length(six)=Length(sixn);
Set(two)=Set(vn{twon});
Set(four)=Set(vn{fourn});
Set(six)=Set(vn{sixn});

p := Random(Lines(hq));
pn := Position(vn,p);
two := Filtered(Lines(hq),x->DistanceBetweenElements(p,x)=2);;
four := Filtered(Lines(hq),x->DistanceBetweenElements(p,x)=4);;
six := Filtered(Lines(hq),x->DistanceBetweenElements(p,x)=6);;
twon := Filtered(Vertices(graph),x->Distance(graph,pn,x)=2);;
fourn := Filtered(Vertices(graph),x->Distance(graph,pn,x)=4);;
sixn := Filtered(Vertices(graph),x->Distance(graph,pn,x)=6);;
Length(two)=Length(twon);
Length(four)=Length(fourn);
Length(six)=Length(sixn);
Set(two)=Set(vn{twon});
Set(four)=Set(vn{fourn});
Set(six)=Set(vn{sixn});

p := Random(Points(hq));
pn := Position(vn,p);
one := Filtered(Lines(hq),x->DistanceBetweenElements(p,x)=1);;
three := Filtered(Lines(hq),x->DistanceBetweenElements(p,x)=3);;
five := Filtered(Lines(hq),x->DistanceBetweenElements(p,x)=5);;
onen := Filtered(Vertices(graph),x->Distance(graph,pn,x)=1);;
threen := Filtered(Vertices(graph),x->Distance(graph,pn,x)=3);;
fiven := Filtered(Vertices(graph),x->Distance(graph,pn,x)=5);;
Length(one)=Length(onen);
Length(three)=Length(threen);
Length(five)=Length(fiven);
Set(one)=Set(vn{onen});
Set(three)=Set(vn{threen});
Set(five)=Set(vn{fiven});

p := Random(Lines(hq));
pn := Position(vn,p);
one := Filtered(Points(hq),x->DistanceBetweenElements(p,x)=1);;
three := Filtered(Points(hq),x->DistanceBetweenElements(p,x)=3);;
five := Filtered(Points(hq),x->DistanceBetweenElements(p,x)=5);;
onen := Filtered(Vertices(graph),x->Distance(graph,pn,x)=1);;
threen := Filtered(Vertices(graph),x->Distance(graph,pn,x)=3);;
fiven := Filtered(Vertices(graph),x->Distance(graph,pn,x)=5);;
Length(one)=Length(onen);
Length(three)=Length(threen);
Length(five)=Length(fiven);
Set(one)=Set(vn{onen});
Set(three)=Set(vn{threen});
Set(five)=Set(vn{fiven});


########## T_3(O) ########## kijken wat dit geeft....
f := GF(8);
vecs := Union(List(f,t->List(f,s->[t^4+s*t+s^6,One(f),s,t])));
Add(vecs,[1,0,0,0]*One(f));
ovoid := List(vecs,x->VectorSpaceToElement(PG(3,f),x));;
tangents := Union(List(ovoid,x->Filtered(Planes(x),x->Number(ovoid,y->y in x)=1)));;
pg := PG(4,f);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0,0]*One(f));
em := NaturalEmbeddingBySubspace(PG(3,f),pg,hyp);
O := List(ovoid,x->x^em);
tangents := List(tangents,x->x^em);;
group := CollineationGroup(pg);
stabhyp := FiningStabiliser(group,hyp);
stab := FiningSetwiseStabiliser(stabhyp,O);
time; #pm 235 seconds.
solids := List(tangents,x->Filtered(Solids(x),y->not y in hyp));;
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
points2 := Union(solids);;
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));;
linesb := Set(O);
pts := Union(points1,points2,points3);;
lns := Union(linesa,linesb);;
inc := \*;
gp := GeneralisedPolygonByElements(pts,lns,inc,stab,\^);
time; #pm 31 seconds
coll := CollineationGroup(gp);
time;

#### GP By Elements (new version)? #####

    adj2 := function(x,y)
    if els[x] in pts and els[y] in pts then
        return false;
    elif els[x] in lns and els[y] in lns then
        return false;
    else
        return inc(els[x],els[y]);
    fi;
    end;

