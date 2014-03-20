#commuting polarities 2
w := SymplecticSpace(3,64);
h := HermitianPolarSpace(3,64);
x := PolarityOfProjectiveSpace(w);
y := PolarityOfProjectiveSpace(h);
x * y = y * x;
tau := x * y;

points := AsList(Points(h));;
fixed := Filtered(points, t -> t^tau = t);;
g := CollineationGroup(h);
stab := FiningSetwiseStabiliser(g, fixed);
DisplayCompositionSeries(stab); # yep it works!

pg := AmbientSpace(h);
em := Embedding( CollineationGroup(pg), CorrelationCollineationGroup(pg));
tau2 := PreImageElm(em,tau); # minor bug, but can be easily fixed
tau2 := PreImagesRepresentative(em,tau);
cent := Centralizer(g, tau2);
DisplayCompositionSeries(cent);
quit;
