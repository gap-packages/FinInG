# ReadPackage("desargues","tst/handledbyniceepi.g");
g := GL(3,5);
v := GF(5)^3;
l := Elements(v);;
l := Set(List(v,x->OnLines(x,One(g))));
l := l{[2..Length(l)]};
epi := ActionHomomorphism(g,l,OnLines,"surjective");
DeclareProperty("IsHandledByNiceEpimorphism",IsMatrixGroup,NICE_FLAGS);
DeclareAttribute("NiceEpimorphism",IsMatrixGroup);
DeclareAttribute("NiceEpiObject",IsMatrixGroup);
DeclareAttribute("NiceEpiKernelGens",IsMatrixGroup);
SetIsHandledByNiceEpimorphism(g,true);
SetNiceEpimorphism(g,epi);
SetNiceEpiObject(g,Image(epi));
SetNiceEpiKernelGens(g,[One(g)*Z(5)]);

InstallMethod( SylowSubgroupOp, "via nice epi", 
  [IsGroup and IsHandledByNiceEpimorphism, IsPosInt],
  function( g, p )
    local epi,gens,h,ps,s;
    h := NiceEpiObject(g);
    epi := NiceEpimorphism(g);
    s := SylowSubgroup(h,p);
    gens := GeneratorsOfGroup(s);
    ps := List(GeneratorsOfGroup(s),x->PreImagesRepresentative(epi,x));
    Append(ps,NiceEpiKernelGens(g));
    return SubgroupNC(g,ps);
  end );

s := SylowSubgroup(g,5);
Size(s);
GeneratorsOfGroup(s);
