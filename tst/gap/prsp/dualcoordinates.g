#DualCoorindates
ps := PG(3,19);
hyp := VectorSpaceToElement(ps,[[1,3,5,7],[0,2,4,0],[8,12,17,0]]*Z(19)^0);
DualCoordinatesOfHyperplane(hyp);
quit;
