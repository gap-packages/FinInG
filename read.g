#############################################################################
##
##  read.g              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2014	Colorado State University, Fort Collins
##					Universitˆ degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Reading the implementation part of the FinInG package.
##
#############################################################################

if InstalledPackageVersion("forms") < "1.2" then
   ReadPackage("fining","gap/forms_ext.gi"); 
fi;
if InstalledPackageVersion("forms") < "1.2.2" then
   Forms_RESET := RESET; 
fi;
if InstalledPackageVersion("forms") <= "1.2.2" then
   ReadPackage("fining","gap/forms_patch.gi"); 
fi;

ReadPackage("fining","gap/genss_patch.g");

ReadPackage("fining","gap/geometry.gi");

ReadPackage("fining","gap/liegeometry.gi"); 

ReadPackage("fining","gap/group.gi"); 

ReadPackage("fining","gap/projectivespace.gi");

ReadPackage("fining","gap/correlations.gi");

ReadPackage("fining","gap/polarspace.gi");
ReadPackage("fining","gap/morphisms.gi");

ReadPackage("fining","gap/enumerators.gi");

ReadPackage("fining","gap/diagram.gi");

ReadPackage("fining","gap/varieties.gi");

ReadPackage("fining","gap/affinespace.gi");
ReadPackage("fining","gap/affinegroup.gi");

ReadPackage("fining","gap/gpolygons.gi");

ReadPackage("fining","gap/orbits-stabilisers.gi"); # added 11/02/13 ml

Print("\n");

