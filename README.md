[![Build Status](https://github.com/gap-packages/FinInG/workflows/CI/badge.svg?branch=master)](https://github.com/gap-packages/FinInG/actions?query=workflow%3ACI+branch%3Amaster)
[![Code Coverage](https://codecov.io/github/gap-packages/FinInG/coverage.svg?branch=master&token=)](https://codecov.io/gh/gap-packages/FinInG)

# FinInG -- Finite Incidence Geometry

- John Bamberg (University of Western Australia)
- Anton Betten (Colorado State University, Fort Collins)
- Philippe Cara (Vrije Universiteit Brussel)
- Jan De Beule (Vrije Universiteit Brussel)
- Michel Lavrauw (University of Primorska)
- Max Neunhoeffer (University of St. Andrews)
                                   

This package provides:

- functionality to create and explore finite incidence structures, such as
	finite projective spaces, finite classical polar spaces, generalised polygons,
	coset geometries, finite affine spaces, and many more;

- functionality to explore algebraic varieties in finite projective and finite affine spaces;
	
- functionality that deals with the automorphism groups of incidence structures,
	and functionality integrating these automorphism groups with the group theoretical
	capabilities of GAP;

- functionality to explore various morphisms between finite incidence structures.


For further information see:
	
	https://github.com/gap-packages/FinInG

## INSTALLATION AND DEPENDENCIES

FinInG requires the following packages: GAPDoc, GenSS, forms, GRAPE, orb, io, and cvec.
The DESIGN package is not required to load FinInG, but is needed to use some particular functions
for generalised polygons. The required packages are all available through the website of GAP, and
are usually installed by default. However, for some of these packages, compilation might still 
be required. The complete installation procedure is explained in the INSTALL file, and in the
FinInG manual. Here we continue with the summary for the experienced or impatient user.

Just unpack one of the archives in the "pkg" subdirectory of your GAP 
installation. The archive is available in several formats:

    fining-x.y.tgz  
    fining-x.y.zip 

Unpacking generates a subdirectory "fining". If the dependencies are satisfied, you can load the package
by typing in the GAP command line "LoadPackage("fining");". All documentation and
examples are included.

Have a lot of fun!

The authors,

John Bamberg, Anton Betten, Philippe Cara, Jan De Beule, Michel Lavrauw and Max Neunhoeffer.
