##########################################################################
##
##  group.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2018	Colorado State University
##                  Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universiteit Gent
##					University of St. Andrews
##					University of Western Australia
##                  Vrije Universiteit Brussel
##
##
##  Implementation stuff for our "projective groups"
##
############################################################################

## helping function. came from projectivespace.gi

# CHECKED 5/09/11 jdb
# We are currently CVec'ing everything. I decide to keep this function as is, and
# convert its output where appriopriate to a list of cvecs
# This makes that converting the vectors here to a dirty GAP vector is useless, so I
# uncommented the if q <= 256 statement.
#############################################################################
#F  MakeAllProjectivePoints( f,d )
# Global function to compute all points of a projective space. Not for users.
# <f>: field; <d>: *projective* dimension.
##
InstallGlobalFunction( MakeAllProjectivePoints, 
function(f,d)

  # f is a finite field
  # d an integer >= 1
  # This function is used for computing the permutation representation (NiceMonomorphism)
  # quickly and with the least memory used as possible, for a projective group.
  # Later, we will convert everything here to CVec's, which should give us an
  # improved permutation representation. For example:
  #   gap> pg:=PG(5,5);
  #   ProjectiveSpace(5, 5)
  #   gap> g:=ProjectivityGroup(pg);
  #   PGL(6,5)
  #   gap> hom := NiceMonomorphism(g);
  #   <action isomorphism>
  #   gap> omega:=UnderlyingExternalSet(hom);;
  #   gap> Random(omega);
  #   ## returns a compressed vector


  local els,i,j,l,q,sp,v,vs,w,ww,x;
  els := Elements(f);
  q := Length(els);
  v := CVec([1..d+1]*Zero(f),f); # using cvecs (ml 31/03/13) 
  #v := ListWithIdenticalEntries(d+1,Zero(f));
  #if q <= 256 then ConvertToVectorRep(v,q); fi;
  vs := EmptyPlist(q^d);
  sp := EmptyPlist((q^(d+1)-1)/(q-1));
  for x in els do
    w := ShallowCopy(v);
    w[d+1] := x;
    Add(vs,w);
  od;
  w := ShallowCopy(v);
  w[d+1] := One(f);
  Add(sp,w);
  for i in [d,d-1..1] do
    l := Length(vs);
    for j in [1..l] do
      ww := ShallowCopy(vs[j]);
      ww[i] := One(f);
      Add(sp,ww);
      Add(vs,ww);
    od;
    if i > 1 then
        for x in els do
          if not(IsZero(x)) and not(IsOne(x)) then
            for j in [1..l] do
              ww := ShallowCopy(vs[j]);
              ww[i] := x;
              Add(vs,ww);
            od;
          fi;
        od;
    fi;
  od;
  return Set(sp);
end);


## make this a global function? Definitely (jdb, 6/9/11).

# CHECKED 6/09/11 jdb
#############################################################################
#F  IsFiningScalarMatrix( a)
#returns true if <a> is a scalar matrix.
##
InstallGlobalFunction(IsFiningScalarMatrix,
	function( a )
	local n;
	n := a[1,1];
	if IsZero(n) then 
		return false;
	else
		return IsOne(a/n);
	fi;
	end );


###################################################################
# Construction of "projective elements", that is matrices modulo scalars:
###################################################################

## A lot of the following is now obselete. We should consider
## deleting some of it.

# CHECKED 5/09/11 jdb
# We are in the very big cvec operation and will skip all methods for non frob groups right now. 19/3/2014.
# I am not completely happy with the behaviour of this method, since there might
# be some issue with the field.
# On the other hand, it is not intended for the user and might even be obselete
#############################################################################
#O  ProjEl( <mat> )
# method to construct an object in the category IsProjGrpEl, i.e. projectivities, 
# aka "matrices modulo scalars". This method is not intended for the users. 
# it has no checks built in. It relies on DefaultFieldOfMatrix to determine the 
# field to be used.
##
InstallMethod( ProjEl, 
	"for a ffe matrix",
	[IsMatrix and IsFFECollColl],
	function( m )
		local el, m2, f;
	    m2 := ShallowCopy( m );
		f := DefaultFieldOfMatrix(m);
		ConvertToMatrixRepNC( m2, f );
		el := rec( mat := m2, fld := f );
		Objectify( NewType( ProjElsFamily,
							IsProjGrpEl and
							IsProjGrpElRep ), el );
		return el;
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  ProjEls( <mat> )
# method to construct objects in the category IsProjGrpEl, i.e. projectivities, 
# aka "matrices modulo scalars". This method is not intended for the users.
# it has no checks built in, and is almost liek ProjEl.
## 
InstallMethod( ProjEls, "for a list of ffe matrices",
  [IsList],
  function( l )
    local el,fld,ll,m,ty,m2;
    fld := FieldOfMatrixList(l);
    ll := [];
    ty := NewType( ProjElsFamily,
                   IsProjGrpEl and
                   IsProjGrpElRep );
    for m in l do
        m2 := ShallowCopy(m);
		ConvertToMatrixRepNC( m2, fld );
        el := rec( mat := m2, fld := fld );
        Objectify( ty, el );
        Add(ll,el);
    od;
    return ll;
  end );

# CHECKED 5/09/11 jdb # changed ml 8/11/12
# changed 19/3/2014 to cmat.
#############################################################################
#O  Projectivity( <mat>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrob, but with
# field automorphism equal to the identity, i.e. a projectivity, 
# This method is intended for the user, and contains
# a check whether the matrix is non-singular.
## 
InstallMethod( Projectivity, [ IsMatrix and IsFFECollColl, IsField],
    ## A bug was found here, during a nice August afternoon involving pigeons,
    ## where the variable m2 was assigned to the size of the field.
    ## jdb 13/12/08, Giessen, cold saturday afternoon. I still remember the
    ## pigeons, so does my computer. I add some lines now to check whether the
    ## matrix is non singular. 
  	function( mat, gf )
		local el, m2, fld, frob, cmat;
		m2 := ShallowCopy(mat);
		#if NrCols(m2) <> NrRows(m2) then
		#	Error("<mat> must be a square matrix");
		#fi;
		if Rank(m2) <> Size(m2) then
			Error("<mat> must not be singular");
		fi;
		#ConvertToMatrixRep( m2, gf );
		cmat := NewMatrix( IsCMatRep, gf, Size(m2) , m2);
		el := rec( mat := cmat, fld := gf, frob := FrobeniusAutomorphism(gf)^0 );
		Objectify( ProjElsWithFrobType, el );
		return el;
	end );

# added 19/3/2014 (cmat).
#############################################################################
#O  Projectivity( <mat>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrob,
# as above, with input a cmat and field
## 
InstallMethod( Projectivity, [ IsCMatRep and IsFFECollColl, IsField],
  	function( mat, gf )
		local el, m2, fld, frob, cmat;
		m2 := ShallowCopy(mat);
		#if NrCols(m2) <> NrRows(m2) then
		#	Error("<mat> must be a square matrix");
		#fi;
		if Rank(m2) <> NrRows(m2) then
			Error("<mat> must not be singular");
		fi;
		#ConvertToMatrixRep( m2, gf );
		el := rec( mat := m2, fld := gf, frob := FrobeniusAutomorphism(gf)^0 );
		Objectify( ProjElsWithFrobType, el );
		return el;
	end );


# Added ml 8/11/2012
#############################################################################
#O  Projectivity( <pg>, <mat> )
# method to construct an object in the category IsProjGrpEl, i.e. a projectivity, 
# This method is intended for the user, and contains
# a check whether the matrix is non-singular.
## 
InstallMethod( Projectivity, [ IsProjectiveSpace, IsMatrix],
#
  	function( pg, mat )
		local d,gf,m2;
		d:=Dimension(pg);
		gf:=pg!.basefield;
		if d <> NrRows(mat)-1 then
			Error("The arguments <mat> and <pg> are incompatible");
		fi;
		m2 := ShallowCopy(mat);
		#if NrCols(m2) <> NrRows(m2) then
		#	Error("<mat> must be a square matrix");
		#fi;
		if Rank(m2) <> NrRows(m2) then
			Error("<mat> must not be singular");
		fi;
		return Projectivity(mat,gf);
	end );

# Added ml cmat version 19/3/14.
#############################################################################
#O  Projectivity( <pg>, <mat> )
# method to construct an object in the category IsProjGrpEl, i.e. a projectivity, 
# This method is intended for the user, and contains
# a check whether the matrix is non-singular.
## 
InstallMethod( Projectivity, [ IsProjectiveSpace, IsCMatRep],
#
  	function( pg, mat )
		local d,gf,m2;
		d:=Dimension(pg);
		gf:=pg!.basefield;
		if d <> NrRows(mat)-1 then
			Error("The arguments <mat> and <pg> are incompatible");
		fi;
		m2 := ShallowCopy(mat);
		#if NrCols(m2) <> NrRows(m2) then
		#	Error("<mat> must be a square matrix");
		#fi;
		if Rank(m2) <> NrRows(m2) then
			Error("<mat> must not be singular");
		fi;
		return Projectivity(mat,gf);
	end );


###################################################################
# Tests whether collineation is a projectivity and so on ...
###################################################################

# Added ml 7/11/2012
#############################################################################
#O  IsProjectivity( <g> )
## 
InstallMethod( IsProjectivity, [ IsProjGrpEl ],
  function( g )
	return true;
  end );

# Added ml 7/11/2012
#############################################################################
#O  IsProjectivity( <g> )
# method to check if a given collineation of a projective space is a projectivity, 
# i.e. if the corresponding frobenius automorphism is the identity
## 
InstallMethod( IsProjectivity, [ IsProjGrpElWithFrob ],
  function( g )
    local F,sigma;
		F:=g!.fld;
		sigma:=g!.frob;
		if sigma = FrobeniusAutomorphism(F)^0
		then return true;
		else return false;
		fi;
  end );

# Added ml 8/11/2012 # changed ml 28/11/2012
#############################################################################
#O  IsStrictlySemilinear( <g> )
## 
InstallMethod( IsStrictlySemilinear, [ IsProjGrpEl],
  function( g )
	return false;
  end );

# Added ml 8/11/2012 # changed ml 28/11/2012
#############################################################################
#O  IsStrictlySemilinear( <g> )
# method to check if a given collineation of a projective space is semilinear, 
# i.e. if the corresponding frobenius automorphism is NOT the identity
## 
InstallMethod( IsStrictlySemilinear, [ IsProjGrpElWithFrob],
  function( g )
    local F,sigma;
		F:=g!.fld;
		sigma:=g!.frob;
		if sigma = FrobeniusAutomorphism(F)^0
		then return false;
		else return true;
		fi;
  end );

  
  
# Added ml 8/11/2012
#############################################################################
#O  IsCollineation( <g> )
## 
InstallMethod( IsCollineation, [ IsProjGrpEl],
  function( g )
	return true;
  end );

# Added ml 8/11/2012
#############################################################################
#O  IsCollineation( <g> )
## 
InstallMethod( IsCollineation, [ IsProjGrpElWithFrob],
  function( g )
    return true;
end );


# Added ml 7/11/2012
#############################################################################
#O  IsProjectivityGroup( <G> )
# method to check if a given projective collineation group G is a projectivity group, 
# i.e. if the corresponding frobenius automorphisms of the generators are the identity
## 
InstallMethod( IsProjectivityGroup, [ IsProjectiveGroupWithFrob],
  function( G )
    local gens, F, set, g;
		gens:=GeneratorsOfMagmaWithInverses(G);
		F:=gens[1]!.fld;
		set:=AsSet(List(gens,g->g!.frob));
		if set = AsSet([FrobeniusAutomorphism(F)^0])
		then return true;
		else return false;
		fi;
  end );


# Added ml 8/11/2012
#############################################################################
#O  IsCollineationGroup( <G> )
##
InstallMethod( IsCollineationGroup, [ IsProjectiveGroupWithFrob],
  function( G )
	return true;
  end );

###################################################################
# Construction of "projective collineation maps", that is matrices 
# modulo scalars with frobenius automorphism
###################################################################

# CHECKED 5/09/11 jdb
# cmat changed 19/3/14
#############################################################################
#O  ProjElWithFrob( <mat>, <frob>, <f> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. projective 
# semilinear maps aka "matrices modulo scalars with frob". This method is not 
# intended for the users, it has no checks built in.
##
InstallMethod( ProjElWithFrob, 
	"for a cmat/ffe matrix and a Frobenius automorphism, and a field",
	[IsCMatRep and IsFFECollColl, #changed 19/3/14 to cmat.
	IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	IsField],
	function( m, frob, f )
		local el, m2;
		m2 := ShallowCopy(m);
		#ConvertToMatrixRep( m2, f );
		el := rec( mat := m2, fld := f, frob := frob );
		Objectify( ProjElsWithFrobType, el );
		return el;
	end );
	
# added 19/03/14
#############################################################################
#O  ProjElWithFrob( <mat>, <frob>, <f> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. projective 
# semilinear maps aka "matrices modulo scalars with frob". This method is not 
# intended for the users, it has no checks built in.
##
InstallMethod( ProjElWithFrob, 
	"for a ffe matrix and a Frobenius automorphism, and a field",
	[IsMatrix and IsFFECollColl, 
	IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	IsField],
	function( m, frob, f )
		local el, m2, cmat;
		m2 := ShallowCopy(m);
		#ConvertToMatrixRep( m2, f );
		cmat := NewMatrix(IsCMatRep,f,NrCols(m2),m2);
		el := rec( mat := cmat, fld := f, frob := frob );
		Objectify( ProjElsWithFrobType, el );
		return el;
	end );
	

# CHECKED 5/09/11 jdb
# cmat changed 19/3/14
#############################################################################
#O  ProjElWithFrob( <mat>, <frob> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. projective 
# semilinear maps aka "matrices modulo scalars with frob". This method is not 
# intended for the users, although there are some checks built in. 
# This method relies on DefaultFieldOfMatrix and Range(<frbo>) to determine the field to be used
##
InstallMethod( ProjElWithFrob, 
	"for a cmat/ffe matrix and a Frobenius automorphism",
	[IsCMatRep and IsFFECollColl, #changed 19/3/14. 
	IsRingHomomorphism and IsMultiplicativeElementWithInverse], 
	1, #to set higher priority than the next method.
	function ( m, frob )
		local matrixfield, frobfield, mchar, fchar, dim;
		if IsOne(frob) then
		  TryNextMethod();
		fi;
		matrixfield := DefaultFieldOfMatrix(m);
		mchar := Characteristic(matrixfield);
		frobfield := Range(frob);
		fchar := Characteristic(frobfield);
		if mchar <> fchar then
		  Error("the matrix and automorphism do not agree on the characteristic");
		fi;
		
		# figure out a field which contains the matrices and admits the
		# automorphisms (nontrivially)
		dim := Lcm(
		  LogInt(Size(matrixfield),mchar),
			LogInt(Size(frobfield),fchar)
			);
	return ProjElWithFrob( m, frob, GF(mchar^dim) );
	end);
 
# CHECKED 5/09/11 jdb
# cmat changed 19/3/14
#############################################################################
#O  ProjElWithFrob( <mat>, <frob> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. projective 
# semilinear maps aka "matrices modulo scalars with frob". This method is not 
# intended for the users; in fact this method is almost the same as the first version.
# This method relies on DefaultFieldOfMatrix to determine the field to be used
# This method should only be called from the above one, if IsOne(frob) is true,
# but we built in a little check to abvoid disasters.
##
InstallMethod( ProjElWithFrob, 
	"for a cmat/ffe matrix and a trivial Frobenius automorphism",
	[IsCMatRep and IsFFECollColl,
	IsRingHomomorphism and IsMultiplicativeElementWithInverse], 
	0, #to set lower priority than the previous method.
	function( m, frob )
		local el, m2;
		if not IsOne(frob) then
		  Error("<frob> is not trivial. Something went wrong when calling this method");
		fi;
		m2 := ShallowCopy(m); 
		#ConvertToMatrixRepNC( m2 );
		el := rec( mat := m2, fld := DefaultFieldOfMatrix(m), frob := frob );
		Objectify( ProjElsWithFrobType, el );
		return el;
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  ProjElsWithFrob( <l>, <f> )
# method to construct a list of objects in the category IsProjGrpElWithFrob,
# using a list of pairs of matrix/frobenius automorphism, and a field.
# This method relies of ProjElWithFrob, and is not inteded for the user.
# no checks are built in. This could result in e.g. the use of a field that is 
# not compatible with (some of) the matrices, and result in a non user friendly 
# error
##
InstallMethod( ProjElsWithFrob,
	"for a list of pairs of ffe matrices and frobenius automorphisms, and a field",
	[IsList, IsField],
	function( l, f )
	local objectlist, m;
		objectlist := [];
		for m in l do
			Add(objectlist, ProjElWithFrob(m[1],m[2],f));
		od;
		return objectlist;
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  ProjElsWithFrob( <l> )
# method to construct a list of objects in the category IsProjGrpElWithFrob,
# using a list of pairs of matrix/frobenius automorphism. It is checked if 
# the given matrices/automorphism pairs can be considered over a common field.
# This method relies eventually also on ProjElWithFrob, and is not inteded for the user,
# although the mechanism of fining the suitable field will display an error 
# if this is not possible.
##
InstallMethod( ProjElsWithFrob, 
	"for a list of pairs of cmat/ffe matrices and Frobenius automorphisms",
	[IsList],
	function( l )
    local matrixfield, frobfield, mchar, fchar, oldchar, f, dim, m, objectlist;
    if(IsEmpty(l)) then
		return [];
		fi;
		dim := 1;

		# get the characteristic of the field that we want to work in.
		# it should be the same for every matrix and every automorphism --
		# if not we will raise an error.
		oldchar := Characteristic(l[1][1]);
		for m in l do
  		matrixfield := DefaultFieldOfMatrix(m[1]);
  		mchar := Characteristic(matrixfield);
  		frobfield := Range(m[2]);
  		fchar := Characteristic(frobfield);
  		if mchar <> fchar or mchar <> oldchar then
  		  Error("matrices and automorphisms do not agree on the characteristic");
  		fi;
		
  		# at each step we increase the dimension of the desired field
		# so that it contains all the matrices and admits all the automorphisms
		# (nontrivially.)
  
  		dim := Lcm( dim, LogInt(Size(matrixfield),mchar),
  			LogInt(Size(frobfield),fchar));
		od;

		f := GF(oldchar ^ dim);
		objectlist := [];
		for m in l do
			Add(objectlist, ProjElWithFrob(m[1],m[2],f));
		od;
		return objectlist;
	end );


# CHECKED 5/09/11 jdb # changed ml 8/11/12
#############################################################################
#O  CollineationOfProjectiveSpace( <mat>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. a 
# collineation of a projective space. 
# This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrob,
# which will produce an error if the entries of <mat> do not all belong to <gf>
# the automorphism will be trivial. Mathematically this is a projectivity.
## 
InstallMethod( CollineationOfProjectiveSpace, 
	[ IsMatrix and IsFFECollColl, IsField],
	function( mat, gf )
    if Rank(mat) <> NrRows(mat) then
		Error("<mat> must not be singular");
    fi;
    return ProjElWithFrob( mat, IdentityMapping(gf), gf);
	end );

# Added ml 8/11/2012
#############################################################################
#O  CollineationOfProjectiveSpace( <pg>, <mat> )
# method to construct an collineation of a projective space.
## 
InstallMethod( CollineationOfProjectiveSpace, [ IsProjectiveSpace, IsMatrix],
#
  	function( pg, mat )
		local d,gf;
		d:=Dimension(pg);
		gf:=pg!.basefield;
		if d <> NrRows(mat)-1 then
			Error("The arguments <mat> and <pg> are incompatible");
		fi;
		return ProjElWithFrob( mat, IdentityMapping(gf), gf);
	end );

# Added jdb 26/05/2016
# not documented yet
#############################################################################
#O  CollineationOfProjectiveSpace( <pg>, <mat> )
# method to construct an collineation of a projective space with identitymatrix,
# but with user defined field automorphism.
## 
InstallMethod( CollineationOfProjectiveSpace, [ IsProjectiveSpace, IsMapping],
  	function( pg, frob )
		local d,gf,mat;
		d:=Dimension(pg);
		gf:=Range(frob);
        if not gf = pg!.basefield then
            Error("basefield of <pg> does not match with range of <frob>");
        fi;
        mat := IdentityMat(d+1,gf);
		return ProjElWithFrob( mat, frob, gf);
	end );


# Added ml 8/11/2012
#############################################################################
#O  CollineationOfProjectiveSpace( <pg>, <mat>, <frob>)
# method to construct an collineation of a projective space.
## 
InstallMethod( CollineationOfProjectiveSpace, [ IsProjectiveSpace, IsMatrix, IsMapping],
#
  	function( pg, mat, frob )
		local d,gf;
		d:=Dimension(pg);
		gf:=pg!.basefield;
		if d <> NrRows(mat)-1 then
			Error("The arguments <mat> and <pg> are incompatible");
		fi;
		return ProjElWithFrob( mat, frob, gf);
	end );


# Added ml 8/11/2012
#############################################################################
#O  Collineation( <pg>, <mat> )
# shorter version of the previous method to construct an collineation of a projective space.
## 
InstallMethod( Collineation, [ IsProjectiveSpace, IsMatrix],
#
  	function( pg, mat )
		return CollineationOfProjectiveSpace(pg,mat);
	end );


# Added ml 8/11/2012
#############################################################################
#O  Collineation( <pg>, <mat>, <frob> )
# shorter version of the previous method to construct an collineation of a projective space.
## 
InstallMethod( Collineation, [ IsProjectiveSpace, IsMatrix, IsMapping],
#
  	function( pg, mat, frob )
		return CollineationOfProjectiveSpace(pg,mat,frob);
	end );


# CHECKED 5/09/11 jdb # changed ml 8/11/12
# 5/09/11 jdb: added a check to see if frob has <gf> as source
#############################################################################
#O  CollineationOfProjectiveSpace( <mat>, <frob>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. a 
# collineation of a projective space, aka a projective collineation map. 
# This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrob,
# which will produce an error if the entries of <mat> do not all belong to <gf>
## 
InstallMethod( CollineationOfProjectiveSpace,  
	[ IsMatrix and IsFFECollColl, IsRingHomomorphism and
    IsMultiplicativeElementWithInverse, IsField], 
	function( mat, frob, gf )
    if Rank(mat) <> NrRows(mat) then
		Error("<mat> must not be singular");
    fi;
    if Source(frob) <> gf then
		Error("<frob> must be defined as an automorphis of <gf>");
	fi;
	return ProjElWithFrob( mat, frob, gf);
	end );


# CHECKED 5/09/11 jdb # changed ml 8/11/12
# 5/09/11 jdb: added a check to see if frob has <gf> as source
#############################################################################
#O  ProjectiveSemilinearMap( <mat>, <frob>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrob, i.e. a 
# collineation of a projective space, aka a projective collineation map. 
# This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrob,
# which will produce an error if the entries of <mat> do not all belong to <gf>
## 
InstallMethod( ProjectiveSemilinearMap,  
	[ IsMatrix and IsFFECollColl, IsRingHomomorphism and
    IsMultiplicativeElementWithInverse, IsField], 
	function( mat, frob, gf )
		return CollineationOfProjectiveSpace(mat,frob,gf);
	end );


# CHECKED 5/09/11 jdb
#############################################################################
#O  ProjectivityByImageOfStandardFrameNC( <pg>, <image> )
# method to construct a projectivity if the image of a standard frame is given.
# THERE IS NO CHECK TO SEE IF THE GIVEN IMAGE CONSISTS OF N+2 POINTS NO N+1 L.D.
# for this reason, this function is not documented (yet).
# despite its name (projectivity), this function returns a projective semlinear map.
## 
InstallMethod( ProjectivityByImageOfStandardFrameNC, [ IsProjectiveSpace, IsList ],
	function(pg,image)
	# If the dimension of the projective space is n, then
	# given a frame, there is a 
	# unique projectivity mapping the standard frame to this set of points
	# THERE IS NO CHECK TO SEE IF THE GIVEN IMAGE CONSISTS OF N+2 POINTS NO N+1 L.D.
	local d,i,x,vlist,mat,coeffs,mat2;
	if not Length(image)=Dimension(pg)+2 then 
	Error("The argument does not have the required length to be the image of a frame");
	fi;
	d:=Dimension(pg);
	vlist:=List(image,x->x!.obj);
	mat:=List([1..d+1],i->vlist[i]);
	coeffs:=vlist[d+2]*(mat^-1);
	mat2:=List([1..d+1],i->coeffs[i]*mat[i]);
	return CollineationOfProjectiveSpace(mat2,pg!.basefield);
end );

###################################################################
# Some operations for elements (without and with frobenius automorphism
###################################################################

# CHECKED 5/09/11 jdb
#############################################################################
#O  MatrixOfCollineation( <c> )
# returns the underlying matrix of <c> 
##
InstallMethod( MatrixOfCollineation, [ IsProjGrpEl and IsProjGrpElRep],
	c -> c!.mat );

# CHECKED 5/09/11 jdb
#############################################################################
#O  MatrixOfCollineation( <c> )
# returns the underlying matrix of <c> 
##  
InstallMethod( MatrixOfCollineation, [ IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	c -> c!.mat );
  
# CHECKED 5/09/11 jdb
#############################################################################
#O  FieldAutomorphism( <c> )
# returns the underlying field automorphism of <c> 
##  
InstallMethod( FieldAutomorphism, [ IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	c -> c!.frob );

# CHECKED 5/09/11 jdb
#############################################################################
#O  Representative( <el> )
# returns the underlying matrix of the projectivity <el> 
##  
InstallOtherMethod( Representative, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		return el!.mat;
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  BaseField( <el> )
# returns the underlying field of <el> 
##  
InstallMethod( BaseField, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		return el!.fld;
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  Representative( <el> )
# returns the underlying matrix and frobenius automorphism of the projective 
# semilinear element <el> 
## 
InstallOtherMethod( Representative, 
	"for a projective group element with Frobenius",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	function( el )
		return [el!.mat,el!.frob];
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  BaseField( <el> )
# returns the underlying field of <el> 
##  
InstallMethod( BaseField, 
	"for a projective group element with Frobenius",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	function( el )
		return el!.fld;
	end );

# CHECKED 5/09/11 jdb
###################################################################
# View, print and display methods for elements (without and with Frobenius)
###################################################################

InstallMethod( ViewObj, "for a projective group element",
  [IsProjGrpEl and IsProjGrpElRep],
  function(el)
    Print("<projective element ");
    ViewObj(el!.mat);
    Print(">");
  end);

InstallMethod( Display, "for a projective group element",
  [IsProjGrpEl and IsProjGrpElRep],
  function(el)
    Print("<projective element, underlying matrix:\n");
    Display(el!.mat);
    Print(">\n");
  end );

InstallMethod( PrintObj, "for a projective group element",
  [IsProjGrpEl and IsProjGrpElRep],
  function(el)
    Print("ProjEl(");
    PrintObj(el!.mat);
    Print(")");
  end );
  
InstallMethod( ViewObj, "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function(el)
    Print("< a collineation: ");
    ViewObj(el!.mat);
    if IsOne(el!.frob) then
        Print(", F^0>");
    else
        Print(", F^",el!.frob!.power,">");
    fi;
  end);

InstallMethod( Display, "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function(el)
    Print("<a collineation , underlying matrix:\n");
    Display(el!.mat);
    if IsOne(el!.frob) then
        Print(", F^0>\n");
    else
        Print(", F^",el!.frob!.power,">\n");
    fi;
  end );

InstallMethod( PrintObj, "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function(el)
    Print("ProjElWithFrob(");
    PrintObj(el!.mat);
    Print(",");
    PrintObj(el!.frob);
    Print(")");
  end );

# CHECKED 5/09/11 jdb
###################################################################
# comparing elements (without and with Frobenius automorphism)
###################################################################

InstallMethod( \=, "for two projective group elements",
  [IsProjGrpEl and IsProjGrpElRep, IsProjGrpEl and IsProjGrpElRep],
  function( a, b )
    local aa,bb,p,s,i;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    aa := a!.mat;
    bb := b!.mat;
    p := PositionNonZero(aa[1]);
    s := bb[1,p] / aa[1,p];
    for i in [1..Length(aa)] do
        if s*aa[i] <> bb[i] then return false; fi;
    od;
    return true;
  end );

InstallMethod(\<,
  [IsProjGrpEl, IsProjGrpEl],
  function(a,b)
    local aa,bb,pa,pb,sa,sb,i,va,vb;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    aa := a!.mat;
    bb := b!.mat;
    pa := PositionNonZero(aa[1]);
    pb := PositionNonZero(bb[1]);
    if pa > pb then 
        return true;
    elif pa < pb then
        return false;
    fi;
    sa := aa[1,pa]^-1;
    sb := bb[1,pb]^-1;
    for i in [1..Length(aa)] do
        va := sa*aa[i];
        vb := sb*bb[i];
        if va < vb then return true; fi;
        if vb < va then return false; fi;
    od;
    return false;
  end);

InstallMethod( \=, "for two projective group elements with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep,
   IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( a, b )
    local aa,bb,p,s,i;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    if a!.frob <> b!.frob then return false; fi;
    aa := a!.mat;
    bb := b!.mat;
    p := PositionNonZero(aa[1]);
    s := bb[1,p] / aa[1,p];
    if s*aa <> bb then return false; fi;
	#for i in [1..Length(aa)] do
        #if s*aa[i] <> bb[i] then return false; fi;
    #od;
    return true;
  end );

InstallMethod(\<,
  [IsProjGrpElWithFrob, IsProjGrpElWithFrob],
  function(a,b)
    local aa,bb,pa,pb,sa,sb,i,va,vb;
    if a!.fld <> b!.fld then Error("different base fields"); fi;
    if a!.frob < b!.frob then
        return true;
    elif b!.frob < a!.frob then
        return false;
    fi;
    aa := a!.mat;
    bb := b!.mat;
    pa := PositionNonZero(aa[1]);
    pb := PositionNonZero(bb[1]);
    if pa > pb then 
        return true;
    elif pa < pb then
        return false;
    fi;
    sa := aa[1,pa]^-1;
    sb := bb[1,pb]^-1;
    for i in [1..Length(aa)] do
        va := sa*aa[i];
        vb := sb*bb[i];
        if va < vb then return true; fi;
        if vb < va then return false; fi;
    od;
    return false; 
  end);

###################################################################
# More operations for elements (without and with frobenius automorphism)
###################################################################

# CHECKED 5/09/11 jdb
#############################################################################
#O  Order( <a> )
# returns the order of <a>. This function relies on ProjectiveOrder.
##  
InstallMethod( Order, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( a )
		return ProjectiveOrder(a!.mat)[1];
	end );

# CHECKED 5/09/11 jdb
# 22/04/12 jb: There was a bug here. I added "and i mod ofrob = 0;"
# 3/6/2015: bug fixed by jb.
#############################################################################
#O  Order( <a> )
# returns the order of <a>.
## 
InstallMethod( Order, 
	"for a projective group element with Frobenius",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],

# This algorithm could be improved by using the ideas of
# Celler and Leedham-Green.

	function( a )
   local b, frob, bfrob, i, ofrob;
	b := a!.mat;
	frob := a!.frob;
	if IsOne(frob) then 
		return ProjectiveOrder(b)[1];
	fi;
	if not IsOne(b) then
		bfrob := b; i := 1;
		ofrob := Order(frob);
		repeat
			bfrob := bfrob^(frob^-1);  ## JB 3/6/2015: Found bug here
			b := b * bfrob;
			i := i + 1;
		until IsFiningScalarMatrix( b ) and i mod ofrob = 0;
		return i;
	else
		return Order(frob);
	fi;
	return 1;
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  IsOne( <a> )
# returns true if <a> is the identity.
## 
InstallMethod( IsOne, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
    local s;
    s := el!.mat[1,1];
    if IsZero(s) then 
		return false; 
	fi;
    s := s^-1;
    return IsOne( s*el!.mat );
	end );

# CHECKED 5/09/11 jdb
#############################################################################
#O  IsOne( <a> )
# returns true if <a> is the identity
## 
InstallMethod( IsOne, 
	"for a projective group element with Frobenius",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	function( el )
    local s;
    if not(IsOne(el!.frob)) then 
		return false; 
	fi;
    s := el!.mat[1,1];
    if IsZero(s) then return false; fi;
    s := s^-1;
    return IsOne( s*el!.mat );
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  DegreeFFE( <el> )
# for projectivities. returns the degree of the underlying field over its 
# prime field.
## 
InstallOtherMethod( DegreeFFE, 
	"for projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		return DegreeOverPrimeField( el!.fld );
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  DegreeFFE( <el> )
# for projective collineation maps. returns the degree of the underlying 
# field over its prime field.
## 
InstallOtherMethod( DegreeFFE, 
	"for projective group element with Frobenius",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	function( el )
		return DegreeOverPrimeField( el!.fld );
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  Characteristic( <el> )
# for projectivities. returns the characteristic of the underlying field.
## 
InstallMethod( Characteristic, 
	"for projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		return Characteristic( el!.fld );
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  Characteristic( <el> )
# for projective collineation maps. returns the characteristic of the underlying field.
## 
InstallMethod( Characteristic, 
	"for projective group element with Frobenius",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	function( el )
		return Characteristic( el!.fld );
	end );

###################################################################
# The things that make it a group :-) (without Frobenius)
###################################################################

# CHECKED 5/09/11 jdb
#############################################################################
#O  \*( <a>, <b> )
# returns a*b, for IsProjGrpEl
## 
InstallMethod( \*, 
	"for two projective group elements",
	[IsProjGrpEl and IsProjGrpElRep, IsProjGrpEl and IsProjGrpElRep],
	function( a, b )
		local el;
		el := rec( mat := a!.mat * b!.mat, fld := a!.fld );
		Objectify( ProjElsType, el );
		return el;
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  InverseSameMutability( <el> )
# returns el^-1, for IsProjGrpEl, keeps mutability.
## 
InstallMethod( InverseSameMutability, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		local m;
		m := rec( mat := InverseSameMutability(el!.mat), fld := el!.fld );
		Objectify( ProjElsType, m );
		return m;
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  InverseMutable( <el> )
# returns el^-1 (mutable) for IsProjGrpEl
## 
InstallMethod( InverseMutable, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		local m;
		m := rec( mat := InverseMutable(el!.mat), fld := el!.fld );
		Objectify( ProjElsType, m );
		return m;
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  OneImmutable( <el> )
# returns immutable one of the group of <el>
## 
InstallMethod( OneImmutable, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		local o;
		o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld );
		Objectify( NewType(FamilyObj(el), IsProjGrpElRep), o );
		return o;
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  OneSameMutability( <el> )
# returns one of the group of <el> with same mutability of <el>.
## 
InstallMethod( OneSameMutability, 
	"for a projective group element",
	[IsProjGrpEl and IsProjGrpElRep],
	function( el )
		local o;
		o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld );
		Objectify( NewType(FamilyObj(el), IsProjGrpElRep), o );
		return o;
	end );

###################################################################
# The things that make it a group :-) (with Frobenius)
###################################################################
# we first need: 
#################################################
# Frobenius automorphisms and groups using them:
#################################################
#12 CHECKED 6/09/11 jdb

InstallOtherMethod( \^, "for a FFE vector and a Frobenius automorphism",
  [ IsVector and IsFFECollection and IsMutable, IsFrobeniusAutomorphism ],
  function( v, f )
    return List(v,x->x^f);
  end );

#cvec version
InstallOtherMethod( \^, "for a cvec/FFE vector and a Frobenius automorphism",
  [ IsCVecRep and IsFFECollection and IsMutable, IsFrobeniusAutomorphism ],
  function( v, f )
    return CVec(List(v,x->x^f),BaseField(v));
  end );

InstallOtherMethod( \^, "for a FFE vector and a Frobenius automorphism",
  [ IsVector and IsFFECollection, IsFrobeniusAutomorphism ],
  function( v, f )
    return MakeImmutable(List(v,x->x^f));
  end );

#cvec version
InstallOtherMethod( \^, "for a cvec/FFE vector and a Frobenius automorphism",
  [ IsCVecRep and IsFFECollection, IsFrobeniusAutomorphism ],
  function( v, f )
    return MakeImmutable(CVec(List(v,x->x^f),BaseField(v)));
  end );

#we think that this method is not needed, worse, should not be used.
#InstallOtherMethod( \^, 
#  "for a mutable FFE vector and a trivial Frobenius automorphism",
#  [ IsVector and IsFFECollection and IsMutable, IsMapping and IsOne ],
#  function( v, f )
#    return v;
#  end );

#cvec version
#InstallOtherMethod( \^, 
#  "for a mutable cvec/FFE vector and a trivial Frobenius automorphism",
#  [ IsCVecRep and IsFFECollection and IsMutable, IsMapping and IsOne ],
#  function( v, f )
#    return v;
#  end );
  
InstallOtherMethod( \^, 
  "for a mutable FFE vector and a trivial Frobenius automorphism",
  [ IsVector and IsFFECollection and IsMutable, IsMapping and IsOne ],
  function( v, f )
    return ShallowCopy(v);
  end );
  
#cvec version
InstallOtherMethod( \^, 
  "for a mutable cvec/FFE vector and a trivial Frobenius automorphism",
  [ IsCVecRep and IsFFECollection and IsMutable, IsMapping and IsOne ],
  function( v, f )
    return ShallowCopy(v);
  end );

#the next operations until the matrix section, the methods will become obsolete as soon as fining is cvec'ed.
  
InstallOtherMethod( \^, 
  "for a compressed GF2 vector and a Frobenius automorphism",
  [ IsVector and IsFFECollection and IsGF2VectorRep, IsFrobeniusAutomorphism ],
  function( v, f )
    local w;
    w := List(v,x->x^f);
    ConvertToVectorRepNC(w,2);
    return MakeImmutable(w);
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed GF2 vector and a Frobenius automorphism",
  [ IsVector and IsFFECollection and IsGF2VectorRep and IsMutable, 
    IsFrobeniusAutomorphism ],
  function( v, f )
    local w;
    w := List(v,x->x^f);
    ConvertToVectorRepNC(w,2);
    return w;
  end );

InstallOtherMethod( \^, 
  "for a compressed GF2 vector and a trivial Frobenius automorphism",
  [ IsVector and IsFFECollection and IsGF2VectorRep, IsMapping and IsOne ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed GF2 vector and a trivial Frobenius automorphism",
  [ IsVector and IsFFECollection and IsGF2VectorRep and IsMutable, 
    IsMapping and IsOne ],
  function( v, f )
    return ShallowCopy(v);
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit vector and a Frobenius automorphism",
  [ IsVector and IsFFECollection and Is8BitVectorRep, IsFrobeniusAutomorphism ],
  function( v, f )
    local w;
    w := List(v,x->x^f);
    ConvertToVectorRepNC(w,Q_VEC8BIT(v));
    return MakeImmutable(w);
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed 8bit vector and a Frobenius automorphism",
  [ IsVector and IsFFECollection and Is8BitVectorRep and IsMutable, 
    IsFrobeniusAutomorphism ],
  function( v, f )
    local w;
    w := List(v,x->x^f);
    ConvertToVectorRepNC(w,Q_VEC8BIT(v));
    return w;
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit vector and a trivial Frobenius automorphism",
  [ IsVector and IsFFECollection and Is8BitVectorRep, IsMapping and IsOne ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed 8bit vector and a trivial Frobenius automorphism",
  [ IsVector and IsFFECollection and Is8BitVectorRep and IsMutable, 
    IsMapping and IsOne ],
  function( v, f )
    return ShallowCopy(v);
  end );

#### matrix methods

InstallOtherMethod( \^, "for a FFE matrix and a Frobenius automorphism",
  [ IsMatrix and IsFFECollColl, IsFrobeniusAutomorphism ],
  function( m, f )
    return MakeImmutable(List(m,v->List(v,x->x^f)));
  end );

#cmat
InstallOtherMethod( \^, "for a FFE matrix and a Frobenius automorphism",
  [ IsCMatRep and IsFFECollColl, IsFrobeniusAutomorphism ],
  function( m, f )
    return MakeImmutable(CMat(List(m,v->v^f)));
  end );  
  
#InstallOtherMethod( \^, "for a FFE matrix and a Frobenius automorphism",
#  [ IsMatrix and IsFFECollColl, IsFrobeniusAutomorphism ],
#  function( m, f )
#    return MakeImmutable(List(m,v->List(v,x->x^f)));
#  end );
  
InstallOtherMethod( \^, "for a mutable FFE matrix and a Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and IsMutable, IsFrobeniusAutomorphism ],
  function( m, f )
    return List(m,v->List(v,x->x^f));
  end );

#cmat
InstallOtherMethod( \^, "for a mutable FFE matrix and a Frobenius automorphism",
  [ IsCMatRep and IsFFECollColl and IsMutable, IsFrobeniusAutomorphism ],
  function( m, f )
    return CMat(List(m,v->v^f));
  end );

InstallOtherMethod( \^, "for a FFE matrix and a trivial Frobenius automorphism",
  [ IsMatrix and IsFFECollColl, IsMapping and IsOne ],
  function( m, f )
    return m;
  end );

#cmat
InstallOtherMethod( \^, "for a FFE matrix and a trivial Frobenius automorphism",
  [ IsCMatRep and IsFFECollColl and IsMutable, IsMapping and IsOne ],
  function( m, f )
    return ShallowCopy(m);
  end );

InstallOtherMethod( \^, 
  "for a mutable FFE matrix and a trivial Frobenius automorphism",
  [ IsMatrix and IsFFECollColl, IsMapping and IsOne ],
  function( m, f )
    return MutableCopyMat(m);
  end );

#cmat  
InstallOtherMethod( \^, "for a FFE matrix and a trivial Frobenius automorphism",
  [ IsCMatRep and IsFFECollColl , IsMapping and IsOne ],
  function( m, f )
    return MakeImmutable(ShallowCopy(m));
  end );

#the next matrix methods will become obsolete.

InstallOtherMethod( \^, 
  "for a compressed GF2 matrix and a Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and IsGF2MatrixRep, IsFrobeniusAutomorphism ],
  function( m, f )
    local w,l,i;
    l := [];
    for i in [1..NrRows(m)] do
        w := List(m[i],x->x^f);
        ConvertToVectorRepNC(w,2);
        Add(l,w);
    od;
    ConvertToMatrixRepNC(l,2);
    return MakeImmutable(l);
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed GF2 matrix and a Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and IsGF2MatrixRep and IsMutable, 
    IsFrobeniusAutomorphism ],
  function( m, f )
    local w,l,i;
    l := [];
    for i in [1..NrRows(m)] do
        w := List(m[i],x->x^f);
        ConvertToVectorRepNC(w,2);
        Add(l,w);
    od;
    ConvertToMatrixRepNC(l,2);
    return l;
  end );

InstallOtherMethod( \^, 
  "for a compressed GF2 matrix and a trivial Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and IsGF2MatrixRep, IsMapping and IsOne ],
  function( m, f )
    return m;
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed GF2 matrix and a trivial Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and IsGF2MatrixRep and IsMutable, 
    IsMapping and IsOne ],
  function( m, f )
    return MutableCopyMat(m);
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit matrix and a Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and Is8BitMatrixRep, IsFrobeniusAutomorphism ],
  function( m, f )
    local w,l,i,q;
    l := [];
    q := Q_VEC8BIT(m[1]);
    for i in [1..NrRows(m)] do
        w := List(m[i],x->x^f);
        ConvertToVectorRepNC(w,q);
        Add(l,w);
    od;
    ConvertToMatrixRepNC(l,q);
    return MakeImmutable(l);
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed 8bit matrix and a Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and Is8BitMatrixRep and IsMutable, 
    IsFrobeniusAutomorphism ],
  function( m, f )
    local w,l,i,q;
    l := [];
    q := Q_VEC8BIT(m[1]);
    for i in [1..NrRows(m)] do
        w := List(m[i],x->x^f);
        ConvertToVectorRepNC(w,q);
        Add(l,w);
    od;
    ConvertToMatrixRepNC(l,q);
    return MakeImmutable(l);
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit matrix and a trivial Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and Is8BitMatrixRep, IsMapping and IsOne ],
  function( m, f )
    return m;
  end );

InstallOtherMethod( \^, 
  "for a mutable compressed 8bit matrix and a trivial Frobenius automorphism",
  [ IsMatrix and IsFFECollColl and Is8BitMatrixRep and IsMutable, 
    IsMapping and IsOne ],
  function( m, f )
    return MutableCopyMat(m);
  end );


# CHECKED 6/09/11 jdb
#############################################################################
#O  \*( <a>, <b> )
# returns a*b, for IsProjGrpElWithFrob
## 
#made a change, added ^-1 on march 8 2007, J&J
InstallMethod( \*, "for two projective group element with Frobenious",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep,
   IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( a, b )
    local el;
    el := rec( mat := a!.mat * (b!.mat^(a!.frob^-1)), fld := a!.fld, 
               frob := a!.frob * b!.frob );
    Objectify( ProjElsWithFrobType, el);
    return el;
  end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  InverseSameMutability( <el> )
# returns el^-1, for IsProjGrpElWithFrob, keeps mutability.
## 
#found a bug 23/09/08 in st. andrews.
#J&J feel a great relief.
#C&P too.
#all after Max concluded that there was a big bug.
InstallMethod( InverseSameMutability, 
  "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( el )
    local m,f;
    f := el!.frob;
    m := rec( mat := (InverseSameMutability(el!.mat))^f, fld := el!.fld,
              frob := f^-1 );
    Objectify( ProjElsWithFrobType, m );
    return m;
  end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  InverseMutable( <el> )
# returns mutable el^-1, for IsProjGrpElWithFrob
## 
InstallMethod( InverseMutable, 
  "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( el )
    local m,f;
    f := el!.frob;
    m := rec( mat := (InverseMutable(el!.mat))^f, fld := el!.fld,
              frob := f^-1 );
    Objectify( ProjElsWithFrobType, m );
    return m;
  end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  OneImmutable( <el> )
# returns immutable one of the group of <el>
## 
InstallMethod( OneImmutable, "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( el )
    local o;
    o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
              frob := el!.frob^0 );
    Objectify( ProjElsWithFrobType, o);
    return o;
  end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  OneSameMutability( <el> )
# returns one of the group of <el> with same mutability
## 
InstallMethod( OneSameMutability, 
  "for a projective group element with Frobenius",
  [IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
  function( el )
    local o;
    o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
              frob := el!.frob^0 );
    Objectify( ProjElsWithFrobType, o);
    return o;
  end );

###################################################################
# General methods to deal with projective groups.
# Construction of (projective) groups is done in the appropriate files
# that deal with geometries.
###################################################################

# 10 CHECKED 6/09/11 jdb
###################################################################
# View, print and display methods for projective groups 
# (without and with Frobenius)
###################################################################


#InstallMethod( ViewObj, 
#	"for a projective group",
#	[IsProjectivityGroup],
#	function( g )
#		Print("<projective group>");
#	end );
#
#InstallMethod( ViewObj, 
#	"for a trivial projective group",
#	[IsProjectivityGroup and IsTrivial],
#	function( g )
#		Print("<trivial projective group>");
#	end );
#
#InstallMethod( ViewObj, 
#	"for a projective group with gens",
#	[IsProjectivityGroup and HasGeneratorsOfGroup],
#	function( g )
#		local gens;
#		gens := GeneratorsOfGroup(g);
#		if Length(gens) = 0 then
#			Print("<trivial projective group>");
#		else
#			Print("<projective group with ",Length(gens),
 #             " generators>");
#		fi;
#	end );
#
#InstallMethod( ViewObj, 
#	"for a projective group with size",
#	[IsProjectivityGroup and HasSize],
#	function( g )
#		if Size(g) = 1 then
#			Print("<trivial projective group>");
#		else
#			Print("<projective group of size ",Size(g),">");
#		fi;
#	end );
#
#InstallMethod( ViewObj, 
#	"for a projective group with gens and size",
#	[IsProjectivityGroup and HasGeneratorsOfGroup and HasSize],
#	function( g )
#		local gens;
#		gens := GeneratorsOfGroup(g);
#		if Length(gens) = 0 then
#			Print("<trivial projective group>");
#		else
#			Print("<projective group of size ",Size(g)," with ",
 #             Length(gens)," generators>");
#		fi;
#	end );
	
InstallMethod( ViewObj,	
	"for a projective collineation group",
	[IsProjectiveGroupWithFrob],
	function( g )
		Print("<projective collineation group>");
	end );

InstallMethod( ViewObj, 
	"for a trivial projective collineation group",
	[IsProjectiveGroupWithFrob and IsTrivial],
	function( g )
		Print("<trivial projective collineation group>");
	end );

InstallMethod( ViewObj, 
	"for a projective collineation group with gens",
	[IsProjectiveGroupWithFrob and HasGeneratorsOfGroup],
	function( g )
		local gens;
		gens := GeneratorsOfGroup(g);
		if Length(gens) = 0 then
			Print("<trivial projective collineation group>");
		else
			Print("<projective collineation group with ",Length(gens),
              " generators>");
		fi;
	end );

InstallMethod( ViewObj, 
	"for a projective collineation group with size",
	[IsProjectiveGroupWithFrob and HasSize],
	function( g )
		if Size(g) = 1 then
			Print("<trivial projective collineation group>");
		else
			Print("<projective collineation group of size ",Size(g),">");
		fi;
	end );

InstallMethod( ViewObj, 
	"for a projective collineation group with gens and size",
	[IsProjectiveGroupWithFrob and HasGeneratorsOfGroup and HasSize],
	function( g )
		local gens;
		gens := GeneratorsOfGroup(g);
		if Length(gens) = 0 then
			Print("<trivial projective collineation group>");
		else
			Print("<projective collineation group of size ",Size(g)," with ",
              Length(gens)," generators>");
		fi;
	end );


###################################################################
# Some operations for projective groups (without and with frobenius automorphism)
###################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#O  BaseField( <g> )
# returns the base field of the projective group <g>
## 


# ml 07/11/2012: I have taken out the view, print and display methods
# for projectivity groups, since these are also collineation groups in FinInG
#InstallMethod( BaseField, 
#	"for a projective group",
#	[IsProjectivityGroup],
#	function( g )
#		local f,gens;
#		if IsBound(g!.basefield) then
#			return g!.basefield;
#		fi;
#		if HasParent(g) then
#			f := BaseField(Parent(g));
#			g!.basefield := f;
#			return f;
#		fi;
 #   # Now start to investigate:
#		gens := GeneratorsOfGroup(g);
#		if Length(gens) > 0 then
#			g!.basefield := gens[1]!.fld;
#			return g!.basefield;
#		fi;
 #   # Now we have to give up:
#		Error("base field could not be determined");
#	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  BaseField( <g> )
# returns the base field of the projective collineation group <g>
## 
InstallMethod( BaseField, 
	"for a projective collineation group",
	[IsProjectiveGroupWithFrob],
	function( g )
		local f,gens,P;
		if IsBound(g!.basefield) then
			return g!.basefield;
		fi;
		# This if statement can cause an infinite loop!
		if HasParent(g) then  # JB 22/03/2014
			P := Parent(g);
			if IsBound(P!.basefield) then
				f := P!.basefield;
				g!.basefield := f;
				return f;
			fi;
		fi;
    # Now start to investigate:
		gens := GeneratorsOfGroup(g);
		if Length(gens) > 0 then
			g!.basefield := gens[1]!.fld;
			return g!.basefield;
		elif IsTrivial(g) then				#JB: 22/03/2014: The trivial group with no generators slipped through.
			g!.basefield := One(g)!.fld;
			return g!.basefield;
		fi;
				
    # Now we have to give up:
		Error("base field could not be determined");
	end );
	
# TO DO (22/03/2014): We ought to set the basefield on creating collineation groups.

# CHECKED 6/09/11 jdb
#############################################################################
#O  Dimension( <g> )
# returns the dimension of the projective group <g>. The dimension of this 
# group is defined as the vector space dimension of the projective space  
# of which <g> was defined as a projective group, or, in other words, as the 
# size of the matrices.
## 

# ml 07/11/2012: I have taken out the view, print and display methods
# for projectivity groups, since these are also collineation groups in FinInG

#InstallMethod( Dimension, 
#	"for a projective group",
#	[IsProjectivityGroup],
#	function( g )
#		local gens;
#		if HasParent(g) then
#			return Dimension(Parent(g));
#		fi;
 #   # Now start to investigate:
#		gens := GeneratorsOfGroup(g);
#		if Length(gens) > 0 then
#			return NrRows(gens[1]!.mat);
#		fi;
#		Error("dimension could not be determined");
#	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  Dimension( <g> )
# returns the dimension of the projective collineation group <g>. The dimension of this 
# group is defined as the vector space dimension of the projective space  
# of which <g> was defined as a projective group, or, in other words, as the 
# size of the matrices.
## 
InstallMethod( Dimension, 
	"for a projective collineation group",
	[IsProjectiveGroupWithFrob],
	function( g )
		local gens;
		if HasParent(g) and HasDimension(Parent(g)) then	#JB: 22/03/2014: Made sure the parent had a dimension first
			return Dimension(Parent(g));
		fi;
    # Now start to investigate:
		gens := GeneratorsOfGroup(g);
		if Length(gens) > 0 then
			return NrRows(gens[1]!.mat);
		elif IsTrivial(g) then				#JB: 22/03/2014: The trivial group with no generators slipped through.
		 	return NrRows(One(g)!.mat);
		fi;
		Error("dimension could not be determined");
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  OneImmutable( <g> )
# returns an immutable one of the projectivity group <g>
## 
# ml 07/11/2012: I have taken out the view, print and display methods
# for projectivity groups, since these are also collineation groups in FinInG

#InstallMethod( OneImmutable, 
#	"for a projective group",
#	# was: [IsGroup and IsProjectivityGroup], I think might be
#	[IsProjectivityGroup],
#	function( g )
#		local gens, o;
#		gens := GeneratorsOfGroup(g);
#		if Length(gens) = 0 then
#			if HasParent(g) then
#				gens := GeneratorsOfGroup(Parent(g));
#			else
#				Error("sorry, no generators, no one");
#			fi;
#		fi;
#		o := rec( mat := OneImmutable( gens[1]!.mat ), fld := gens[1]!.fld );
#		Objectify( NewType(FamilyObj(gens[1]), IsProjGrpElRep), o );
#		return o;
#	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  OneImmutable( <g> )
# returns immutable one of the projective collineation group <g>
## 
InstallMethod( OneImmutable, 
	"for a projective collineation group",
	# was [IsGroup and IsProjectiveGroupWithFrob], I think might be
	[IsProjectiveGroupWithFrob],
	function( g )
		local gens, o;
		gens := GeneratorsOfGroup(g);
		if Length(gens) = 0 then
			if HasParent(g) and HasOneImmutable(Parent(g)) then	# JB 22/03/2014
				gens := GeneratorsOfGroup(Parent(g));
			else
				Error("sorry, no generators, no one");
			fi;
		fi;
		o := rec( mat := OneImmutable( gens[1]!.mat ), fld := BaseField(g),
				frob := gens[1]!.frob^0 );
		Objectify( ProjElsWithFrobType, o);
		return o;
	end );

###################################################################
# All about actions. But low level stuff. In each appropriate files
# for particular geometries, user-friendly action functions must be
# provided.
###################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#P  CanComputeActionOnPoints( <g> )
# is set true if we consider the computation of the action feasible.
# for projective groups.
##
# ml 07/11/2012: I have taken out the view, print and display methods
# for projectivity groups, since these are also collineation groups in FinInG
 
#InstallMethod( CanComputeActionOnPoints, 
#	"for a projective group",
#	[IsProjectivityGroup],
#	function( g )
#		local d,q;
#		d := Dimension( g );
#		q := Size( BaseField( g ) );
#		if (q^d - 1)/(q-1) > FINING.LimitForCanComputeActionOnPoints then
#			return false;
#		else
#			return true;
#		fi;
#	end );
  
# CHECKED 6/09/11 jdb
#############################################################################
#P  CanComputeActionOnPoints( <g> )
# is set true if we consider the computation of the action feasible.
# for projective collineation groups.
## 
InstallMethod( CanComputeActionOnPoints, 
	"for a projective group with frob",
	[IsProjectiveGroupWithFrob],
	function( g )
		local d,q;
		d := Dimension( g );
		q := Size( BaseField( g ) );
		if (q^d - 1)/(q-1) > FINING.LimitForCanComputeActionOnPoints then
			return false;
		else
			return true;
		fi;
	end );
  
###################################################################
# Action functions for projective groups and projective collineation
# groups. The four action functions here are low level and not 
# intended for the user.
###################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#F  OnProjPoints( <line>, <el> )
# computes <line>^<el> where this action is the "natural" one, and <line> represents
# a projective point. We called it line, since this functions relies on the Gap action
# function OnLines, which is the "natural" actions of a matrix on a vector line.
# Important: despite its natural name, this function is *not* intended for the user.
# <line>: just a row vector, representing a vector line
# <el>: a projective group element (so a projectivity, *not* a projective collineation element.
# normalizing the result is handled by the Gap function OnLines. This function assumes that 
# the input vector is also normalized (says the GAP manual). This became reality on september 19, 2011 :-)
## 
InstallGlobalFunction( OnProjPoints,
	function( line, el )
		return OnLines(line,el!.mat);
	end );

# CHECKED 6/09/11 jdb
# CHANGED 19/09/2011 jdb + ml
# can be shortened if you use that OnLines normalizes the result.
#############################################################################
#F  OnProjPointsWithFrob( <line>, <el> )
# computes <line>^<el> where this action is the "natural" one, and <line> represents
# a projective point. This function relies on the GAP function OnLines (see above),
# the result is hence normalized
# Important: despite its natural name, this function is *not* intended for the user.
# <line>: just a row vector, representing a projective point.
# <el>: a projective collineation element 
## 
InstallGlobalFunction( OnProjPointsWithFrob,
  function( line, el )
    local vec,c;
#    vec := OnPoints(line,el!.mat)^el!.frob;
    vec := OnLines(line,el!.mat)^el!.frob;
    #c := PositionNonZero(vec);
    #if c <= Length( vec )  then
#        if not(IsMutable(vec)) then
#		 vec := ShallowCopy(vec);
#        fi;
#        MultVector(vec,Inverse( vec[c] ));
#    fi;
    return vec;
	end );

# CHECKED 6/09/11 jdb
# CHANGED 19/09/2011 jdb + ml
# CHANGED 20/09/2011 jdb + ml (SemiEchelonMat -> EchelonMat).
#############################################################################
#F  OnProjSubspacesNoFrob( <subspace>, <el> )
# computes <subspace>^<el> where this action is the "natural" one, and <subspace> represents
# a projective subspace. This function relies on the GAP action function
# OnSubspacesByCanonicalBasis. This function assumes as arguments a list (mat) of linearly 
# independent row vectors, in Hermite normal form (triangulied), and return the 
# mat*<el> in Hermite normal form. To be used in user action functions, we EchelonMat it, so that
# the output can be used directly in a Wrap.
# Important: despite its natural name, this function is *not* intended for the user.
# <el>: a projective group element (so a projectivity, *not* a projective collineation element.
## 
InstallGlobalFunction( OnProjSubspacesNoFrob,
	function( matrix, el )
		# matrix is a matrix containing the basis vectors of some subspace.
		local mat;
		mat := TriangulizeMat(OnSubspacesByCanonicalBasis(matrix,el!.mat));
		return mat;
		#return EchelonMat(OnSubspacesByCanonicalBasis(matrix,el!.mat)).vectors;
	end );

# CHECKED 6/09/11 jdb
# CHANGED 19/09/2011 jdb + ml
# CHANGED 20/09/2011 jdb + ml (SemiEchelonMat -> EchelonMat).
#############################################################################
#F  OnProjSubspacesWithFrob( <subspace>, <el> )
# computes <subspace>^<el> where this action is the "natural" one, and <subspace> represents
# a projective subspace. This function relies on the GAP action function
# OnRight, which computs the action of a matrix on a sub vector space. Here we have to rely on 
# OnRight, and so we have to EchelonMat afterwards.
# Important: despite its natural name, this function is *not* intended for the user.
# <el>: a projective group element (so a projectivity, *not* a projective collineation element.
##
InstallGlobalFunction( OnProjSubspacesWithFrob,
  function( matrix, el )
	# matrix is a matrix containing the basis vectors of some subspace.
    local vec,c;
    vec := OnRight(matrix,el!.mat)^el!.frob;
    if not(IsMutable(vec)) then
        vec := MutableCopyMat(vec);
    fi;
    TriangulizeMat(vec);
	#return EchelonMat(vec).vectors;
	return vec;
  end );

###################################################################
# Higher level user friendly operations to compute the action of a
# projective and a projective collineation group. These operations
# are based on the low level functions above.
###################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#P  ActionOnAllProjPoints( <g> )
# returns the action of the projective group <g> on the projective points
# of the underlying projective space.
## 

# ml 07/11/2012: I have taken out the view, print and display methods
# for projectivity groups, since these are also collineation groups in FinInG

#InstallMethod( ActionOnAllProjPoints, 
#	"for a projective group",
#	[ IsProjectivityGroup ],
#	function( pg )
#		local a,d,f,orb;
#		f := BaseField(pg);
#		d := Dimension(pg);
#		orb := MakeAllProjectivePoints(f,d);
#		a := ActionHomomorphism(pg,orb,OnProjPoints,"surjective");
#		SetIsInjective(a,true);
#		return a;
#	end );

# CHECKED 6/09/11 jdb
# cvec change 19/3/14
#############################################################################
#O  ActionOnAllProjPoints( <g> )
# returns the action of the projective collineation group <g> on the projective points
# of the underlying projective space.
## 
InstallMethod( ActionOnAllProjPoints, 
    "for a projective collineation group",
	[ IsProjectiveGroupWithFrob ],
	function( pg )
		local a,d,f,o,on,orb,v, m, j;
		Info(InfoFinInG,4,"Using ActionOnAllProjPoints");
		f := BaseField(pg);
		d := Dimension(pg);
		o := One(f);
		on := One(pg);
		v := ZeroMutable(on!.mat[1]);
		v[1] := o;
		#orb := Orbit(pg,v,OnProjPointsWithFrob);
		#orb := Orb(pg,v,OnProjPointsWithFrob);
		orb := [];
		for m in f^d do
			j := PositionNonZero(m);
		if j <= d and m[j] = o then
			Add(orb, CVec(m,f)); #here is the change.
		fi;
		od;
		a := ActionHomomorphism(pg,orb,OnProjPointsWithFrob,"surjective");
		SetIsInjective(a,true);
		return a;
	end );

###################################################################
# NiceMonomorphism material for projective and projective collineation 
# groups. 
###################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#F  NiceMonomorphismByOrbit( <g>, <x>, <op>, <orblen> )
# <g>: projective groups; <x>: an element; <op> operation suitable for <x> and <g>
# important: this functions relies on the GenSS package. 
# As you can probably guess: this is not intended for a user.
##
InstallGlobalFunction( NiceMonomorphismByOrbit,
  function(g,x,op,orblen)
    # g a funny group, Size attribute set!
    # x an element
    # op an operation suitable for x and g
    # It is guaranteed that g acts faithfully on the orbit.
    local cand,h,iso,nr,orb,pgens;
    if orblen <> false then
		orb := Orb(g,x,op,rec(orbsizelimit := orblen, hashlen := 2*orblen,
                              storenumbers := true));
        Enumerate(orb);
    else
        orb := Orb(g,x,op,rec(storenumbers := true));
        Enumerate(orb);
    fi;
	pgens := ActionOnOrbit(orb,GeneratorsOfGroup(g));
    h := GroupWithGenerators(pgens);
    SetSize(h,Size(g));
    nr := Minimum(100,Length(orb));
    cand := rec( points := orb{[1..nr]}, used := 0,
                 ops := ListWithIdenticalEntries(nr,op) );
	iso := GroupHomomorphismByImagesNCStabilizerChain(g,h,pgens,
              rec( Cand := cand ), rec( ) );
    SetIsBijective(iso,true);
    return iso;
  end );

# CHECKED 6/09/11 jdb
#############################################################################
#F  NiceMonomorphismByDomain( <g>, <dom>, <op> )
# <g>: projective groups, size attribute *set* ; <x>: an element; 
# <op> operation suitable for <x> and <g>
# important: this functions relies on the GenSS package. 
##
InstallGlobalFunction( NiceMonomorphismByDomain,
  function(g,dom,op)
    # g a funny group, Size attribute set!
    # dom an orbit of g
    # op the operation suitable for x and g
    # It is guaranteed that g acts faithfully on the orbit.
    local cand,gens,h,ht,i,iso,nr,pgens;
    ht := HTCreate(dom[1],rec(hashlen:=Length(dom)*2));
    for i in [1..Length(dom)] do
      HTAdd(ht,dom[i],i);
    od;
    pgens := [];
    gens := GeneratorsOfGroup(g);
    for i in [1..Length(gens)] do
        Add(pgens,PermList( List([1..Length(dom)],
                                 j->HTValue(ht,op(dom[j],gens[i]))) ));
    od;
    h := GroupWithGenerators(pgens);
    SetSize(h,Size(g));
    nr := Minimum(100,Length(dom));
    cand := rec( points := dom{[1..nr]}, used := 0,
                 ops := ListWithIdenticalEntries(nr,op) );
    iso := GroupHomomorphismByImagesNCStabilizerChain(g,h,pgens,
              rec( Cand := cand ), rec( ) );
    SetIsBijective(iso,true);
    return iso;  
  end );

# CHECKED 6/09/11 jdb
# cvec change 19/3/14
#############################################################################
#O  NiceMonomorphism( <pg> )
# <pg> is a projective group. This operation returns a nice monomorphism.
##
InstallMethod( NiceMonomorphism, 
	"for a projective group (feasible case)",
	[IsProjectivityGroup and CanComputeActionOnPoints and IsHandledByNiceMonomorphism], 
	50,
	function( pg )
	local hom, dom,bf;
	Info(InfoFinInG,4,"Using NiceMonomorphism for proj. group (feasible)");
	bf := BaseField(pg);
    dom := MakeAllProjectivePoints( bf, Dimension(pg) - 1);
    #dom := List(dom,x->CVec(x,bf)); # (ml 31/03/14) MakeAllProjectivePoints produces already cvecs 
	if FINING.Fast then
	   hom := NiceMonomorphismByDomain( pg, dom, OnProjPointsWithFrob );
    else 
       hom := ActionHomomorphism(pg, dom, OnProjPointsWithFrob, "surjective");    
       SetIsBijective(hom, true);
    fi;
    return hom;
	end );
  
# CHECKED 6/09/11 jdb
# cvec change 19/3/14
#############################################################################
#O  NiceMonomorphism( <pg> )
# <pg> is a projective group. This operation returns a nice monomorphism.
##
InstallMethod( NiceMonomorphism, 
	"for a projective group (nasty case)",
	[IsProjectiveGroupWithFrob and IsHandledByNiceMonomorphism], 
	50,
	function( pg )
		local can, dom, hom, bf;
		Info(InfoFinInG,4,"Using NiceMonomorphism for proj. group (nasty)");
		bf := BaseField(pg);
		can := CanComputeActionOnPoints(pg);
		if not(can) then
			Error("action on projective points not feasible to calculate");
		else
			dom := MakeAllProjectivePoints( BaseField(pg), Dimension(pg) - 1 );
		    #dom := List(dom,x->CVec(x,bf)); # (ml 31/03/14) MakeAllProjectivePoints produces already cvecs 
			if FINING.Fast then
				hom := NiceMonomorphismByDomain( pg, dom, OnProjPointsWithFrob );
			else 
				hom := ActionHomomorphism(pg, dom, OnProjPointsWithFrob, "surjective");    
				SetIsBijective(hom, true);
			fi;
			return hom; 
		fi;
	end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  NiceMonomorphism( <pg> )
# <pg> is a projective collineation group. This operation returns a nice monomorphism.
##
InstallMethod( NiceMonomorphism, 
	"for a projective collineation group (feasible case)",
	[IsProjectiveGroupWithFrob and CanComputeActionOnPoints and
	IsHandledByNiceMonomorphism], 1,
	function( pg )
		return ActionOnAllProjPoints( pg );
	end );
  
# CHECKED 6/09/11 jdb
#############################################################################
#O  NiceMonomorphism( <pg> )
# <pg> is a projective collineation group. This operation returns a nice monomorphism.
##
InstallMethod( NiceMonomorphism, 
	"for a projective collineation group (nasty case)",
	[IsProjectiveGroupWithFrob and IsHandledByNiceMonomorphism], 50,
	function( pg )
		local can;
		can := CanComputeActionOnPoints(pg);
		if not(can) then
			Error("action on projective points not feasible to calculate");
		else
			return ActionOnAllProjPoints( pg );
		fi;
	end );

## FindBasePointCandidates: are these methods also obsolete in the sense that they are never used? No! They are used in GenSS :-)

#############################################################################
#O  FindBasePointCandidates( <g>, <opt>, <i> )
# <pg> is a projective collineation group. <op> and <i> are arguments required
# to be used in GenSS.
##
InstallMethod( FindBasePointCandidates,
  "for a projective group",
  [IsProjectivityGroup,IsRecord,IsInt],
  function(g,opt,i)
    local cand,d,f,gens;
    if IsBound(g!.basepointcandidates) and
       g!.basepointcandidates.used < Length(g!.basepointcandidates.points) then
        return g!.basepointcandidates;
    fi;
    gens := GeneratorsOfGroup(g);
    if IsObjWithMemory(gens[1]) then
        f := BaseField(gens[1]!.el);
        d := NrRows(gens[1]!.el!.mat);
    else
        f := BaseField(g);
        d := Dimension(g);
    fi;
    cand := rec( points := NewMatrix(IsCMatRep, f,d, IdentityMat(d,f)) , used := 0,
                 ops := ListWithIdenticalEntries(d,OnProjPoints) );
    return cand;
  end );

#############################################################################
#O  FindBasePointCandidates( <g>, <opt>, <i> )
# <pg> is a projective collineation group. <op> and <i> are arguments required
# to be used in GenSS.
##
InstallMethod( FindBasePointCandidates,
  "for a projective collineation group",
  [IsProjectiveGroupWithFrob,IsRecord,IsInt],
  function(g,opt,i)
    local cand,d,f,gens;
    if IsBound(g!.basepointcandidates) and
       g!.basepointcandidates.used < Length(g!.basepointcandidates.points) then
        return g!.basepointcandidates;
    fi;
    gens := GeneratorsOfGroup(g);
    if IsObjWithMemory(gens[1]) then
        f := BaseField(gens[1]!.el);
        d := NrRows(gens[1]!.el!.mat);
    else
        f := BaseField(g);
        d := Dimension(g);
    fi;
    cand := rec( points := NewMatrix(IsCMatRep, f,d, IdentityMat(d,f)), used := 0,
                 ops := ListWithIdenticalEntries(d,OnProjPointsWithFrob) );
    if d > 1 then
        Add(cand.points,ShallowCopy(cand.points[1]));
        Add(cand.ops,OnProjPointsWithFrob);
        cand.points[d+1][2] := PrimitiveRoot(f);
    fi;
    return cand;
  end );


#############################################################################
#O  FindBasePointCandidates( <g>, <opt>, <i> )
# <pg> is a projective collineation group. <opt>, <i>, <parentS> are arguments 
# required to be used in GenSS.
##
InstallMethod( FindBasePointCandidates,
  "for a projective collineation group",
  [IsProjectiveGroupWithFrob,IsRecord,IsInt,IsObject],
#
# We need a four-argument version of this method for recent versions of GenSS.
# We don't use "parentS" at all here.
#
  function(g,opt,i,parentS)
    local cand,d,f,j,gens;
    if IsBound(g!.basepointcandidates) and
       g!.basepointcandidates.used < Length(g!.basepointcandidates.points) then
        return g!.basepointcandidates;
    fi;
    gens := GeneratorsOfGroup(g);
    if IsObjWithMemory(gens[1]) then
        f := BaseField(gens[1]!.el);
        d := NrRows(gens[1]!.el!.mat);
    else
        f := BaseField(g);
        d := Dimension(g);
    fi;
    cand := rec( points := NewMatrix(IsCMatRep, f,d, IdentityMat(d,f)), used := 0,
                 ops := ListWithIdenticalEntries(d,OnProjPointsWithFrob) );
    if d > 1 then
        for j in [2..d] do
            Add(cand.points,ShallowCopy(cand.points[1]));
            Add(cand.ops,OnProjPointsWithFrob);
            cand.points[d+j-1][j] := PrimitiveRoot(f);
        od;
    fi;
    return cand;
  end );


#################################################
# Our classical groups:
#################################################


#############################################################################
# Part I: methods for canonical gram matrices and canonical quadratic forms.
# these methods are not intended for the user.
#############################################################################

# CHECKED 21/09/11 jdb
#############################################################################
#O  CanonicalGramMatrix( <type>, <d>, <f> )
## Constructs the canonical gram matrix to construct the canonical 
## forms used in FinInG. See Appendix for exact information on these forms
## and there matrices.
##
InstallMethod( CanonicalGramMatrix, 
	"for a string, an integer, and a field",
	[IsString, IsPosInt, IsField],
	function( type, d, f )
		local one, q, m, i, t, x, w, p;
		one := One( f );
		q := Size(f);

      # Symplectic Gram matrix
		if type = "symplectic" then    
			if IsOddInt(d) then
				Error( "the dimension <d> must be even" );
			fi;    
			m := List( 0 * IdentityMat(d, f), ShallowCopy );
			for i  in [ 1 .. d/2 ]  do
				m[2*i,2*i-1] := -one;
				m[2*i-1,2*i] := one;
			od;
    
      # Unitary Gram matrix      
		elif type = "hermitian" then
			if IsOddInt(DegreeOverPrimeField(f)) then
				Error("field order must be a square");
			fi;
			m := IdentityMat(d, f);
        
      # Orthogonal Gram matrix
		elif type = "hyperbolic" then
			m := List( 0*IdentityMat(d, f), ShallowCopy );
			p := Characteristic(f);
			if IsOddInt(p) and (p + 1) mod 4 = 0 then
				w := one * ((p + 1) / 2);
			else
				w := one;
			fi; 
			for i  in [ 1 .. d/2 ]  do
				m[2*i-1,2*i] := w;
				m[2*i,2*i-1] := w;
			od;
		elif type = "elliptic" then   
			m := List( 0*IdentityMat(d, f), ShallowCopy );
			p := Characteristic(f);
      ## if q is congruent to 5,7 mod 8, then #wrong comment?
      ## the anisotropic part is the primitive root.
			if q mod 4 in [1,2] then 
				t := Z(q);
			else
				t := one;
			fi;
			m{[1,2]}{[1,2]} := [ [ 1, 0 ], [ 0, t ] ] * one;

			if IsOddInt(p) then
				w := one * ((p + 1) / 2);
			else
				w := one;
			fi; 
			for i in [ 2 .. d/2 ]  do
				m[2*i-1,2*i] := w;
				m[2*i,2*i-1] := w;
			od;
		elif type = "parabolic" then 
			m := List( 0*IdentityMat(d, f), ShallowCopy );          
			p := Characteristic(f);
			if IsOddInt(p) then
         ## if q is congruent to 5,7 mod 8, then
         ## the anisotropic part is the primitive root
         ## of the prime subfield.
				if q mod 8 in [5,7] then 
					t := Z(p);
				else
					t := one;
				fi;
				m[1,1] := t;
				w := t * ((p + 1) / 2);
			else
				w := one;
			fi; 
			for i in [ 1 .. (d-1)/2 ]  do
				m[2*i,2*i+1] := w;
				m[2*i+1,2*i] := w;
			od;
		else Error( "type is unknown or not implemented" );
		fi;

    ##  We should return a compressed matrix in order that
    ##  our computations are efficient

		ConvertToMatrixRep( m, f );
		return m;
	end );

# CHECKED 21/09/11 jdb
#############################################################################
#O  CanonicalQuadraticForm( <type>, <d>, <f> )
## Constructs the canonical gram matrix to construct the canonical quadratic
## forms used in FinInG. See Appendix for exact information on these forms
## and there matrices.
####
InstallMethod( CanonicalQuadraticForm, 
	"for a string, an integer and a field",
	[IsString, IsPosInt, IsField],
	function( type, d, f )
		local m, one, q, p, j, x, R;
		one := One( f );
        if type = "hyperbolic" then
			m := MutableCopyMat(0 * IdentityMat(d, f));
			for j in [ 1 .. d/2 ]  do
				m[ 2*j-1 , 2*j ] := one;
			od;
		elif type = "elliptic" then
			m := MutableCopyMat(0 * IdentityMat(d, f));
			m[1,1] := one;
			m[2,1] := one; 
			m[d,d-1] := one;
			for j in [ 2 .. d/2-1 ]  do
				m[ 2*j-1 , 2*j ] := one;
			od;
			p := Characteristic(f);
			q := Size(f);
			if IsOddInt(Log(q, p)) then
				m[2,2] := one;
			else
				R := PolynomialRing( f, 1 );
				x := Indeterminate( f );
				m[2,2] := Z(q)^First( [ 0 .. q-2 ], u -> 
					Length( Factors( R, x^2+x+PrimitiveRoot( f )^u ) ) = 1 );         
			fi;
		elif type = "parabolic" then
			m := MutableCopyMat(0 * IdentityMat(d, f));
			m[1,1] := one;
			for j in [ 1 .. (d-1)/2 ]  do
				m[ 2*j+1 , 2*j ] := one;
			od;
		else Error( "type is unknown or not implemented" );
		fi;

    ##  We should return a compressed matrix in order that
    ##  our computations are efficient

		ConvertToMatrixRep( m, f );
		return m;
	end );


#############################################################################
# Part II: constructor methods.
#############################################################################

###################################################################################
#
#  JB 20/11/2011:  
#  I've done some rigorous testing, and the following work very well:
#    all groups of SymplecticSpace
#    all groups of HyperbolicQuadric
#    all groups of HermitianPolarSpace (hard to check though, things get big fast)
#    all groups of EllipticQuadric (I've recently fixed a bug)
#    all groups of ParabolicQuadric (I've recently fixed a bug)
#
# I guess we could run more tests for higher dimensions and field orders, but I'm
# reasonably confident that it all works well. For example, I tested groups
# of the parabolic quadric up to q^d = 8^6 (d is the projective dimension here).
#    
###################################################################################

#####################################################################
# Isometry groups. In this order: PSO, PGO, PSU, PGU, PSp, and PGSp
#####################################################################

###### Orthogonal groups ######

#############################################################################
#O  SOdesargues( <type>, <d>, <f> )
## returns the projective special orthogonal group, as a projective collineation group.
## The generators of the group are the projective collineation elements that are 
## represented by the matrices that generate SO(type,d,q). The latter is available in GAP
##
InstallMethod( SOdesargues, 
  "for an integer, a positive integer, and a finite field",
  [IsInt, IsPosInt, IsField and IsFinite],
    function(i, d, f)
    local s, m, frob, b, gens, g, q;
    if i = -1 then s := "elliptic";
    elif i = 0 then s := "parabolic";
    elif i = 1 then s := "hyperbolic";
    fi;
    q := Size(f);
    if IsEvenInt(q) then
      m := InvariantQuadraticForm(SO(i,d,q))!.matrix;
      b := BaseChangeOrthogonalQuadratic(m, f)[1];
    else 
      m := InvariantBilinearForm(SO(i,d,q))!.matrix;
      b := BaseChangeOrthogonalBilinear(m, f)[1]; 
    fi; 
    frob := FrobeniusAutomorphism(f);
 
    ## new group elements: x -> b^-1 x b (conjugation by base change)
    ## preserves form... (b x b^-1) (b m b^T) (b x b^-1)^T = x m x^T

    gens := GeneratorsOfGroup( SO(i,d,q) );
    gens := List( gens, y -> b * y * b^-1);
    gens := ProjElsWithFrob( List(gens, x -> [x,frob^0]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PSO(",String(i),",",String(d),",",String(q),")") );
    if i = 0 then 
       SetSize( g, Size(SO(i, d, q)) );  ##/ 2); This might be a mistake in Kleidman and Liebeck!
    else
       SetSize( g, Size(SO(i, d, q)) / GCD_INT(2, q-1) ); 
    fi;

    return g;
  end );

#############################################################################
#O  GOdesargues( <type>, <d>, <f> )
## returns the projective general orthogonal group, as a projective collineation group.
## The generators of the group are the projective collineation elements that are 
## represented by the matrices that generate GO(type,d,q). The latter is available in GAP
##
InstallMethod( GOdesargues, [IsInt, IsPosInt, IsField and IsFinite],
  function(i, d, f)
    local m, frob, b, gens, s, g, q;
    if i = -1 then s := "elliptic";
    elif i = 0 then s := "parabolic";
    elif i = 1 then s := "hyperbolic";
    fi;
    q := Size(f);
    if IsEvenInt(q) then
      m := InvariantQuadraticForm(GO(i,d,q))!.matrix;
      b := BaseChangeOrthogonalQuadratic(m, f)[1];
    else 
      m := InvariantBilinearForm(SO(i,d,q))!.matrix;   
      b := BaseChangeOrthogonalBilinear(m, f)[1]; 
    fi; 
    frob := FrobeniusAutomorphism(f);

    ## new group elements: x -> b x b^-1 (conjugation by base change)
    ## preserves form... (b x b^-1) (b m b^T) (b x b^-1)^T = x m x^T

    gens := GeneratorsOfGroup( GO(i,d,q) );
    gens := List( gens, y -> b * y * b^-1);
    gens := ProjElsWithFrob( List(gens, x -> [x,frob^0]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PGO(",String(i),",",String(d),",",String(q),")") );
    SetSize( g, Size(GO(i, d, q)) / GCD_INT(2,q-1) ); 
    return g;
  end );

###### Unitary groups ######

#############################################################################
#O  SUdesargues( <type>, <d>, <f> )
## returns the projective special unitary group, as a projective collineation group.
## The generators of the group are the projective collineation elements that are 
## represented by the matrices that generate SU(type,d,q). The latter is available in GAP
##
InstallMethod( SUdesargues, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local m, frob, b, gens, g, sqrtq;
    sqrtq := Sqrt(Size(f));
    m := InvariantSesquilinearForm(SU(d,sqrtq))!.matrix;        
    frob := FrobeniusAutomorphism(f);
    b := BaseChangeHermitian(m, f)[1];
    ## new group elements: x -> b x b^-1 (conjugation by base change)
    ## preserves form... (b x b^-1) (b m b^Tfrob) (b x b^-1)^Tfrob = x m x^Tfrob
    gens := GeneratorsOfGroup( SU(d,sqrtq) );
    gens := List( gens, y -> b * y * b^-1);
    gens := ProjElsWithFrob( List(gens, x -> [x,frob^0]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PSU(",String(d),",",String(sqrtq),"^2)") );
    SetSize( g, Size( SU(d, sqrtq) ) / GCD_INT(sqrtq+1,d)  );
    return g;
  end );

#############################################################################
#O  GUdesargues( <type>, <d>, <f> )
## returns the projective general unitary group, as a projective collineation group.
## The generators of the group are the projective collineation elements that are 
## represented by the matrices that generate GU(type,d,q). The latter is available in GAP
##
InstallMethod( GUdesargues, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local m, frob, b, gens, g, sqrtq;
    sqrtq := Sqrt(Size(f));
    m := InvariantSesquilinearForm(GU(d,sqrtq))!.matrix;      
    frob := FrobeniusAutomorphism(f);
    b := BaseChangeHermitian(m, f)[1];
    ## new group elements: x -> b x b^-1 (conjugation by base change)
    gens := GeneratorsOfGroup( GU(d,sqrtq) );
    gens := List( gens, y -> b * y * b^-1);
    gens := ProjElsWithFrob( List(gens, x -> [x,frob^0]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PGU(",String(d),",",String(sqrtq),"^2)") );
    SetSize( g, Size( GU(d, sqrtq) ) / (sqrtq+1) );
    return g;
  end );

###### Symplectic groups ######

#############################################################################
#O  Spdesargues( <d>, <f> )
## returns the projective special symplectic group, as a projective collineation group.
## The generators of the group are the projective collineation elements that are 
## represented by the matrices that generate Sp(d,q). The latter is available in GAP
##
InstallMethod( Spdesargues, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local m, frob, b, gens, g, q, sp;
    q := Size(f);
    m := InvariantBilinearForm(Sp(d,q))!.matrix;   
    b := BaseChangeSymplectic(m, f)[1];  ## change made after new forms code
    frob := FrobeniusAutomorphism(f);
    ## new group elements: x -> b x b^-1 (conjugation by base change)
    sp := Sp(d,q);
    gens := GeneratorsOfGroup( sp );
    gens := List( gens, y -> b * y * b^-1);
    gens := ProjElsWithFrob( List(gens, x -> [x,frob^0]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PSp(",String(d),",",String(q),")") );
    SetSize( g, Size( sp )/GCD_INT(2, q-1) );
    return g;
  end );

#############################################################################
#O  GeneralSymplecticGroup( <d>, <f> )
## returns the general symplectic group. See the internal comment here why
## we add this function.
##
InstallMethod( GeneralSymplecticGroup, [IsPosInt, IsField and IsFinite],
  function(d, f)

## The command "Sp" in the GAP library returns the symplectic 
## isometry group (see classical.gi). However, in odd characteristic,
## the symplectic isometries have index 2 in the symplectic similarity
## group. The preimage in the matrix group of the symplectic 
## similarities is the isometries extended by the cyclic
## group generated by the map which simply multiplies one half
## of the symplectic basis by the primitive element of the field
## and leaves the other half alone. 
  
  local sp, gens, z, delta, i, g, q;
  q := Size(f);
  if IsOddInt(d) then 
     Error("dimension must be an even");
  fi;
  sp := Sp(d, q);
  gens := GeneratorsOfGroup(sp);
  z := PrimitiveRoot( f );
  delta := IdentityMat(d, f);
  for i in [1..d/2] do delta[i,i] := z; od;
  gens := Concatenation(gens, [delta]);
  g := GroupWithGenerators( gens );
  SetName( g, Concatenation("GSp(",String(d),",",String(q),")") );
  SetSize( g, (q-1) * Size(sp) );
  return g;
  end );

#############################################################################
#O  GSpdesargues( <d>, <f> )
## returns the projective general symplectic group, as a projective collineation group.
## The generators of the group are the projective collineation elements that are 
## represented by the matrices that generate GSp(d,q). The latter is made possible now (see above method).
##
InstallMethod( GSpdesargues, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local m, frob, b, gens, g, q, gsp;
    q := Size(f);
    m := InvariantBilinearForm(Sp(d,q))!.matrix;     
    b := BaseChangeToCanonical( BilinearFormByMatrix(m, f) );
    frob := FrobeniusAutomorphism(f);
    ## new group elements: x -> b x b^-1 (conjugation by base change)
    gsp := GeneralSymplecticGroup(d,f);
    gens := GeneratorsOfGroup( gsp );
    gens := List( gens, y -> b * y * b^-1);
    gens := ProjElsWithFrob( List(gens, x -> [x,frob^0]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PGSp(",String(d),",",String(q),")") );
    SetSize( g, Size(gsp) / (q-1) );    
    return g;
  end );

#################################################
# Similarity and semi-similarity groups: In this order: PGammaSp, DeltaO (+,-,parabolic), 
#################################################

###### Symplectic group ######

#############################################################################
#O  GammaSp( <d>, <f> )
## returns the projective semi-linear symplectic group.
## We rely on GSp(d,q), and the frobenius automorphism.
##
InstallMethod( GammaSp, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local m, frob, b, gens, g, q, gsp;
    q := Size(f);
    m := InvariantBilinearForm(Sp(d,q))!.matrix;     
    b := BaseChangeToCanonical( BilinearFormByMatrix(m, f) );
    frob := FrobeniusAutomorphism(f);
    ## new group elements: x -> b x b^-1 (conjugation by base change)
    gsp := GeneralSymplecticGroup(d,f);
    gens := GeneratorsOfGroup( gsp );
    gens := List( gens, y -> b * y * b^-1);
    gens := List( gens, x -> [x, frob^0]);
    Add(gens, [IdentityMat(d, f), frob] );
    gens := ProjElsWithFrob( gens, f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PGammaSp(",String(d),",",String(q),")") );
    SetSize( g, Order(frob) * Size(gsp) / (q - 1) );  
    return g;
  end );

###### Orthogonal (elliptic) groups ######

#############################################################################
#O  DeltaOminus( <d>, <f> )
## returns the projective similarity group of an elliptic orthogonal form
## We rely on GOdesargues.
##
InstallMethod( DeltaOminus, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local go, gens, g, q, one, mat, i, combs, two, a, b, 
          twobytwo, mu, z, zero;  

    ## Note here that for q even, the projective similarity group
    ## is equal to the projective isometry group. For q odd,
    ## the index of the projective isometry group in the projective
    ## similarity group is 2. Moreover, we have two cases modulo 4.
    ## For q = 3 mod 4, we adjoin the matrix (e.g., for d = 6)
    ##    a  b  .  .  .  .  
    ##    b -a  .  .  .  .  
    ##    .  .  .  1  .  .  
    ##    .  . -1  .  .  .  
    ##    .  .  .  .  .  1  
    ##    .  .  .  . -1  .
    ## where a^2+b^2 = Z(q)^((q-1)/2), 
    ## to the isometry group to obtain the similarity group (see
    ## Kleidman and Liebeck, Section 2.8). For q = 1 mod 4, we
    ## use the matrix
    ##    .  1  .  .  .  .  
    ##    z  .  .  .  .  .  
    ##    .  .  .  1  .  .  
    ##    .  .  z  .  .  .  
    ##    .  .  .  .  .  1  
    ##    .  .  .  .  z  .
    ## where z = Z(q).

    one := One(f);
    mat := NullMat(d,d,f);
    q := Size(f);
    z := Z(q);
    go := GOdesargues(-1,d,f);

    if q mod 4 = 3 then
       for i in [2..d/2] do
           mat[2*i-1,2*i] := one;   
           mat[2*i,2*i-1] := -one;  
       od;  
       mu := z^((q-1)/2);;
       combs := Combinations(AsList(f),2);;
       two := First(combs, t -> not IsZero(t[1]) and not IsZero(t[2])
                           and t[1]^2+t[2]^2=mu);
       a := two[1]; b:= two[2];
       twobytwo := [[a,b],[b,-a]];
       mat{[1,2]}{[1,2]} := twobytwo;
    elif q mod 4 = 1 then
       z := Z(q);
       zero := Zero(f);
       for i in [1..d/2] do
           mat{[2*i-1,2*i]}{[2*i-1,2*i]} := [[ zero, one ], [z, zero]];
       od;
    else 
       return go; 
    fi;

    gens := ShallowCopy(GeneratorsOfGroup( go ));
    Add(gens, ProjElWithFrob( mat, IdentityMapping(f), f) );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PDeltaO-(",String(d),",",String(Size(f)),")") );
       ## scalars are completely contained in matrix group, (q-1) cancels with (q-1)
    SetSize( g, Size(GO(-1,d,q)) ); 
    return g;
  end );	


#############################################################################
#O  GammaOminus( <d>, <f> )
## returns the projective semi-similarity group of an elliptic orthogonal form
## We rely on DeltaOminus and the Frobenius automorphism.
##
InstallMethod( GammaOminus, [IsPosInt, IsField and IsFinite],
  function(d, f)
  local q, gram, mat, p, a, go, gens, coll, frob, block, mat2, i; 
  
  ## Works beautifully for odd q! Tested for possible output 
  ## (q,d) in {(4,9),(4,25),(4,27),(4,49),(6,9)}

  ## After some calculations using the information in Section 2.8
  ## of Kleidman and Liebeck, we find that the following matrix M 
  ## (n.b., you should extrapolate the dimension) together with
  ## the Frobenius automorphism of GF(q) defines a semisimilarity
  ## of our canonical form in FinInG:
  ##    a  .  .  .  .  .  
  ##    .  b  .  .  .  .  
  ##    .  .  .  l  .  .  
  ##    .  .  l  .  .  .  
  ##    .  .  .  .  .  l  
  ##    .  .  .  .  l  .
  ## where a = l * alpha^((1-p)/2) and b = l * beta^((1-p)/2), and 
  ## the first block of our Gram matrix is diag(alpha, beta).
  ##  JB 20/11/2011: Fixed the bug for q even. Simply changed the change of basis matrix.

  q := Size(f);
  p := Characteristic( f );
  go := DeltaOminus( d, f );

  if q = p then
     coll := go;
  elif IsOddInt(q) then
     gram := CanonicalGramMatrix("elliptic", d, f);
     mat := MutableCopyMat( gram );
	 mat[1,1] := gram[3,4] * gram[1,1]^((1-p)/2); 
	 mat[2,2] := gram[3,4] * gram[2,2]^((1-p)/2);
	 ConvertToMatrixRep( mat, f );
	 frob := FrobeniusAutomorphism( f );
	 a := ProjElWithFrob( mat, frob, f);
     gens := ShallowCopy( GeneratorsOfGroup(go) );
     Add(gens, a);
     coll := GroupWithGenerators( gens );
	 SetName( coll, Concatenation("PGammaO-(",String(d),",",String(q),")") );
	 SetSize( coll, Size(go) * Order(frob) );
  else
     ## We must find a semisimilarity for the first (2x2) block. Then
     ## simply extend it naturally to fit with the canonical form.

     gram := CanonicalQuadraticForm("elliptic", d, f);
     block :=Forms_RESET( MutableCopyMat( gram{[1,2]}{[1,2]} ), 2, q);
	 frob := FrobeniusAutomorphism( f );
	 mat := BaseChangeOrthogonalQuadratic(block^(frob^-1), f)[1];
	
 	 ## JB is a little bit unsure if this will work all the time. So a test is needed:
	 if not Forms_RESET(mat * block^(frob^-1) *TransposedMat(mat), 2, q) = block then
	    Error("Inappropriate matrix for change of basis");
	 fi;
	
     mat2 := IdentityMat(d,f);  
     mat2{[1,2]}{[1,2]} := mat;
	 a := ProjElWithFrob( mat2, frob, f);
	 gens := ShallowCopy( GeneratorsOfGroup(go) );
     Add(gens, a);
     coll := GroupWithGenerators( gens );
	 SetName( coll, Concatenation("PGammaO-(",String(d),",",String(q),")") );
	 SetSize( coll, Size(go) * Order(frob) );
  fi;
  
  return coll;
  end );

###### Orthogonal (parabolic) groups ######

#############################################################################
#O  GammaO( <d>, <f> )
## returns the projective semi-similarity group of a parabolic orthogonal form
## We rely on GO (available in GAP).
##
InstallMethod( GammaO, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local q, p, go, gens, frob, m, b, lambda, g, w, one;
    one := One(f);
    q := Size(f);
    p := Characteristic(f);
    go := GO(0, d, q);
    gens := ShallowCopy(GeneratorsOfGroup(go));
    frob := FrobeniusAutomorphism( f );
    if IsOddInt(q) then
      m := InvariantBilinearForm(GO(0,d,q))!.matrix;
      b := BaseChangeOrthogonalBilinear(m, f)[1]; 
      gens := List(gens, t->b*t*b^-1);; 
    else 
      m := InvariantQuadraticForm(GO(0,d,q))!.matrix;
      b := BaseChangeOrthogonalQuadratic(m, f)[1];
      gens := List(gens, t->b*t*b^-1);;
    fi;
    if IsOddInt(p) then
       if q mod 8 in [5,7] then 
          w := Z(p) * ((p + 1) / 2);
       else
          w := one * ((p + 1) / 2);
       fi; 
    else
       w := one;
    fi; 
    lambda := First(AsList(f),t->t^2 = w^(p-1));
    gens := List(gens, x -> [x,frob^0]);
    Add(gens, [lambda * IdentityMat(d, f), frob] );
    gens := ProjElsWithFrob( gens, f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PGammaO(",String(d),",",String(q),")") );  
    SetSize( g, Order(frob) * Size(go) / GCD_INT(2,q-1) );    
       ## Careful to read  Kleidman and Liebeck correctly. For q even, we take the symplectic group.
    return g;
  end );


###### Orthogonal (hyperbolic) groups ######

#############################################################################
#O  DeltaOplus( <d>, <f> )
## returns the projective similarity group of an hyperbolic orthogonal form
## We rely on GO (available in GAP).
##
InstallMethod( DeltaOplus, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local q, go, m, w, mu, i, gens, g, b;  
    q := Size(f);
    go := GO(1, d, q);
    gens := ShallowCopy(GeneratorsOfGroup(go));
  
    if IsOddInt(q) then
      m := InvariantBilinearForm(go)!.matrix;
      b := BaseChangeOrthogonalBilinear(m, f)[1]; 
      w := PrimitiveRoot( f );
    
    ## put primitive root on odd places of diagonal
    ## of identity matrix

      mu := IdentityMat(d,f);
      for i in [1..d/2] do
        mu[2*i-1,2*i-1] := w;
      od;

      gens := List(gens, t->b*t*b^-1);; 
      Add(gens, mu);
    else 
      m := InvariantQuadraticForm(go)!.matrix;
      b := BaseChangeOrthogonalQuadratic(m, f)[1];
      gens := List(gens, t->b*t*b^-1);;
    fi;

    gens := ProjElsWithFrob( List(gens, x -> [x,IdentityMapping(f)]), f );
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PDeltaO+(",String(d),",",String(q),")") );
    SetSize( g, 2*q^(d*(d-2)/4)*(q^(d/2)-1)*Product(List([1..d/2-1],
                   i -> (q^(2*i)-1) )) );
    return g;
  end );

#############################################################################
#O  GammaOplus( <d>, <f> )
## returns the projective semi-similarity group of an hyperbolic orthogonal form
## We rely on DeltaOplus and the Frobenius automorphism.
##
InstallMethod( GammaOplus, [IsPosInt, IsField and IsFinite],
  function(d, f)
    local q, deltao, gens, frob, lambda, g;
    q := Size(f);
    deltao := DeltaOplus(d, f);
    gens := ShallowCopy(GeneratorsOfGroup(deltao));
    frob := FrobeniusAutomorphism( f );
    Add(gens, ProjElWithFrob( IdentityMat(d, f), frob, f ));
    g := GroupWithGenerators( gens );
    SetName( g, Concatenation("PGammaO+(",String(d),",",String(q),")") );
    SetSize( g, Log(q, Characteristic(f)) * Size(deltao) );
    return g;
  end );

###### Hermitian groups ######

#############################################################################
#O  GammaU( <d>, <f> )
## returns the projective semi-similarity group of hermitian form
## We rely on GU and the Frobenius automorphism.
##
## Issue here. The centre of GammaU is nontrivial for d=2.
## Our construction of classical groups in FinInG factors out the scalars,
## but not the full centre.
##
InstallMethod( GammaU, [IsPosInt, IsField and IsFinite],
  function(d, f)
   ## bug here, there is more than scalars in the kernel of the action!
    local m, frob, b, gens, g, q, sqrtq, gu;
    q := Size(f);
    sqrtq := Sqrt(Size(f));
    m := InvariantSesquilinearForm(GU(d,sqrtq))!.matrix;     
    b := BaseChangeHermitian(m, f)[1];
    frob := FrobeniusAutomorphism(f);
    gu := GU(d,sqrtq);
    gens := GeneratorsOfGroup( gu );
    gens := List( gens, y -> b * y * b^-1);
    gens := List( gens, x -> [x, frob^0]);
    Add(gens, [IdentityMat(d, f), frob] );
    gens := ProjElsWithFrob( gens, f );
    g := GroupWithGenerators( gens );
    SetSize( g, Order(frob) * Size(gu) / (sqrtq+1) );
    if d = 2 then
       Info(InfoFinInG, 1, "Warning: We have only factored scalars out of GammaU to construct a central cover of PGammaU.\n The centre is thus nontrivial and acts trivially on totally isotropic 1-spaces.");
       Info(InfoFinInG, 2, "So be careful because you're opening a can of worms!");
       SetName( g, Concatenation("2.PGammaU(",String(d),",",String(sqrtq),"^2)") );
    else
       SetName( g, Concatenation("PGammaU(",String(d),",",String(sqrtq),"^2)") ); 
    fi;
    return g;
  end );






