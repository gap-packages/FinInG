#############################################################################
##
##  correlations.gi              FinInG package
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
##  Implementation stuff for correlation groups
##
#############################################################################

###################################################################
# Code for the "standard duality" of a projective space. We want it to 
# be an involutory mapping. To be used later on in the objects 
# representing a correlation. 
# Difficulty: make it a mapping and overload the \* operator
# (because we know that delta^-1 = delta.
###################################################################



# CHECKED 14/09/11 jdb
#############################################################################
#O  IdentityMappingOfElementsOfProjectiveSpace( <ps> )
# returns the identity mapping on the collection of subspaces of a projective space <ps>
## 
InstallMethod( IdentityMappingOfElementsOfProjectiveSpace, 
   "for a projective space",
   [IsProjectiveSpace],
   function(ps)
		local map;
		map := IdentityMapping(ElementsOfIncidenceStructure(ps));
		SetFilterObj(map,IsIdentityMappingOfElementsOfProjectiveSpace);
		return map;
	end );

# CHECKED 14/09/11 jdb
#############################################################################
#O  StandardDualityOfProjectiveSpace( <ps> )
# returns the Standard duality of a projective space
## 
InstallMethod( StandardDualityOfProjectiveSpace, 
	"for a projective space",
	[IsProjectiveSpace],
	function( ps )
    ## In this method, we implement the standard duality acting
    ## on projective spaces.
		local map, f, S, ty, obj, one;
		f := function(v)
        local ty, b, newb, rk;
        ty := v!.type;
        b := v!.obj;
        if ty = 1 then
			b := [b];
        fi;       
        newb := NullspaceMat(TransposedMat(b));
        rk := Rank(newb);
        if rk = 1 then 
			newb := newb[1];
        fi;
        return VectorSpaceToElement(ps, newb);  ## check that this normalises the element
		end;
		S := ElementsOfIncidenceStructure(ps);   ##    map := MappingByFunction(ElementsOfIncidenceStructure(ps), ElementsOfIncidenceStructure(ps), f, f);
		ty := TypeOfDefaultGeneralMapping( S, S,
					IsStandardDualityOfProjectiveSpace
					and IsSPMappingByFunctionWithInverseRep
					and IsBijective and HasOrder and HasIsOne);
		# We looked at the code to create a MappingByFunction. Restricted the filter
		# was sufficient to be able to overload \* for my new category of mappings.
		obj := rec( fun := f, invFun := f, preFun := f, ps := ps );
		one := IdentityMappingOfElementsOfProjectiveSpace(ps);
		ObjectifyWithAttributes(obj, ty, Order, 2, IsOne, false, One, one); #this command I learned in st andrews!
		Setter(InverseImmutable)(obj,obj); #cannot set this attribute in previous line, because map does not exist then
		return obj;
	end );


# Added ml 5/02/2013
#############################################################################
#O  IsCollineation( <g> )
##
InstallMethod( IsCollineation, [ IsProjGrpElWithFrobWithPSIsom],
  function( g )
        if IsIdentityMappingOfElementsOfProjectiveSpace(g!.psisom) then
        return true;
        else return false;
        fi;
end );


# Added ml 8/11/2012
#############################################################################
#O  IsCorrelation( <g> )
# method to check if a given correlation-collineation of a projective space is 
# a correlation, i.e. if the underlying psisom is not the IdentityMappingOfElementsOfProjectiveSpace
## 
InstallMethod( IsCorrelation, [ IsProjGrpElWithFrobWithPSIsom ],
  function( g )
    local delta;
		delta:=g!.psisom;
		if IsIdentityMappingOfElementsOfProjectiveSpace(delta)
		then return false;
		else return true;
		fi;
  end );
  
# Added ml 5/02/2013 User friendly method
#############################################################################
#O  IsCorrelation( <g> )
# 
## 
InstallMethod( IsCorrelation, [ IsProjGrpElWithFrob ],
  function( g )
    return false;
end );

# Added ml 5/02/2013 User friendly method
#############################################################################
#O  IsCorrelation( <g> )
# 
## 
InstallMethod( IsCorrelation, [ IsProjGrpEl ],
  function( g )
    return false;
end );

 
  
# Added ml 7/11/2012
#############################################################################
#O  IsProjectivity( <g> )
# method to check if a given ProjGrpElWithFrobWithPSIsom is a projectivity,
# i.e. if the corresponding frobenius automorphism and the psisom is the identity
##
InstallMethod( IsProjectivity, 
	"for objects in IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep",
	[ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	g -> IsOne(g!.frob) and IsOne(g!.psisom) );
  
#  function( g )
#    local F,sigma,delta;
#                F:=g!.fld;
#                sigma:=g!.frob;
#                delta:=g!.psisom;
#                if sigma = FrobeniusAutomorphism(F)^0 and IsIdentityMappingOfElementsOfProjectiveSpace(delta)
#                then return true;
#                else return false;
#                fi;
#  end );


# Added ml 8/11/2012 # changed ml 28/11/2012
#############################################################################
#O  IsStrictlySemilinear( <g> )
# method to check if a given ProjGrpElWithFrobWithPSIsom is a projective semilinear map,
# i.e. if the corresponding frobenius automorphism is NOT the identity ##
InstallMethod( IsStrictlySemilinear,
	"for objects in IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep",
	[ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	g -> not IsOne(g!.frob) );

#function( g )
#    local F,sigma,delta;
#                F:=g!.fld;
#                sigma:=g!.frob;
#                delta:=g!.psisom;
#                if sigma <> FrobeniusAutomorphism(F)^0
#                then return true;
#                else return false;
#                fi;
#  end );



###################################################################
# Tests whether CorrelationCollineationGroup is a ProjectivityGroup and so on ...
###################################################################

# Added ml 8/11/2012
#############################################################################
#O  IsProjectivityGroup( <G> )
# method to check if a given CorrelationCollineationGroup G is a projectivity group, 
# i.e. if the corresponding frobenius automorphisms of the generators are the identity
# and the corresponding projective isomorphisms of the generators are the identity
## 
InstallMethod( IsProjectivityGroup, [ IsProjGroupWithFrobWithPSIsom ],
  function( G )
    local gens, F, d, set, g, set2;
		gens:=GeneratorsOfMagmaWithInverses(G);
		F:=gens[1]!.fld;
		d:=NrRows(gens[1]!.mat)-1;
		set:=AsSet(List(gens,g->g!.frob));
		set2:=AsSet(List(gens,g->g!.psisom));
		if set = AsSet([FrobeniusAutomorphism(F)^0]) and 
				set2 = AsSet([IdentityMappingOfElementsOfProjectiveSpace(PG(d,F))])
		then return true;
		else return false;
		fi;
  end );


# Added ml 8/11/2012
#############################################################################
#O  IsCollineationGroup( <G> )
##
InstallMethod( IsCollineationGroup, [ IsProjGroupWithFrobWithPSIsom ],
  function( G )
    local gens, F, d, set, g, set2;
		gens:=GeneratorsOfMagmaWithInverses(G);
		F:=gens[1]!.fld;
		d:=NrRows(gens[1]!.mat)-1;
		set2:=AsSet(List(gens,g->g!.psisom));
		if set2 = AsSet([IdentityMappingOfElementsOfProjectiveSpace(PG(d,F))])
		then return true;
		else return false;
		fi;
  end );


###################################################################
# ViewObj/Print/Display methods
###################################################################
# CHECKED 14/09/11 jdb

InstallMethod( ViewObj,
	"for such a nice looking standard duality of a projective space",
	[IsStandardDualityOfProjectiveSpace and IsSPMappingByFunctionWithInverseRep],
	function(delta)
		Print("StandardDuality( ",Source(delta)," )");
	end);

InstallMethod( Display,
	"for such a nice looking standard duality of a projective space",
	[IsStandardDualityOfProjectiveSpace and IsSPMappingByFunctionWithInverseRep],
	function(delta)
		Print("StandardDuality( ",Source(delta)," )");
	end);

InstallMethod( PrintObj,
	"for such a nice looking standard duality of a projective space",
	[IsStandardDualityOfProjectiveSpace and IsSPMappingByFunctionWithInverseRep],
	function(delta)
		Print("StandardDuality( ",Source(delta)," )");
	end);

###################################################################
# multiplying projective space isomorphisms. Actually, these methods
# are not intended for the user, since there is no check to see
# if the arguments are projective space isomorphisms of the same
# projective space. On the other hand, the user should only use these 
# objects as an argument for constructor functions of corrlelations. 
# The methods here are in the first place helper methods for the methods
# multiplying correlations. So we allow ourselves here some sloppyness.
###################################################################

# CHECKED 14/09/11 jdb
#############################################################################
#O  \*( <delta1>, <delta2> )
# returns the product of two standard dualities of a projective space.
## 
InstallMethod( \*,
	"for multiplying a standard-duality of a projective space",
	[IsStandardDualityOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
	function(delta1,delta2)
		return One(delta1);
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \*( <delta1>, <delta2> )
# returns the product of the identitymap and the standard duality of a projective space.
## 
InstallMethod( \*,
	"for multiplying a standard-duality of a projective space",
	[IsIdentityMappingOfElementsOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
	function(delta1,delta2)
		return delta2;
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \*( <delta1>, <delta2> )
# returns the product of the standard duality and the identitymap of a projective space.
## 
InstallMethod( \*,
	"for multiplying a standard-duality of a projective space",
	[IsStandardDualityOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace],
	function(delta1,delta2)
		return delta1;
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \*( <delta1>, <delta2> )
# returns the product of two identitymaps of a projective space.
## 
InstallMethod( \*,
	"for multiplying a standard-duality of a projective space",
	[IsIdentityMappingOfElementsOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace],
	function(delta1,delta2)
		return delta1;
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \^( <delta1>, 0 )
# returns One(delta1), <delta1> a projective space isomorphism
## 
InstallMethod( \^,
    "for psisom and zero",
    [ IsProjectiveSpaceIsomorphism, IsZeroCyc ],
    function(delta,p)
		return One(delta);
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \=( <delta1>, <delta2> )
# returns true iff <delta1>=<delta2>, two standard dualities.
## 
InstallMethod( \=,
	"for two standard-dualities of a projective space",
	[IsStandardDualityOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
	function(delta,eta)
		return Source(delta) = Source(eta);
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \=( <delta1>, <delta2> )
# returns false, since a duality is not the identitymap.
InstallMethod( \=,
	"for a standard-duality of a projective space and the identitymap",
	[IsStandardDualityOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace],
	function(delta,eta)
		return false;
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \=( <delta1>, <delta2> )
# returns false, since a duality is not the identitymap.
InstallMethod( \=,
	"for the identity map and a standard-duality of a projective space",
	[IsIdentityMappingOfElementsOfProjectiveSpace, IsStandardDualityOfProjectiveSpace],
	function(delta,eta)
		return false;
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \=( <delta1>, <delta2> )
# returns true iff <delta1>=<delta2>, two identitymaps of a projective space.
## 
InstallMethod( \=,
	"for two identity maps of a projective space",
	[IsIdentityMappingOfElementsOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace],
	function(delta,eta)
		return Source(delta) = Source(eta);
	end);

###################################################################
# Constructor methods for "projective elements with frobenius with 
# vector space isomorphism", 
# Such an object represents a collineation (delta = identity) OR 
# a correlation (delta = standard duality).
###################################################################


# CHECKED 14/09/11 jdb
# cmat changed 19/3/14
#############################################################################
#O  ProjElWithFrobWithPSIsom( <mat>, <frob>, <f>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom,
# i.e. correlations. This method is not intended for the users, it has no 
# checks built in. the fourth argument must be the standard duality of a projective
# space.
##
InstallMethod( ProjElWithFrobWithPSIsom, 
	"for a cmat/ffe matrix, a Frobenius automorphism, a field and the st. duality",
	[IsCMatRep and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsStandardDualityOfProjectiveSpace],
	function( m, frob, f, delta )
		local el;
		el := rec( mat := m, fld := f, frob := frob, psisom := delta );
		Objectify( ProjElsWithFrobWithPSIsomType, el );
		return el;
	end );


#added 19/3/14
#############################################################################
#O  ProjElWithFrobWithPSIsom( <mat>, <frob>, <f>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom,
# i.e. correlations. This method is not intended for the users, it has no 
# checks built in. the fourth argument must be the standard duality of a projective
# space.
##
InstallMethod( ProjElWithFrobWithPSIsom, 
	"for a ffe matrix, a Frobenius automorphism, a field and the st. duality",
	[IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsStandardDualityOfProjectiveSpace],
	function( m, frob, f, delta )
		local el,cmat;
		cmat := NewMatrix(IsCMatRep,f,NrCols(m),m);
		el := rec( mat := cmat, fld := f, frob := frob, psisom := delta );
		Objectify( ProjElsWithFrobWithPSIsomType, el );
		return el;
	end );


# CHECKED 14/09/11 jdb
# cmat changed 19/3/14
#############################################################################
#O  ProjElWithFrobWithPSIsom( <mat>, <frob>, <f> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom,
# i.e. correlations. This method is not intended for the users, it has no 
# checks built in. There is no fourth argument, the projective space isomorphism
# will be the identity mapping of the projective space.
##
InstallMethod( ProjElWithFrobWithPSIsom, 
	"for a cmat/ffe matrix, a Frobenius automorphism and a field",
	[IsCMatRep and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField],
	function( m, frob, f )
		local el,isom,n;
		n := NrRows(m);
		isom := IdentityMappingOfElementsOfProjectiveSpace(ProjectiveSpace(n-1,f));  
		## I hope this works! was wrong, for godsake, don't tell Celle about this type of mistakes :-(
		el := rec( mat := m, fld := f, frob := frob, psisom := isom);
		Objectify( ProjElsWithFrobWithPSIsomType, el );
		return el;
	end );
	
# added 19/3/14
#############################################################################
#O  ProjElWithFrobWithPSIsom( <mat>, <frob>, <f> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom,
# i.e. correlations. This method is not intended for the users, it has no 
# checks built in. There is no fourth argument, the projective space isomorphism
# will be the identity mapping of the projective space.
##
InstallMethod( ProjElWithFrobWithPSIsom, 
	"for a ffe matrix, a Frobenius automorphism and a field",
	[IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField],
	function( m, frob, f )
		local el,isom,n,cmat;
		n := NrRows(m);
		cmat := NewMatrix(IsCMatRep,f,NrCols(m),m);
		isom := IdentityMappingOfElementsOfProjectiveSpace(ProjectiveSpace(n-1,f));  
		## I hope this works! was wrong, for godsake, don't tell Celle about this type of mistakes :-(
		el := rec( mat := cmat, fld := f, frob := frob, psisom := isom);
		Objectify( ProjElsWithFrobWithPSIsomType, el );
		return el;
	end );


# CHECKED 14/09/11 jdb
# cmat changed 19/3/14
#############################################################################
#O  ProjElWithFrobWithPSIsom( <mat>, <frob>, <f>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom,
# i.e. correlations. This method is not intended for the users, it has no 
# checks built in. The fourth argument must be the identity mapping of a projective space.
##
InstallMethod( ProjElWithFrobWithPSIsom, 
	"for a cmat/ffe matrix and a Frobenius automorphism, a field and the identity mapping",
	[IsCMatRep and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsGeneralMapping	and IsSPGeneralMapping and IsOne],
	function( m, frob, f, delta )
		local el;
		el := rec( mat := m, fld := f, frob := frob, psisom := delta );
		Objectify( ProjElsWithFrobWithPSIsomType, el );
		return el;
	end );
	
# added 19/3/14
#############################################################################
#O  ProjElWithFrobWithPSIsom( <mat>, <frob>, <f>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom,
# i.e. correlations. This method is not intended for the users, it has no 
# checks built in. The fourth argument must be the identity mapping of a projective space.
##
InstallMethod( ProjElWithFrobWithPSIsom, 
	"for a ffe matrix and a Frobenius automorphism, a field and the identity mapping",
	[IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse,
	  IsField, IsGeneralMapping	and IsSPGeneralMapping and IsOne],
	function( m, frob, f, delta )
		local el,cmat;
		cmat := NewMatrix(IsCMatRep,f,NrCols(m),m);
		el := rec( mat := cmat, fld := f, frob := frob, psisom := delta );
		Objectify( ProjElsWithFrobWithPSIsomType, el );
		return el;
	end );


###################################################################
# Viewing, displaying, printing methods.
###################################################################
# CHECKED 14/09/11 jdb

InstallMethod( ViewObj, 
	"for a projective group element with Frobenius with duality",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function(el)
		Print("<projective element with Frobenius with projectivespace isomorphism: ");
		ViewObj(el!.mat);
		if IsOne(el!.frob) then
			Print(", F^0, ");
		else
			Print(", F^",el!.frob!.power,", ");
		fi;
		ViewObj(el!.psisom);
		Print(" >");
	end);

InstallMethod( Display, 
	"for a projective group element with Frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function(el)
		Print("<projective element with Frobenius, underlying matrix, \n");
		Print(el!.mat);
		if IsOne(el!.frob) then
			Print(", F^0");
		else
			Print(", F^",el!.frob!.power);
		fi;
		Print(", underlying projective space isomorphism:\n",el!.psisom,">\n");
	end );

InstallMethod( PrintObj, 
	"for a projective group element with Frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function(el)
		Print("ProjElWithFrobWithPSIsom(");
		PrintObj(el!.mat);
		Print(",");
		PrintObj(el!.frob);
		Print(",");
		PrintObj(el!.psisom);
		Print(")");
	end );

###################################################################
# Some operations for correlations and correlation-collineation groups.
###################################################################

# CHECKED 14/09/11 jdb
#############################################################################
#O  Representative( <el> )
# returns the underlying matrix, field automorphism and projective space isomorphism
# of the correlation <el>
##
InstallOtherMethod( Representative, 
	"for a projective group element with Frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( el )
		return [el!.mat,el!.frob,el!.psisom];
	end );

# CHECKED 14/09/11 jdb
#############################################################################
#O  BaseField( <el> )
# returns the underlying field of the correlation <el> 
##  
InstallMethod( BaseField,
	"for a projective group element with Frobenius with proj space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( el )
		return el!.fld;
	end );

# CHECKED 14/09/11 jdb
#############################################################################
#O  BaseField( <g> )
# returns the base field of the correlation-collineation group group <g>
## 
InstallMethod( BaseField, "for a projective group with Frobenius with proj space isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
  function( g )
    local f,gens;
    if IsBound(g!.basefield) then
        return g!.basefield;
    fi;
    if HasParent(g) then
        f := BaseField(Parent(g));
        g!.basefield := f;
        return f;
    fi;
    # Now start to investigate:
    gens := GeneratorsOfGroup(g);
    if Length(gens) > 0 then
        g!.basefield := gens[1]!.fld;
        return g!.basefield;
    fi;
    # Now we have to give up:
    Error("base field could not be determined");
  end );

###################################################################
# code for multiplying, comparing... correlations with themselves and 
# other elements, and more operations.
###################################################################

# CHECKED 15/09/11 jdb
# small change 19/3/14 
#############################################################################
#O  \=( <a>, <b> )
# returns true iff the correlations <a> and <b> are the same.
##
InstallMethod( \=, 
	"for two projective group els with Frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
	IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( a, b )
		local aa,bb,p,s,i;
		if a!.fld <> b!.fld then Error("different base fields"); fi;
		if a!.frob <> b!.frob then return false; fi;
		aa := a!.mat;
		bb := b!.mat;
		p := PositionNonZero(aa[1]);
		s := bb[1][p] / aa[1][p];
        if s*aa <> bb then return false; fi; #small change
		#for i in [1..Length(aa)] do
		#	if s*aa[i] <> bb[i] then return false; fi;
		#od;
		if a!.psisom <> b!.psisom then return false; fi;
		return true;
	end );

# CHECKED 15/09/11 jdb
#############################################################################
#O  IsOne( <a> )
# returns true if the correlation <a> is the identity.
##
InstallMethod( IsOne, 
  "for a projective group elm with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local s;
    if not(IsOne(el!.frob)) then return false; fi;
    if not(IsOne(el!.psisom)) then return false; fi;
    s := el!.mat[1][1];
    if IsZero(s) then return false; fi;
    s := s^-1;
    return IsOne( s*el!.mat );
  end );

# CHECKED 15/09/11 jdb
#############################################################################
#O  OneImmutable( <el> )
# returns immutable one of the group the correlation <el>
## 
InstallOtherMethod( OneImmutable, 
	"for a projective group elm with Frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( el )
		local o;
		o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
				frob := el!.frob^0, psisom := el!.psisom^0);
		Objectify( ProjElsWithFrobWithPSIsomType, o);
		return o;
	end );

# CHECKED 15/09/11 jdb
#############################################################################
#O  OneImmutable( <g> )
# returns immutable one of the group the correlation group <g>
## 
InstallOtherMethod( OneImmutable, 
	"for a projective group with Frobenius with projective space isomorphism",
	[IsGroup and IsProjGrpElWithFrobWithPSIsom],
	function( g )
		local gens, o;
		gens := GeneratorsOfGroup(g);
		if Length(gens) = 0 then
			if HasParent(g) then
				gens := GeneratorsOfGroup(Parent(g));
			else
				Error("sorry, no generators, no one");
			fi;
		fi;
		o := rec( mat := OneImmutable( gens[1]!.mat ), fld := BaseField(g),
				frob := gens[1]!.frob^0, psisom := gens[1]!.psisom^0 );
		Objectify( ProjElsWithFrobWithPSIsomType, o);
		return o;
	end );

# CHECKED 15/09/11 jdb
#############################################################################
#O  OneSameMutability( <el> )
# returns one of the group of <el> with same mutability of <el>.
## 
InstallMethod( OneSameMutability, 
	"for a projective group element with Frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( el )
		local o;
		o := rec( mat := OneImmutable( el!.mat ), fld := el!.fld,
				frob := el!.frob^0, psisom := el!.psisom^0 );
		Objectify( ProjElsWithFrobWithPSIsomType, o);
		return o;
	end );


###################################################################
# The things that make it a group :-) 
###################################################################
# we first need: 
###################################
# Methods for \^ (standard duality)
###################################
#7 CHECKED 15/09/11 jdb

# added the cvec one 20/3/14.
InstallOtherMethod( \^, "for a cvec/FFE vector and a id. mapping of el. of ps.",
  [ IsCVecRep and IsFFECollection, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, "for a FFE vector and a id. mapping of el. of ps.",
  [ IsVector and IsFFECollection, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a compressed GF2 vector and a id. mapping of el. of ps.",
  [ IsVector and IsFFECollection and IsGF2VectorRep, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

InstallOtherMethod( \^, 
  "for a compressed 8bit vector and a id. mapping of el. of ps.",
  [ IsVector and IsFFECollection and Is8BitVectorRep, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( v, f )
    return v;
  end );

#added the cmat one 20/3/14.
InstallOtherMethod( \^, "for a FFE matrix and a st. duality",
  [ IsCMatRep and IsFFECollColl, IsStandardDualityOfProjectiveSpace ],
  function( m, f )
    return TransposedMat(Inverse(m));
  end );

InstallOtherMethod( \^, "for a FFE matrix and a st. duality",
  [ IsMatrix and IsFFECollColl, IsStandardDualityOfProjectiveSpace ],
  function( m, f )
    return TransposedMat(Inverse(m));
  end );

#added the cmat one 20/3/14.
InstallOtherMethod( \^, "for a FFE matrix and a id. mapping of el. of ps.",
  [ IsCMatRep and IsFFECollColl, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( m, f )
    return m;
  end );
  
InstallOtherMethod( \^, "for a FFE matrix and a id. mapping of el. of ps.",
  [ IsMatrix and IsFFECollColl, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( m, f )
    return m;
  end );

InstallOtherMethod( \^, "for an element of a projective space and a id. mapping of el. of ps.",
  [ IsSubspaceOfProjectiveSpace, IsIdentityMappingOfElementsOfProjectiveSpace ],
  function( p, f)
    return p;
  end );

InstallOtherMethod( \^, "for an elements of a projective space and a st. duality",
  [ IsSubspaceOfProjectiveSpace, IsStandardDualityOfProjectiveSpace ],
  function( p, f)
    return f!.fun(p);
  end );

# CHECKED 15/09/11 jdb
#############################################################################
#O  \*( <a>, <b> )
# returns a*b, for two correlations <a> and <b>
## 
InstallMethod( \*, 
	"for two projective group elements with frobenius with projective space isomorphism",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
	IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( a, b )
		local el;  
		el := rec( mat := a!.mat * ((b!.mat^(a!.frob^-1)))^a!.psisom, fld := a!.fld, 
				#O Celle, dear friend, you missed this correction :-)
				#O True Jan, but I can't see everything, hey :-)
				frob := a!.frob * b!.frob, psisom := a!.psisom * b!.psisom );
		Objectify( ProjElsWithFrobWithPSIsomType, el);
		return el;
	end );

# CHECKED 14/09/11 jdb
#############################################################################
#O  \<( <a>, <b> )
# method for LT, for two correlations.
## 
InstallMethod(\<, 
	"for two correlations",
	[IsProjGrpElWithFrobWithPSIsom, IsProjGrpElWithFrobWithPSIsom],
	function(a,b)
		local aa,bb,pa,pb,sa,sb,i,va,vb;
		if a!.fld <> b!.fld then Error("different base fields"); fi;
		if a!.psisom < b!.psisom then
			return true;
		elif b!.psisom < a!.psisom then
			return false;
		fi;
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
		sa := aa[1][pa]^-1;
		sb := bb[1][pb]^-1;
		for i in [1..Length(aa)] do
			va := sa*aa[i];
			vb := sb*bb[i];
			if va < vb then return true; fi;
			if vb < va then return false; fi;
		od;
		return false;
	end);

## Let M be a matrix, f be a Frobenius aut, and t be a psisom.
## Then the inverse of Mft is 
##        t^-1 f^-1 M^-1 = (M^-1)^(ft) f^-1 t^-1

# CHECKED 19/09/11 jdb
#############################################################################
#O  InverseSameMutability( <el> )
# returns el^-1, for IsProjGrpElWithFrobWithPSIsom, keeps mutability (of matrix).
## 
InstallMethod( InverseSameMutability, 
  "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local m, f, t;
    f := el!.frob^-1;
    t := el!.psisom;
    m := rec( mat := ((InverseSameMutability(el!.mat))^f)^t, fld := el!.fld,
              frob := f, psisom := t);
    Objectify( ProjElsWithFrobWithPSIsomType, m );
    return m;
  end );

# CHECKED 19/09/11 jdb
#############################################################################
#O  InverseMutable( <el> )
# returns el^-1 (mutable) for IsProjGrpElWithFrobWithPSIsom
## 
InstallMethod( InverseMutable, 
  "for a projective group element with Frobenius with projective space isomorphism",
  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
  function( el )
    local m, f, t;
    f := el!.frob;
    t := el!.psisom;
    m := rec( mat := ((InverseMutable(el!.mat))^f)^t, fld := el!.fld,
              frob := f^-1, psisom := t );
    Objectify( ProjElsWithFrobWithPSIsomType, m );
    return m;
  end );

#############################################################################################
# Methods for \* to multiply IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrob
#############################################################################################

#the following method is wrong, and is btw never used.
#InstallMethod( \*, 
#  "for two projective group elements with frobenius with projective space isomorphism",
#  [IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
#   IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
#  function( a, b )
#    local el;  
#    el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1)))^a!.psisom, fld := a!.fld, 
#               frob := a!.frob * b!.frob, psisom := a!.psisom * b!.psisom );
#    Objectify( ProjElsWithFrobWithPSIsomType, el);
#	return el;
#  end );

InstallMethod( \*, 
	"for a projective group element with frobenius and a correlation",
	[IsProjGrpElWithFrob and IsProjGrpElWithFrobRep,
	IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep],
	function( a, b )
		local el;  
		el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1))), fld := a!.fld, 
				frob := a!.frob * b!.frob, psisom := b!.psisom );
		Objectify( ProjElsWithFrobWithPSIsomType, el);
		return el;
	end );

InstallMethod( \*, 
	"for a correlation and a projective group element with frobenius",
	[IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep,
	IsProjGrpElWithFrob and IsProjGrpElWithFrobRep],
	function( a, b )
		local el;  
		el := rec( mat := (a!.mat * (b!.mat^(a!.frob^-1)))^a!.psisom, fld := a!.fld, 
				frob := a!.frob * b!.frob, psisom := a!.psisom  );
		Objectify( ProjElsWithFrobWithPSIsomType, el);
		return el;
	end );

#####################################################################
# Methods to construct a collection of ProjElsWithFrobWithPSIsom
#####################################################################

# CHECKED 19/09/11 jdb
#############################################################################
#O  ProjElsWithFrobWithPSIsom( <l>, <f> )
# method to construct a list of objects in the category IsProjGrpElWithFrobWithPSIsom,
# using a list of triples of matrix/frobenius automorphism/projective space isomorphism, and a field.
# This method relies of ProjElWithFrobWithPSIsom, and is not inteded for the user.
# no checks are built in. This could result in e.g. the use of a field that is 
# not compatible with (some of) the matrices, and result in a non user friendly 
# error
##
InstallMethod( ProjElsWithFrobWithPSIsom,
	"for a list of triples of ffe matrice + frob aut + st duality, and a field",
	[IsList, IsField],
	function( l, f )
		local objectlist, m;
		objectlist := [];
		for m in l do
			Add(objectlist, ProjElWithFrobWithPSIsom(m[1],m[2],f,m[3]));
		od;
		return objectlist;
	end );

# CHECKED 19/09/2011 jdb # changed 02/11/2012 ml
#############################################################################
#A  CorrelationCollineationGroup( <ps> )
# returns the group of collineations and correlations of the projective space <ps>
##
InstallMethod( CorrelationCollineationGroup, 
	"for a full projective space",
	[ IsProjectiveSpace and IsProjectiveSpaceRep ],
	function( ps )
		local corr,d,f,frob,g,newgens,q,tau,pow,string;
		f := ps!.basefield;
		q := Size(f);
		d := ProjectiveDimension(ps);
		g := GL(d+1,f);
		frob := FrobeniusAutomorphism(f);
		tau := StandardDualityOfProjectiveSpace(ps);
		newgens := List(GeneratorsOfGroup(g),x->[x,frob^0,tau^0]);
		Add(newgens,[One(g),frob,tau^0]);
		Add(newgens,[One(g),frob^0,tau]);
		newgens := ProjElsWithFrobWithPSIsom(newgens, f);
		corr := GroupWithGenerators(newgens);
		SetSize(corr, Size(g) / (q - 1) * Order(frob) * 2); #* 2 for the standard duality.
	    pow := DegreeOverPrimeField(f);
		if pow > 1 then 
			string := Concatenation("The FinInG correlation-collineation group PGammaL(",String(d+1),",",String(q),")");
		else
			string := Concatenation("The FinInG correlation-collineation group PGL(",String(d+1),",",String(q),")");
		fi;
		string := Concatenation(string," : 2");
		SetName(corr,string);
		return corr;
	end );

#####################################################################
# User friendly Methods to construct a correlation of a projective space.
#####################################################################

# CHECKED 19/09/11 jdb
#############################################################################
#O  CorrelationOfProjectiveSpace( <mat>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom, i.e. a 
# correlation of a projective space. This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrobWithPSIsom,
# the field automorphism and the projective space automorphism will be trivial
## 
InstallMethod( CorrelationOfProjectiveSpace, 
	"for a matrix and a finite field",
	[ IsMatrix and IsFFECollColl, IsField],
	function( mat, gf )
		if Rank(mat) <> NrRows(mat) then
			Error("<mat> must not be singular");
		fi;
		return ProjElWithFrobWithPSIsom( mat, IdentityMapping(gf), gf);
	end );

# CHECKED 19/09/11 jdb
#############################################################################
#O  CorrelationOfProjectiveSpace( <mat>, <frob>, <gf> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom, i.e. a 
# correlation of a projective space. This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrobWithPSIsom,
# the projective space automorphism will be trivial
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a matrix, a frobenius automorphism, and a finite field",
	[ IsMatrix and IsFFECollColl, IsRingHomomorphism and
    IsMultiplicativeElementWithInverse, IsField], 
	function( mat, frob, gf )
		if Rank(mat) <> NrRows(mat) then
			Error("<mat> must not be singular");
		fi;
		return ProjElWithFrobWithPSIsom( mat, frob, gf);
	end );

# CHECKED 19/09/11 jdb
#############################################################################
#O  CorrelationOfProjectiveSpace( <mat>, <gf>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom, i.e. a 
# correlation of a projective space. This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrobWithPSIsom,
#
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a matrix, a finite field, and a projective space isomorphism",
	[ IsMatrix and IsFFECollColl, IsField, IsStandardDualityOfProjectiveSpace], 
	function( mat, gf, delta )
		if Rank(mat) <> NrRows(mat) then
			Error("<mat> must not be singular");
		fi;
		if delta!.ps!.basefield <> gf then
			Error("<delta> is not a duality of the correct projective space");
		fi;
		return ProjElWithFrobWithPSIsom( mat, IdentityMapping(gf), gf, delta);
	end );

# Added ml 8/11/12
#############################################################################
#O  CorrelationOfProjectiveSpace( <mat>, <gf>, <delta> )
# 
# same method as above but now for delta equal to IsIdentityMappingOfElementsOfProjectiveSpace
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a matrix, a finite field, and a projective space isomorphism",
	[ IsMatrix and IsFFECollColl, IsField, IsIdentityMappingOfElementsOfProjectiveSpace], 
	function( mat, gf, delta )
		if Rank(mat) <> NrRows(mat) then
			Error("<mat> must not be singular");
		fi;
		if Source(delta)!.geometry <> PG(NrRows(mat)-1,gf) then
			Error("<delta> is not the identity mapping of the correct projective space");
		fi;
		return ProjElWithFrobWithPSIsom( mat, IdentityMapping(gf), gf, delta);
	end );

# CHECKED 19/09/11 jdb
#############################################################################
#O  CorrelationOfProjectiveSpace( <mat>, <frob>, <gf>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom, i.e. a 
# correlation of a projective space. This method is intended for the user, and contains
# a check whether the matrix is non-singular. The method relies on ProjElWithFrobWithPSIsom.
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a matrix, a frobenius automorphism, a finite field, and a projective space isomorphism",
	[ IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse, IsField, 
    IsStandardDualityOfProjectiveSpace], 
	function( mat, frob, gf, delta )
		if Rank(mat) <> NrRows(mat) then
			Error("<mat> must not be singular");
		fi;
		if delta!.ps!.basefield <> gf then
			Error("<delta> is not a duality of the correct projective space");
		fi;
		return ProjElWithFrobWithPSIsom( mat, frob, gf, delta);
	end ); 

# Added ml 8/11/12
#############################################################################
#O  CorrelationOfProjectiveSpace( <mat>, <frob>, <gf>, <delta> )
# 
# same method as above but now for delta equal to IsIdentityMappingOfElementsOfProjectiveSpace
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a matrix, a frobenius automorphism, a finite field, and a projective space isomorphism",
	[ IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse, IsField, 
    IsIdentityMappingOfElementsOfProjectiveSpace], 
	function( mat, frob, gf, delta )
		if Rank(mat) <> NrRows(mat) then
			Error("<mat> must not be singular");
		fi;
		if Source(delta)!.geometry <> PG(NrRows(mat)-1,gf) then
			Error("<delta> is not the identity mapping of the correct projective space");
		fi;
		return ProjElWithFrobWithPSIsom( mat, frob, gf, delta);
	end ); 

# Added ml 8/11/12
#############################################################################
#O  CorrelationOfProjectiveSpace( <pg>, <mat>, <frob>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom, i.e. a 
# correlation of a projective space. This method uses CorrelationOfProjectiveSpace
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a projective space, a matrix, a field automorphism, and a projective space isomorphism",
	[ IsProjectiveSpace, IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse, 
    IsStandardDualityOfProjectiveSpace], 
	function( pg, mat, frob, delta )
		if Dimension(pg)+1 <> NrRows(mat) then
			Error("The arguments <pg> and <mat> are not compatible");
		fi;
		return CorrelationOfProjectiveSpace( mat, frob, BaseField(pg), delta);
	end ); 

# Added ml 8/11/12
#############################################################################
#O  CorrelationOfProjectiveSpace( <pg>, <mat>, <frob>, <delta> )
# 
# same method as above but now for delta equal to IsIdentityMappingOfElementsOfProjectiveSpace
## 
InstallMethod( CorrelationOfProjectiveSpace,
	"for a projective space, a matrix, a field automorphism, and a projective space isomorphism",
	[ IsProjectiveSpace, IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse, 
    IsIdentityMappingOfElementsOfProjectiveSpace], 
	function( pg, mat, frob, delta )
		if Dimension(pg)+1 <> NrRows(mat) then
			Error("The arguments <pg> and <mat> are not compatible");
		fi;
		if Source(delta)!.geometry <> pg then
			Error("<delta> is not the identity mapping of the correct projective space");
		fi;
		return CorrelationOfProjectiveSpace( mat, frob, BaseField(pg), delta);
	end ); 

# Added ml 8/11/12
#############################################################################
#O  Correlation( <pg>, <mat>, <frob>, <delta> )
# method to construct an object in the category IsProjGrpElWithFrobWithPSIsom, i.e. a 
# correlation of a projective space. The method uses CorrelationOfProjectiveSpace
## 
InstallMethod( Correlation,
	"for a projective space, a matrix, a field automorphism, and a projective space isomorphism",
	[ IsProjectiveSpace, IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse, 
    IsStandardDualityOfProjectiveSpace], 
	function( pg, mat, frob, delta )
		if Dimension(pg)+1 <> NrRows(mat) then
			Error("The arguments <pg> and <mat> are not compatible");
		fi;
		return CorrelationOfProjectiveSpace( mat, frob, BaseField(pg), delta);
	end ); 

# Added ml 8/11/12
#############################################################################
#O  Correlation( <pg>, <mat>, <frob>, <delta> )
# 
# same method as above but now for delta equal to IsIdentityMappingOfElementsOfProjectiveSpace
## 
InstallMethod( Correlation,
	"for a projective space, a matrix, a field automorphism, and a projective space isomorphism",
	[ IsProjectiveSpace, IsMatrix and IsFFECollColl, IsRingHomomorphism and IsMultiplicativeElementWithInverse, 
    IsIdentityMappingOfElementsOfProjectiveSpace], 
	function( pg, mat, frob, delta )
		if Dimension(pg)+1 <> NrRows(mat) then
			Error("The arguments <pg> and <mat> are not compatible");
		fi;
		if Source(delta)!.geometry <> pg then
			Error("<delta> is not the identity mapping of the correct projective space");
		fi;
		return CorrelationOfProjectiveSpace( mat, frob, BaseField(pg), delta);
	end ); 

###################################################################
# Some operations for correlations
###################################################################


# CHECKED 19/09/11 jdb
#############################################################################
#O  MatrixOfCorrelation( <c> )
# returns the underlying matrix of <c> 
##
InstallMethod( MatrixOfCorrelation, [ IsProjGrpElWithFrobWithPSIsom and 
                                   IsProjGrpElWithFrobWithPSIsomRep],
  c -> c!.mat );
  
# CHECKED 19/09/11 jdb
#############################################################################
#O  FieldAutomorphism( <c> )
# returns the underlying field automorphism of <c> 
##
InstallMethod( FieldAutomorphism, [ IsProjGrpElWithFrobWithPSIsom and 
                                    IsProjGrpElWithFrobWithPSIsomRep],
  c -> c!.frob );

# CHECKED 19/09/11 jdb
#############################################################################
#O  ProjectiveSpaceIsomorphism( <c> )
# returns the underlying projective space isomorphism of <c> 
##
InstallMethod( ProjectiveSpaceIsomorphism, [ IsProjGrpElWithFrobWithPSIsom and 
                                    IsProjGrpElWithFrobWithPSIsomRep],
  c -> c!.psisom );

#####################################################################
# Embedding from a collineation group into a correlation group.
#####################################################################

# CHECKED 19/09/11 jdb
#############################################################################
#O  Embedding( <group>, <corr> )
# returns an embedding from a collineation group into a correlation group,
# i.e. a group homomorphism (which is in fact trivial), from PG(amma)L(n+1,q) ->
# PG(amma)L(n+1,q) : 2
##
InstallOtherMethod( Embedding,
    "for a collineation group",
	[IsProjectiveGroupWithFrob, IsProjGroupWithFrobWithPSIsom],
	function(group,corr)
	local hom;
	if not ((BaseField(group)=BaseField(corr)) and (Dimension(group)=Dimension(corr))) then
		Error("Embedding not allowed, dimension and/or base field do not match");
	fi;
	hom :=  GroupHomomorphismByFunction(group,corr,
	y->ProjElWithFrobWithPSIsom(y!.mat,y!.frob,y!.fld),false,
	function(x)
		if IsOne(x!.psisom) then
			return ProjElWithFrob(x!.mat,x!.frob,x!.fld);
			Print("method prefun");
		else
			Error("<x> has no preimage");
		fi; 
	end);
	SetIsInjective(hom,true);
	return hom;
	end );

#####################################################################
# Actions
# We follow almost the same approach as in goup.gi. We  define the 
# action of a correlation on vectorspaces, but we our user function
# for actions does not rely on it, since it would cause more function
# calls. 
# Also recall that in group.gi we have implemented the methods that do algebraic
# stuff. The user methods, related to the projective geometry stuff, are 
# in projectivespace.gi. Here we have both parts, since this file
# is anyway related to projectivespace.gi
# 
#####################################################################

# CHECKED 19/09/11 jdb
#############################################################################
#F  OnProjPointsWithFrobWithPSIsom( <line>, <el> )
# computes <line>^<el> where this action is the "natural" one, and <line> represents
# a projective point. This function relies on the GAP function OnPoints, which represents
# the natural action of matrices on row *vectors* (So the result is *not* normalized, neither
# it is assumed that the input is normalized.
# Important: despite its natural name, this function is *not* intended for the user.
# <line>: just a row vector, representing a projective point.
# <el>: a correlation. 
# Important: since this function is just "doing the algebra", it returns a vector that
# must be interpreted as the coordinates of a hyperplane now. 
## 
InstallGlobalFunction( OnProjPointsWithFrobWithPSIsom,
	function( line, el )
		local vec,c;
		vec := OnPoints(line,el!.mat)^el!.frob;
		c := PositionNonZero(vec);
		if c <= Length( vec )  then
			if not(IsMutable(vec)) then
				vec := ShallowCopy(vec);
			fi;
			MultVector(vec,Inverse( vec[c] ));
		fi;
		return vec;
	end );

# CHECKED 19/09/11 jdb
#############################################################################
#F  OnProjSubspacesWithFrobWithPSIsom( <subspace>, <el> )
# computes <subspace>^<el> where this action is the "natural" one, and <subspace> represents
# a projective subspace. This function relies on the GAP action function
# OnRight, which computes the action of a matrix on a sub vector space. 
# Important: despite its natural name, this function is *not* intended for the user.
# <el>: a projective group element (so a projectivity, *not* a projective semilinear element.
##
InstallGlobalFunction( OnProjSubspacesWithFrobWithPSIsom,
	function( line, el )
		local vec,c;
		vec := OnRight(line,el!.mat)^el!.frob;
		if not(IsMutable(vec)) then
			vec := MutableCopyMat(vec);
		fi;
		TriangulizeMat(vec);
		return vec;
		#return EchelonMat(vec).vectors;
	end );

# CHECKED 20/09/11 jdb
# changed name 05/02/13 ml jdbOnProjSubspacesExtended
#############################################################################
#F  OnProjSubspacesExtended( <sub>, <el> )
# computes <sub>^<el>, where <sub> is an element of a projective space, and 
# <el> a correlation. This function is stand alone now, and hence we still have
# to do the canonization. Therefore we use VectorSpaceToElement.
##
InstallGlobalFunction( OnProjSubspacesExtended,
	function( sub, el )
		local vec,newsub,ps,ty,newvec,rk;
		vec := Unwrap(sub);
		ps := AmbientSpace(sub!.geo);
		ty := sub!.type;
		newvec := (vec*el!.mat)^el!.frob;
		if ty = 1 then
			newvec := [newvec];
		fi;       
		if not IsOne(el!.psisom) then
			newvec := NullspaceMat(TransposedMat(newvec));
		fi;
		rk := Rank(newvec);
		if rk = 1 then 
			newvec := newvec[1];
		fi;
		return VectorSpaceToElement(ps,newvec);
	end );	

# CHECKED 19/09/11 jdb
#############################################################################
#O  Dimension( <g> )
# returns the dimension of the correlation group <g>. The dimension of this 
# group is defined as the vector space dimension of the projective space  
# of which <g> was defined as a projective group, or, in other words, as the 
# size of the matrices.
## 
InstallMethod( Dimension, 
  "for a projective group with Frobenius with vspace isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
  function( g )
    local gens;
    if HasParent(g) then
        return Dimension(Parent(g));
    fi;
    # Now start to investigate:
    gens := GeneratorsOfGroup(g);
    if Length(gens) > 0 then
        return NrRows(gens[1]!.mat);
    fi;
    Error("dimension could not be determined");
  end );

# CHECKED 20/09/11 jdb
#############################################################################
#P  ActionOnAllPointsHyperplanes( <g> )
# returns the action of the correlation group <g> on the projective points
# an hyperplanes of the underlying projective space.
## 
InstallMethod( ActionOnAllPointsHyperplanes, 
	"for a correlation group",
	[ IsProjGroupWithFrobWithPSIsom ],
	function( pg )
		local a,d,f,o,orb,orb2, m, j, flip, vs, ps;
		f := BaseField(pg);
		d := Dimension(pg);
		ps := ProjectiveSpace(d-1,f);
		vs := f^d;
		o := One(f);
		orb := [];
		for m in vs do
			j := PositionNonZero(m);
			if j <= d and m[j] = o then
				Add(orb, m);
			fi;
		od;
		flip := function(j)
			local vec;
			vec := TriangulizedNullspaceMat(TransposedMat([j]));
			return vec;
			end;
		orb2 := [];
		for m in orb do
			Add(orb2, flip(m));
		od;
		orb := List(Concatenation(orb,orb2),x->VectorSpaceToElement(ps,x)); #corrected 12/4/11
		a := ActionHomomorphism(pg, orb,
				OnProjSubspacesExtended, "surjective");
		SetIsInjective(a,true);
		return a;
	end );

# CHECKED 20/09/11 jdb
#############################################################################
#P  CanComputeActionOnPoints( <g> )
# is set true if we consider the computation of the action feasible.
# for correlation groups.
## 
InstallMethod( CanComputeActionOnPoints, 
	"for a projective group with frob with pspace isomorphism",
	[IsProjGroupWithFrobWithPSIsom],
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

# CHECKED 20/09/11 jdb
#############################################################################
#O  NiceMonomorphism( <pg> )
# <pg> is a correlation group. This operation returns a nice monomorphism.
##
InstallMethod( NiceMonomorphism, 
	"for a projective group with Frobenius with pspace isomorphism (feasible case)",
	[IsProjGroupWithFrobWithPSIsom and CanComputeActionOnPoints and
	IsHandledByNiceMonomorphism], 50,
	function( pg )
		return ActionOnAllPointsHyperplanes( pg );
	end );
  
# CHECKED 20/09/11 jdb
#############################################################################
#O  NiceMonomorphism( <pg> )
# <pg> is a correlation group. This operation returns a nice monomorphism.
##
InstallMethod( NiceMonomorphism, 
	"for a projective group with Frobenius with pspace isomorphism (nasty case)",
	[IsProjGroupWithFrobWithPSIsom and IsHandledByNiceMonomorphism], 50,
	function( pg )
		local can;
		can := CanComputeActionOnPoints(pg);
		if not(can) then
			Error("action on projective points not feasible to calculate");
		else
			return ActionOnAllPointsHyperplanes( pg );
		fi;
	end );

# CHECKED 19/09/11 jdb
###################################################################
# View methods of groups of projective elements with frobenius with 
# projective space isomorphism
###################################################################

InstallMethod( ViewObj, 
  "for a projective group with frobenius with pspace isomorphism",
  [IsProjGroupWithFrobWithPSIsom],
  function( g )
    Print("<projective group with Frobenius with proj. space isomorphism>");
  end );

InstallMethod( ViewObj, 
  "for a trivial projective group with frobenius with pspace isomorphism ",
  [IsProjGroupWithFrobWithPSIsom and IsTrivial],
  function( g )
    Print("<trivial projective group with Frobenius with proj. space isomorphism>");
  end );

InstallMethod( ViewObj, 
  "for a projective group with Frobenius with proj. space isomorphism with gens",
  [IsProjGroupWithFrobWithPSIsom and HasGeneratorsOfGroup],
  function( g )
    local gens;
    gens := GeneratorsOfGroup(g);
    if Length(gens) = 0 then
        Print("<trivial projective group with Frobenius with proj. space isomorphism>");
    else
        Print("<projective group with Frobenius with proj. space isomorphism with ",Length(gens),
              " generators>");
    fi;
  end );

InstallMethod( ViewObj, 
  "for a projective group with frobenius with pspace isomorphism with size",
  [IsProjGroupWithFrobWithPSIsom and HasSize],
  function( g )
    if Size(g) = 1 then
        Print("<trivial projective group with Frobenius with proj. space isomorphism>");
    else
        Print("<projective group with Frobenius with proj. space isomorphism of size ",Size(g),">");
    fi;
  end );

InstallMethod( ViewObj, 
  "for a projective group with frobenius with pspace isomorphism with gens and size",
  [IsProjGroupWithFrobWithPSIsom and HasGeneratorsOfGroup and HasSize],
  function( g )
    local gens;
    gens := GeneratorsOfGroup(g);
    if Length(gens) = 0 then
        Print("<trivial projective group with Frobenius with proj. space isomorphism>");
    else
        Print("<projective group with Frobenius with proj. space isomorphism of size ",Size(g)," with ",
              Length(gens)," generators>");
    fi;
  end );


#############################################################################
# The polarity section of this file. 
#############################################################################

#############################################################################
# operations to create polarities of projective spaces 
#############################################################################

# operation to construct polarity of projective space using a sesquilinear form.
# to be considered as the standard.

InstallMethod(PolarityOfProjectiveSpaceOp,
  "for a sesquilinear form of the underlying vectorspace",
  [IsSesquilinearForm and IsFormRep],
  function(form)
  local mat,field,n,aut,v,ps,q,delta,polarity;
  if IsDegenerateForm(form) then
    Error("<form> must not be degenerate");
  fi;
  mat := GramMatrix(form);
  field := BaseField(form);
  n := NrRows(mat);
  aut := CompanionAutomorphism(form);
  v := field^n;
  ps := ProjectiveSpace(n-1,field);
  delta := StandardDualityOfProjectiveSpace(ps);
  polarity :=  ProjElWithFrobWithPSIsom(mat,aut,field,delta);
  polarity!.form := form;
  SetFilterObj( polarity, IsPolarityOfProjectiveSpace );
  SetFilterObj( polarity, IsPolarityOfProjectiveSpaceRep );
  return polarity;
end );

#############################################################################
# standard View, Display and Print operations 
#############################################################################

InstallMethod( ViewObj, 
  "for a polarity",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function(el)
  local dim, field;
  field := el!.fld;
  dim := NrRows(el!.mat);
  Print("<polarity of PG(", dim-1, ", ", field, ")");
  #ViewObj(el!.mat);
  #if IsOne(el!.frob) then
  #    Print(", F^0");
  #else
  #    Print(", F^",el!.frob!.power);
  #fi;
  Print(" >");
  end);

InstallMethod( PrintObj,
  "for a polarity",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function( f )
  local dim, field;
  field := f!.fld;
  dim := NrRows(f!.mat);
  Print("<polarity of PG(", dim-1, ", ", field, ")>, underlying matrix\n");
  PrintObj(f!.mat);
  Print(",");
  PrintObj(f!.frob);
  Print(") \n");
end );

InstallMethod( Display, "for a projective group element with Frobenius with projective space isomorphism",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  function(f)
    local dim, field;
    dim := NrRows(f!.mat);
    field := f!.fld;
    Print("<polarity of PG(", dim-1, ", ", field, ")>, underlying matrix\n");
    Display(f!.mat);
    if IsOne(f!.frob) then
        Print(", F^0");
    else
        Print(", F^",f!.frob!.power);
    fi;
    Print(">\n");
  end );

#############################################################################
# Constructor operations 
#############################################################################

#First user function to create polarity of projective space.
#using a form. Just calls ...Op version of this operation.

InstallMethod(PolarityOfProjectiveSpace,
  "for a sesquilinear form of the underlying vectorspace",
  [IsSesquilinearForm and IsFormRep],
  function(form)
  return PolarityOfProjectiveSpaceOp(form);
end );

InstallMethod(PolarityOfProjectiveSpace,
  "for a matrix and a finite field",
  [IsMatrix,IsField and IsFinite],
  function(matrix,field)
  local form;
  if Rank(matrix) <> NrRows(matrix) then
    Error("<matrix> must not be singular");
  fi;
  form := BilinearFormByMatrix(matrix,field);  
  return PolarityOfProjectiveSpaceOp(form);
end );

InstallMethod(PolarityOfProjectiveSpace,
  "for a matrix, a finite field, and a frobenius automorphism",
  [IsMatrix, IsFrobeniusAutomorphism, IsField and IsFinite],
  function(matrix,frob,field)
  local form;
  if Rank(matrix) <> NrRows(matrix) then
    Error("<matrix> must not be singular");
  fi;
  if Order(frob)<>2 then
    Error("<frob> must be involutory or identity");
  else
    form := HermitianFormByMatrix(matrix,field);
    return PolarityOfProjectiveSpaceOp(form);
  fi;
end );

InstallMethod(HermitianPolarityOfProjectiveSpace,
  "for a sesquilinear form of a projective space",
  [IsMatrix,IsField and IsFinite],
  function(matrix,field)
  local form;
  if Rank(matrix) <> NrRows(matrix) then
    Error("<matrix> must not be singular");
  fi;
  if IsOddInt(DegreeOverPrimeField(field)) then
    Error("Size of <field> must be a square" );
  fi;
  form := HermitianFormByMatrix(matrix,field);  
  return PolarityOfProjectiveSpaceOp(form);
end );

#############################################################################
# operations, attributes and properties
#############################################################################

InstallMethod( GramMatrix, "for a polarity of a projective space",
   [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   x -> x!.mat );

InstallOtherMethod( BaseField, "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   x -> x!.fld );

InstallMethod( CompanionAutomorphism, "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   x -> x!.frob );

InstallMethod( SesquilinearForm, "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
   el -> el!.form );


InstallMethod( IsHermitianPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsHermitianForm(x!.form) );

InstallMethod( IsOrthogonalPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsOrthogonalForm(x!.form) );

InstallMethod( IsSymplecticPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsSymplecticForm(x!.form) );

InstallMethod( IsPseudoPolarityOfProjectiveSpace,
  "for a polarity of a projective space",
  [IsPolarityOfProjectiveSpace and IsPolarityOfProjectiveSpaceRep],
  x -> IsPseudoForm(x!.form) );

#installs the method for sub^phi with sub a subspace of a projectivespace and
# phi a polarity of a projective space. To be discussed whether this should be done
# in general or only for polarities.
# uses OnProjSubspacesExtended, defined in group2.g*
InstallOtherMethod( \^,
  "for an element of a projective space and a polarity of a projective space",
  [ IsSubspaceOfProjectiveSpace, IsPolarityOfProjectiveSpace],
  function(sub,phi)
  return OnProjSubspacesExtended(sub,phi);
end );

#the following is obsolete now.
#InstallMethod( IsDegeneratePolarity, "for a polarity of a projective space",
#    [ IsPolarityOfProjectiveSpace ],
#  function( polarity )
#    local form;
#    form := Form( polarity );
#    return IsDegenerateForm( form );
#  end );

#InstallGlobalFunction( OnProjSubspacesExtended,
#  function( line, el )
#    local vec,c;
#    vec := (OnRight(line,el!.mat)^el!.frob)^el!.duality;
#    if not(IsMutable(vec)) then
#        vec := MutableCopyMat(vec);
#    fi;
#    TriangulizeMat(vec);
#    return vec;
#  end );
