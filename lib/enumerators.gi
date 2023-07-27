#############################################################################
##
##  enumerators.gi              FinInG package
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
##  Implementation stuff for enumerators of elements of polar spaces
##
#############################################################################
##  15/2/2016: We acknowledge Max Horn for sorting out and solving a problem
##  with some AsList methods, causing a problem with orb 4.7.5.
#############################################################################

#############################################################################
# The enumerator functionality is quite technical. We need many tiny and less
# tiny helping functions. We go for a bottom up approach, where we first 
# declare the helping functions, and then the real user stuff in the end.
#############################################################################

#############################################################################
# Very Low level help functions
# most of these (short) functions are self explanatory.
#############################################################################

#############################################################################
#F  PositionNonZeroFromRight( x )
# <x> a vector
##
InstallGlobalFunction( PositionNonZeroFromRight,
	function(x)
	local perm;
		perm := PermList( List([1..Length(x)],y->Length(x)-y+1) );
		return Length(x)-PositionNonZero( Permuted(x, perm) )+1;
	end );

#############################################################################
#F  FG_pos( q, x ): inverse function of FG_ffenumber
# q: prime power, x element of GF(q).
##
#InstallGlobalFunction( FG_pos,
#	function(q, x)
#		return Position(AsList(GF(q)), x) - 1;
#	end );
#
#  JB (10/04/2013): Changed this slightly, but I don't think it makes a difference.

InstallGlobalFunction( FG_pos,
		function(q, x)
			return Position(Enumerator(GF(q)), x) - 1;
		end );

#############################################################################
#F  FG_div(	a, b )
# a,b: integers
##
#InstallGlobalFunction( FG_div,
#	function(a, b)
#		return (a - (a mod b)) / b;
#	end );

#############################################################################
#F  FG_ffenumber( q, a )
# q: prime power, a: integer. Returns element number a, where element 0= Zero(GF(q))
# element a (<>0) = Z(q)^(a-1)
##
#InstallGlobalFunction( FG_ffenumber,
#	function(q, a)       
#		if a = 0 then 
#			return 0 * Z(q);
#		else 
#			return Z(q)^(a-1);
#		fi;
#	end );
#
#  JB (10/04/2013). First, happy birthday Jan! A simple change to the 
#  method FG_ffenumber now solves the problems with enumerators. Apparently
#  GAP stores the list of GF(q) elements as an attribute for GF(q) ... so we
#  can simply call Enumerator(GF(q)) for the correct ordering. Otherwise, if
#  this is slow, we could actually put the ordering by degree and exponent here.
#  For the moment, let us just make sure that it works.

InstallGlobalFunction( FG_ffenumber,
	function(q, a)       
		return Enumerator(GF(q))[a+1];
	end );



#the next function seems never used.
#############################################################################
#F  FG_unrank_GFQ( q, rk )
# q: prime power, rk: natural number
##
#InstallGlobalFunction( FG_unrank_GFQ,
#	function(q, rk)
#		#local Q;
#		#Q := q * q;
#		if rk = 0 then
#			return 0*Z(q^2);
#		fi;
#		return Z(q^2)^(rk-1);
#	end );

#the next function seems never used.
#############################################################################
#F  FG_rank_GFQ( q, x )
# q: prme power, x element of GF(q)
##
#InstallGlobalFunction( FG_rank_GFQ, 
#	function(q, x)
#		if IsZero(x) then
#			return 0;
#		else
#			return LogFFE(x,Z(q^2))+1;
#		fi;
#	end );

#############################################################################
#F  FG_alpha_power( q, a )
# q: prime power, a: integer
##	
InstallGlobalFunction( FG_alpha_power,
	function(q, a)
		return Z(q^2)^a;
	end );

#############################################################################
#F  FG_log_alpha( q, x )
# q: prime power, x element of GF(q^2)
##
InstallGlobalFunction( FG_log_alpha,
	function(q, x)   
		return LogFFE(x, Z(q^2));
	end );

#the next function seems never used.
#############################################################################
#F  FG_beta_power( q, a )
# q: prime power, a: integer
##	
InstallGlobalFunction( FG_beta_power,
	function(q, a)
		return Z(q^2)^((q + 1) * a);
	end );

#############################################################################
#F  FG_log_beta( q, x )
# q: prime power, x element in GF(q^2)
##
InstallGlobalFunction( FG_log_beta,
	function(q, x)
	#local Q;
	#Q := q * q;
		return LogFFE(x,Z(q^2)^(q+1));
	end );

#############################################################################
#F  FG_norm_one_element( q, a )
# q: prime power, a an integer
##
InstallGlobalFunction( FG_norm_one_element,
	function(q, a)
		return Z(q^2)^((q - 1) * a);
	end );

#############################################################################
#F  FG_index_of_norm_one_element( q, x )
# q: prime power, x an element of GF(q^2)
##
InstallGlobalFunction( FG_index_of_norm_one_element,
	function(q, x)
		return LogFFE(x,Z(q^2)^(q-1));
	end );

#############################################################################
#F PG_element_normalize( v, offset, n )
## This function takes the first nonzero element
## from the right (!) and normalises the vector v
## by this element.
##
InstallGlobalFunction( PG_element_normalize,
	function(v, offset, n)
	local ii, i, a, one;
		one := One(v[1]);	   
		for ii in [0..n - 1] do 
			i := n - 1 - ii;    
			a := v[offset + i]; 
			if not IsZero(a) then
				if IsOne(a) then  
					return;
				fi;
				a := a^-1;
				v{[offset..i-1+offset]} := a * v{[offset..i-1+offset]};
				v[offset + i] := one;
				return;
			fi;
		od;
		Error("zero vector");
	end );

#############################################################################
#F FG_evaluate_hyperbolic_quadratic_form( q, v, offset, n )
# a low level way for EvaluateForm
##
InstallGlobalFunction( FG_evaluate_hyperbolic_quadratic_form, 
  function(q, v, offset, n)
    local alpha, beta, u;	
    alpha := 0*Z(q);
    for u in [0..n-1] do
	 beta := v[offset + 2 * u] * v[offset + 2 * u + 1];
	 alpha := alpha + beta;
    od;
    return alpha;
end );

#############################################################################
#F FG_evaluate_hermitian_form( q, v, offset, n )
# a low level way for EvaluateForm
##
InstallGlobalFunction(FG_evaluate_hermitian_form, 
  function(q, v, offset, n)
    local alpha, beta, u, Q;
	Q := q * q;	
    alpha := 0*Z(Q);
    for u in [0..n-1] do
	 beta := v[offset + u]^(q + 1);
	 alpha := alpha + beta;
    od;
    return alpha;
end );

#############################################################################
# Anton's Enumerator:
#############################################################################

## counting functions

#############################################################################
#F  FG_nb_pts_Nbar( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_nb_pts_Nbar,  
  function(n, q)
	if n = 1 then
		return q - 1;
	fi;
  end );

#############################################################################
#F  FG_nb_pts_Nbar( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_nb_pts_S,
  function(n, q)
	local a;	
	if n = 1 then
		return 2 * q - 1;
	fi;
	a := FG_nb_pts_S(1, q) * FG_nb_pts_S(n - 1, q);
	a := a + FG_nb_pts_N(1, q) * FG_nb_pts_N1(n - 1, q);
	return a;
  end );

#############################################################################
#F  FG_nb_pts_N( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_nb_pts_N,
  function(n, q)
	local a;
	if n = 1 then
		return (q - 1) * (q - 1);
	fi;
	a := FG_nb_pts_S(1, q) * FG_nb_pts_N(n - 1, q);
	a := a + FG_nb_pts_N(1, q) * FG_nb_pts_S(n - 1, q);
	a := a + FG_nb_pts_N(1, q) * (q - 2) * FG_nb_pts_N1(n - 1, q);
	return a;
  end );

#############################################################################
#F  FG_nb_pts_N1( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_nb_pts_N1,
  function(n, q)
      local a;
	if n = 1 then
		return q - 1;
	fi;
	a := FG_nb_pts_S(1, q) * FG_nb_pts_N1(n - 1, q);
	a := a + FG_nb_pts_N1(1, q) * FG_nb_pts_S(n - 1, q);
	a := a + FG_nb_pts_N1(1, q) * (q - 2) * FG_nb_pts_N1(n - 1, q);
	return a;
  end );

#############################################################################
#F  FG_nb_pts_Sbar( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_nb_pts_Sbar,
  function(n, q)
	local a;
	if n = 1 then
		return 2;
	fi;
	a := FG_nb_pts_Sbar(n - 1, q);
	a := a + FG_nb_pts_Sbar(1, q) * FG_nb_pts_S(n - 1, q);
	a := a + FG_nb_pts_Nbar(1, q) * FG_nb_pts_N1(n - 1, q);
	return a;
  end );

#counting functions for hermitian. to be renamed.

# June 25, 2011, Anton

#############################################################################
#F  FG_herm_nb_pts_N( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_herm_nb_pts_N,  
  function(n, q)
	local Q, a, b, c;
	Q := q * q;
	if n = 1 then
		return Q - 1;
	fi;
	a := FG_herm_nb_pts_N(n - 1, q) * (Q - q - 1);
	b := FG_herm_nb_pts_S(n - 1, q) * (Q - 1);
	c := a + b;
	return c;
  end );

#############################################################################
#F  FG_herm_nb_pts_S( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_herm_nb_pts_S,  
  function(n, q)
	local a, b, c;
	#Q := q * q;
	if n = 1 then
		return 1;
	fi;
	a := FG_herm_nb_pts_N(n - 1, q) * (q + 1);
	b := FG_herm_nb_pts_S(n - 1, q);
	c := a + b;
	return c;
  end );

#############################################################################
#F  FG_herm_nb_pts_N1( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_herm_nb_pts_N1,  
  function(n, q)
	local a, b, c;
	#Q := q * q;
	if n = 1 then
		return q + 1;
	fi;
	a := FG_herm_nb_pts_N1(n - 1, q);
	b := FG_herm_nb_pts_N1(n - 1, q) * (q - 2) * (q + 1);
	c := FG_herm_nb_pts_S(n - 1, q) * (q + 1);
	return a + b + c;
  end );

#############################################################################
#F  FG_herm_nb_pts_Sbar( n, q )
# n: integer, q: prime power.
##
InstallGlobalFunction( FG_herm_nb_pts_Sbar,  
  function(n, q)
	local  a, b;
	#Q := q * q;
	if n = 1 then
		return 0;
	fi;
	a := FG_herm_nb_pts_Sbar(n - 1, q);
	b := FG_herm_nb_pts_N1(n - 1, q);
	return a + b;
  end );

#############################################################################
# precursors of "ElementNumber" functions
#############################################################################

#############################################################################
#F  FG_N1_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_N1_unrank, 
  function(q, v, offset, n, a)
  local l, i, j, k, j1, x, y, z, yz, u, alpha, beta, gamma, one, zero;
  one := Z(q)^0;
  zero := 0*Z(q);
  if n = 1 then
     l := q - 1;
     if a < l then
        alpha := FG_ffenumber(q,a+1);
        beta := alpha^-1;
        v[offset + 0] := alpha;
        v[offset + 1] := beta;	
        return;
     fi;
     Error("Error in FG_N1_unrank");
  fi;
  x := FG_nb_pts_S(1, q);
  y := FG_nb_pts_N1(n - 1, q);
  l := x * y;
  if a < l then
     i := QuoInt(a, y);    
     j := RemInt(a,y);
     FG_S_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_N1_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := FG_nb_pts_N1(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;

  if a < l then
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_N1_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := FG_nb_pts_N1(1, q);
  y := q - 2;
  z := FG_nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  if a < l then
     i := QuoInt(a, yz);
     j1 := RemInt(a,yz);
     j := QuoInt(j1, z);
     k := RemInt(j1, z);
     FG_N1_unrank(q, v, offset + (n - 1) * 2, 1, i);
     alpha := FG_ffenumber(q,2+j);  
     v[offset + 2 * (n - 1)] := alpha * v[offset + 2 * (n - 1)];
     FG_N1_unrank(q, v, offset, n - 1, k);
     beta := -alpha;
     gamma := one + beta;
     for u in [0..n-2] do
	   v[offset + 2 * u] := gamma * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in FG_N1_unrank (2)");
end );

#############################################################################
#F  FG_S_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_S_unrank, 
  function(q, v, offset, n, a)
  local l, i, j, x, y, u, alpha, beta, one, zero;
  one := Z(q)^0;
  zero := 0*Z(q);

  if n = 1 then 
     if a < q then 
        v[offset + 0] := FG_ffenumber(q,a);
        v[offset + 1] := zero;		
        return;
     fi;
     a := a - (q - 1);
     if a < q then	
        v[offset + 0] := zero;
        v[offset + 1] := FG_ffenumber(q,a);
        return;
     fi;
     Error("Error in FG_S_unrank");
  fi;
  x := FG_nb_pts_S(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;
  if a < l then 
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_S_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := FG_nb_pts_N(1, q);
  y := FG_nb_pts_N1(n - 1, q);
  l := x * y;
  if a < l then
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_N_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_N1_unrank(q, v, offset, n - 1, j);
     alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
     beta := -alpha;
     for u in [0..n-2] do
         v[offset + 2 * u] := beta * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in FG_S_unrank (2)");
end );

#############################################################################
#F  FG_Sbar_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_Sbar_unrank, 
  function(q, v, offset, n, a)
	local l, i, j, x, y, u, alpha, beta, one, zero;
  one := Z(q)^0; 
  zero := 0*Z(q);

  if n = 1 then 
     if a = 0 then 
	  v[offset + 0] := one;
	  v[offset + 1] := zero;
	  return;
     fi;
     if a = 1 then
        v[offset + 0] := zero;
        v[offset + 1] := one;
        return;
     fi;
     Error("Error in FG_Sbar_unrank");
  fi;
  y := FG_nb_pts_Sbar(n - 1, q);
  l := y;
  if a < l then 
     u := n - 1;
     v[offset + 2 * u] := zero;
     v[offset + 2 * u + 1] := zero;
     FG_Sbar_unrank(q, v, offset, n - 1, a);
     return;
  fi;
  a := a - l;
  x := FG_nb_pts_Sbar(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;
  if a < l then
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_Sbar_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := FG_nb_pts_Nbar(1, q);
  y := FG_nb_pts_N1(n - 1, q);
  l := x * y;
  if a < l then
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_Nbar_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_N1_unrank(q, v, offset, n - 1, j);
     alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
     beta := -alpha;
     for u in [0..n-2] do
	   v[offset + 2 * u] := beta * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in FG_Sbar_unrank (2)");
end );

#############################################################################
#F  FG_Nbar_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_Nbar_unrank, 
  function(q, v, offset, n, a)
  if n = 1 then
	if a < q - 1 then
		v[offset + 0] := FG_ffenumber(q,a+1);
		v[offset + 1] := Z(q)^0;
		return;
	fi;
      Error("Error in FG_Nbar_unrank");
  fi;
  Error("Error in FG_Nbar_unrank (2)");
end );

#############################################################################
#F  FG_N_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_N_unrank, 
  function(q, v, offset, n, a)
	local l, i, j, k, j1, x, y, z, yz, u, alpha, beta, gamma, delta, epsilon, 
            one, zero, w;
  w := Z(q);
  one := w^0;
  zero := 0*w;
  if n = 1 then
     x := q - 1;
     y := q - 1;
     l := x * y;
     if a < l then
        i := QuoInt(a, y);
        j := RemInt(a, y);
        v[offset + 0] := FG_ffenumber(q,1+j); 
        v[offset + 1] := FG_ffenumber(q,1+i);
        return;
     fi;
     Error("Error in FG_N_unrank");
  fi;
  x := FG_nb_pts_S(1, q);
  y := FG_nb_pts_N(n - 1, q);
  l := x * y;
  if a < l then
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_S_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_N_unrank(q, v, offset, n - 1, j);
  return;
  fi;
  a := a - l;
  x := FG_nb_pts_N(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;
  if a < l then
     i := QuoInt(a, y);
     j := RemInt(a, y);
     FG_N_unrank(q, v, offset + 2 * (n - 1), 1, i);
     FG_S_unrank(q, v, offset, n - 1, j);
     return;
  fi;
  a := a - l;
  x := FG_nb_pts_N(1, q);;
  y := q - 2;
  z := FG_nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  if a < l then
     i := QuoInt(a, yz);
     j1 := RemInt(a, yz);
     j := QuoInt(j1, z);
     k := RemInt(j1, z);
     FG_N_unrank(q, v, offset + (n - 1) * 2, 1, i);
     FG_N1_unrank(q, v, offset, n - 1, k);
     alpha := w;  
     beta := alpha^(j + 1);
     gamma := v[offset + (n - 1) * 2] * v[offset + (n - 1) * 2 + 1];
     delta := -gamma;
     epsilon := delta * beta;
     for u in [0..n-2] do
         v[offset + 2 * u] := epsilon * v[offset + 2 * u];
     od;
     return;
  fi;
  Error("Error in FG_N_unrank(2)");
end );

#######hermitian ranking stuff, new from Anton.

#############################################################################
#F  FG_herm_N_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_N_unrank, 
  function(q, v, offset, n, a)
  local Q, A, coset, rk1, x, one, zero, val, coset0, rk0, m_val, log, nb, l;
  Q := q * q;
  one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
     l := Q - 1;
     if a < l then
        x := FG_alpha_power(q, a);
        v[offset + 0] := x;
        return;
     fi;
     Error("Error in FG_herm_N_unrank");
  fi;
  A := Q - q - 1;
  nb := FG_herm_nb_pts_N(n - 1, q);

  if a < A * nb then
    coset := QuoInt(a, nb);
	rk1 := RemInt(a, nb);
    FG_herm_N_unrank(q, v, offset, n - 1, rk1);

    if coset = 0 then
      v[offset + n - 1] := zero;
    else
      coset := coset - 1;
      val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
      coset0 := QuoInt(coset, q + 1);
      rk0 := RemInt(coset, q + 1); # here was coset mod q + 1 (i.e., without parenthesis), which is wrong, Anton 7/15/11
      m_val := - val;
      log := FG_log_beta(q, m_val);
      if coset0 >= log then
        coset0 := coset0 + 1;
      fi;
      v[offset + n - 1] := FG_alpha_power(q, coset0) * FG_norm_one_element(q, rk0);     
    fi;
  else
    a := a - A * nb;
	nb := FG_herm_nb_pts_S(n - 1, q);
    rk1 := RemInt(a, nb);
    coset := QuoInt(a, nb);
    FG_herm_S_unrank(q, v, offset, n - 1, rk1);
    v[offset + n - 1] := FG_alpha_power(q, coset);
  fi;
end );

#############################################################################
#F  FG_herm_N_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_N_rank,
  function(q, v, offset, n)
  local Q, rk, val, m_val, alpha, A, rk1, coset, a, coset0, rk0, beta, nb, log;
  Q := q * q;
  if n = 1 then
     alpha := v[offset + 0];
     rk := FG_log_alpha(q, alpha);  
	 return rk;
  fi;
  val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
  nb := FG_herm_nb_pts_N(n - 1, q);
  if not IsZero(val) then
    rk1 := FG_herm_N_rank(q, v, offset, n - 1);
    if IsZero(v[offset + n - 1]) then
      coset := 0;
    else
      m_val := -val;
      log := FG_log_beta(q, m_val);
      a := v[offset + n - 1]^(q + 1);
      coset0 := FG_log_beta(q, a); 
      beta := v[offset + n - 1] * FG_alpha_power(q, coset0)^-1;  
      if coset0 > log then
         coset0 := coset0 - 1;
       fi;
      rk0 := FG_index_of_norm_one_element(q, beta); 
      coset := coset0 * (q + 1) + rk0;
      coset := coset + 1; 
    fi;
	rk := coset * nb + rk1;
  else
    A := Q - q - 1;
    rk := A * nb;
    coset := FG_log_alpha(q, v[offset + n - 1]);
	rk1 := FG_herm_S_rank(q, v, offset, n - 1);
    rk := rk + coset * FG_herm_nb_pts_S(n - 1, q) + rk1;
  fi;
  return rk;
end );

#############################################################################
#F  FG_herm_S_unrank( q, v, offset, n, a )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_S_unrank, 
  function(q, v, offset, n, rk)
  local Q, zero, nb, coset, rk1, log, val, m_val;
  Q := q * q;
  zero := 0*Z(Q);
  if n = 1 then
     v[offset + 0] := zero;
     return;
  fi;
  nb := FG_herm_nb_pts_N(n - 1, q);
  if rk < (q + 1) * nb then
    coset := QuoInt(rk, nb);
    rk1 := RemInt(rk, nb);
    FG_herm_N_unrank(q, v, offset, n - 1, rk1);
    val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
    m_val := - val;
    log := FG_log_beta(q, m_val);
    v[offset + n - 1] := FG_alpha_power(q, log) * FG_norm_one_element(q, coset);
  else
    rk := rk - (q + 1) * nb;
    FG_herm_S_unrank(q, v, offset, n - 1, rk);
    v[offset + n - 1] := zero;
  fi;
end );

#############################################################################
#F  FG_herm_S_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_S_rank,
  function(q, v, offset, n)
  local val, rk, rk1, m_val, log, a, log1, nb, coset;
  if n = 1 then
     return 0;
  fi;
  rk := 0;
  if not IsZero(v[offset + n - 1]) then
    rk1 := FG_herm_N_rank(q, v, offset, n - 1);
    val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
    m_val := - val;  	
    log := FG_log_beta(q, m_val);
   a := v[offset + n - 1]^(q + 1);
    log1 := FG_log_beta(q, a);
    if log1 <> log then
      Error("Error in hermitian::FG_S_rank fatal: log1 != log");
    fi;
    a := v[offset + n - 1] * FG_alpha_power(q, log)^-1;
    coset := FG_index_of_norm_one_element(q, a);
    nb := FG_herm_nb_pts_N(n - 1, q);
    rk := coset * nb + rk1;
  else
    nb := FG_herm_nb_pts_N(n - 1, q);
    rk := FG_herm_S_rank(q, v, offset, n - 1);
    rk := rk + (q + 1) * nb;            
  fi;
  return rk;
end );

#############################################################################
#F  FG_herm_N1_unrank( q, v, offset, n, rk )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_N1_unrank, 
  function(q, v, offset, n, rk)
  local Q, i, one, zero, nb, rk1, rk2, coset, coset1, coset2, nb1, a, val, new_val, log, A;
  Q := q * q;
  one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
     v[offset + 0] := FG_norm_one_element(q, rk);
     return;
  fi;
  nb := FG_herm_nb_pts_N1(n - 1, q);
  if rk < nb then
    FG_herm_N1_unrank(q, v, offset, n - 1, rk);
    v[offset + n - 1] := zero;
    return;
  else
    rk := rk - nb;
    A := (q + 1) * (q - 2) * nb;
    if rk < A then
      nb1 := (q - 2) * nb;
      coset1 := QuoInt(rk, nb1);
      rk1 := RemInt(rk, nb1);
      coset2 := QuoInt(rk1, nb);
      rk2 := RemInt(rk1, nb);
      FG_herm_N1_unrank(q, v, offset, n - 1, rk2);
      val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
      coset2 := coset2 + 1;
      a := FG_alpha_power(q, coset2);
      for i in [0 .. n - 2] do
        v[offset + i] := a * v[offset + i];
      od;
      val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
      new_val := one - val;
      log := FG_log_beta(q, new_val);
      v[offset + n - 1] := FG_alpha_power(q, log) * FG_norm_one_element(q, coset1);
	else
      rk := rk - A;
      nb := FG_herm_nb_pts_S(n - 1, q);
      coset := QuoInt(rk, nb);
      rk1 := RemInt(rk, nb);
      FG_herm_S_unrank(q, v, offset, n - 1, rk1);
      v[offset + n - 1] := FG_norm_one_element(q, coset);
    fi;
  fi;
end );

#############################################################################
#F  FG_herm_N1_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_N1_rank,
  function(q, v, offset, n)
  local Q, one, val, rk, rk1, nb, nb1, A, coset, coset1, coset2, a, av, i, new_val, log, rk2, log1;
  Q := q * q;
  one := Z(Q)^0;
  if n = 1 then
     return FG_index_of_norm_one_element(q, v[offset + 0]);
  fi;
  rk := 0;
  if IsZero(v[offset + n - 1]) then
    rk := FG_herm_N1_rank(q, v, offset, n - 1);
    return rk;
  fi;
  nb := FG_herm_nb_pts_N1(n - 1, q);
  rk := nb;
  nb1 := (q - 2) * nb;
  A := (q + 1) * nb1;
	val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
	if not IsZero(val) then
		coset2 := FG_log_beta(q, val);
		a := FG_alpha_power(q, coset2);
		av := a^(-1);
		for i in [0 .. n - 2] do
			v[offset + i] := av * v[offset + i];
		od;
		rk2 := FG_herm_N1_rank(q, v, offset, n - 1);
		coset2 := coset2 - 1;
		new_val := one - val; 
		log := FG_log_beta(q, new_val);
		a := v[offset + n - 1]^(q + 1);
		log1 := FG_log_beta(q, a);
		a := FG_alpha_power(q, log)^(-1);
		a := a * v[offset + n - 1];
		coset1 := FG_index_of_norm_one_element(q, a);
		rk1 := coset2 * nb + rk2;
		rk := rk + coset1 * nb1 + rk1;
	else
		rk := rk + A;
		rk1 := FG_herm_S_rank(q, v, offset, n - 1); 
		coset := FG_index_of_norm_one_element(q, v[offset + n - 1]);
		rk := rk + coset * FG_herm_nb_pts_S(n - 1, q) + rk1;  
    fi;
  return rk;
end );

#############################################################################
#F  FG_herm_Sbar_unrank( q, v, offset, n, rk )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_Sbar_unrank, 
  function(q, v, offset, n, rk)
  local Q, one, zero, a, b, log, nb, i;
  Q := q * q;
  one := Z(Q)^0;
  zero := 0*Z(Q);
  if n = 1 then
    Error("FG_herm_Sbar_unrank error: n = 1");
  fi;
    nb := FG_herm_nb_pts_Sbar(n - 1, q);

	if rk < nb then
		FG_herm_Sbar_unrank(q, v, offset, n - 1, rk);
		v[offset + n - 1] := zero;
	else
		rk := rk - nb;

		FG_herm_N1_unrank(q, v, offset, n - 1, rk);
		a := - one;
		log := FG_log_beta(q, a);
        b := FG_alpha_power(q, log);
        for i in [0..n - 2] do
          v[offset + i] := v[offset + i] * b;
        od;
        v[offset + n - 1] := one;
	fi;
end );

#############################################################################
#F  FG_herm_Sbar_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_herm_Sbar_rank,
  function(q, v, offset, n)
  local Q, one, val, rk, rk0, nb, i, a, b, bv, log;
  
  Q := q * q;
  one := Z(Q)^0;
  if n = 1 then
    Error("FG_herm_Sbar_rank error: n = 1");
  fi;
	if IsZero(v[offset + n - 1]) then
		rk := FG_herm_Sbar_rank(q, v, offset, n - 1);
	else
        #my_PG_element_normalize(v, offset, n);
        PG_element_normalize(v, offset, n);
  		nb := FG_herm_nb_pts_Sbar(n - 1, q);
		rk := nb;
		val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
        a := - one;
		if val <> a then
			Error("FG_herm_Sbar_rank error: val <> -1");
		fi;
		log := FG_log_beta(q, a);
		b := FG_alpha_power(q, log);
		bv := b^(-1);
		for i in [0 .. n - 2] do
			v[offset + i] := bv * v[offset + i];
		od;
		val := FG_evaluate_hermitian_form(q, v, offset, n - 1);
		if not IsOne(val) then
			Error("FG_herm_Sbar_rank not IsOne(val)");
		fi;
		rk0 := FG_herm_N1_rank(q, v, offset, n - 1);
		rk := rk + rk0;
	fi;
  return rk;
end );

#############################################################################
#F  FG_S_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_S_rank,
  function(q, v, offset, n)
  local a, l, i, j, x, y, u, alpha, beta, gamma, delta, epsilon;
  if n = 1 then				
     if IsZero(v[offset + 1]) then
        a := FG_pos(q, v[offset + 0]);
        return a;
     fi;	
     a := q - 1;		
     a := a + FG_pos(q, v[offset + 1]);	
     return a;
   fi;
   x := FG_nb_pts_S(1, q);
   y := FG_nb_pts_S(n - 1, q);
   l := x * y;
   alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
   if IsZero(alpha) then
      i := FG_S_rank(q, v, offset + 2 * (n - 1), 1);
      j := FG_S_rank(q, v, offset, n - 1);
      a := i * y + j;
      return a;
   fi;				
   a := l;
   y := FG_nb_pts_N1(n - 1, q);
   i := FG_N_rank(q, v, offset + 2 * (n - 1), 1);	
   beta := -alpha;
   delta := beta^-1;			# JB 18/08/2014: Found a bug here and fixed it
   for u in [0..n-2] do
       v[offset + 2 * u] := delta * v[offset + 2 * u];
   od;
   j := FG_N1_rank(q, v, offset, n - 1);	
   a := a + i * y + j;	
   return a;
end );

#############################################################################
#F  FG_N_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_N_rank,
  function(q, v, offset, n)
  local a, l, i, j, k, x, y, z, yz, u, alpha, beta, one,
            gamma, delta, epsilon, gamma2, epsilon_inv;		
  one := Z(q)^0;
  if n = 1 then	
     y := q - 1;
     j := FG_pos(q,v[offset + 0]) - 1;
     i := FG_pos(q, v[offset + 1]) - 1;
######     a := FG_ffenumber(q, i * y + j); # JB: 18/08/2014, found the bug here ####
	 a := i * y + j; 	# it was hard to find!
     return a;
  fi;

  gamma := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  x := FG_nb_pts_S(1, q);
  y := FG_nb_pts_N(n - 1, q);
  l := x * y;
  if IsZero(gamma) then
     i := FG_S_rank(q, v, offset + 2 * (n - 1), 1);
     j := FG_N_rank(q, v, offset, n - 1);
     a := i * y + j;
     return a;
  fi;
  a := l;
  x := FG_nb_pts_N(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;
  gamma2 := FG_evaluate_hyperbolic_quadratic_form(q, v, offset, n - 1);
  if IsZero(gamma2) then
     i := FG_N_rank(q, v, offset + 2 * (n - 1), 1);
     j := FG_S_rank(q, v, offset, n - 1);
     a := a + i * y + j;
  fi;
  a := a + l;
  x := FG_nb_pts_N(1, q);
  y := q - 2;
  z := FG_nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  i := FG_N_rank(q, v, offset + 2 * (n - 1), 1);
  alpha := Z(q);
  delta := -gamma;
  for j in [0..q-3] do
      beta := alpha^(j + 1);
      epsilon := delta * beta;
      if epsilon = gamma2 then
         epsilon_inv := epsilon^-1;
         for u in [0..n-2] do
             v[offset + 2 * u] := epsilon_inv * v[offset + 2 * u];
         od;
         k := FG_N1_rank(q, v, offset, n - 1);
         a := a + i * yz + j * z + k;
         return a;
      fi;
  od;
end );

#############################################################################
#F  FG_N1_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_N1_rank,
  function(q, v, offset, n)
  local a, l, i, j, k, x, y, z, yz, u, alpha, 
        alpha_inv, beta, gamma, gamma2, gamma_inv;
  if n = 1 then
     alpha := v[offset + 0];
     # beta := v[offset + 1];
     # gamma := alpha^-1;
     a := FG_pos(q, alpha) - 1;  	
     return a;
  fi;
  a := 0;
  alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  x := FG_nb_pts_S(1, q);
  y := FG_nb_pts_N1(n - 1, q);
  l := x * y;
  if IsZero(alpha) then
     i := FG_S_rank(q, v, offset + 2 * (n - 1), 1);		
     j := FG_N1_rank(q, v, offset, n - 1);
     a := i * y + j; 			
     return a;
  fi;
  a := a + l;
  gamma2 := FG_evaluate_hyperbolic_quadratic_form(q, v, offset, n - 1);
  x := FG_nb_pts_N1(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;
  if IsZero(gamma2) then
     i := FG_N1_rank(q, v, offset + 2 * (n - 1), 1);
     j := FG_S_rank(q, v, offset, n - 1);	
     a := a + i * y + j;		
     return a;
  fi;
  a := a + l;
  x := FG_nb_pts_N1(1, q);
  y := q - 2;
  z := FG_nb_pts_N1(n - 1, q);
  yz := y * z;
  l := x * yz;
  alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  j := FG_pos(q,alpha) - 2;
  alpha_inv := alpha^-1;
  v[offset + 2 * (n - 1)] := alpha_inv * v[offset + 2 * (n - 1)];
  i := FG_N1_rank(q, v, offset + 2 * (n - 1), 1);
  gamma2 := FG_evaluate_hyperbolic_quadratic_form(q, v, offset, n - 1);
  gamma_inv := gamma2^-1;
  for u in [0..n-2] do
      v[offset + 2 * u] := gamma_inv * v[offset + 2 * u];
  od;
  k := FG_N1_rank(q, v, offset, n - 1);

  a := a + i * yz + j * z + k;
  return a;
end );

#############################################################################
#F  FG_Sbar_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_Sbar_rank,
  function(q, v, offset, n)
  local a, l, i, j, x, y, u, alpha, beta, beta_inv;
  PG_element_normalize(v, offset, 2 * n);
  if n = 1 then
     if IsOne(v[offset + 0]) and IsZero(v[offset + 1]) then
	 a := 0;
	 return a;
     fi;
     if IsZero(v[offset + 0]) and IsOne(v[offset + 1]) then
        a := 1;
        return a;
     fi;
  fi;
  a := 0;
  if IsZero(v[offset + 2 * (n - 1)]) and IsZero(v[offset + 2 * (n - 1) + 1]) then
     a := FG_Sbar_rank(q, v, offset, n - 1);
     return a;
  fi;
  l := FG_nb_pts_Sbar(n - 1, q);
  a := a + l;
  alpha := v[offset + 2 * (n - 1)] * v[offset + 2 * (n - 1) + 1];
  x := FG_nb_pts_Sbar(1, q);
  y := FG_nb_pts_S(n - 1, q);
  l := x * y;
  if IsZero(alpha) then		
     i := FG_Sbar_rank(q, v, offset + 2 * (n - 1), 1);	
     j := FG_S_rank(q, v, offset, n - 1);	
     a := a + i * y + j;	
     return a;
  fi; 
  a := a + l;
  x := FG_nb_pts_Nbar(1, q);
  y := FG_nb_pts_N1(n - 1, q);
  i := FG_Nbar_rank(q, v, offset + 2 * (n - 1), 1);
  beta := -alpha;
  beta_inv := beta^-1;
  for u in [0..n-2] do
      v[offset + 2 * u] := beta_inv * v[offset + 2 * u];
  od;
  j := FG_N1_rank(q, v, offset, n - 1);
  a := a + i * y + j;
  return a;
end );

#############################################################################
#F  FG_Nbar_rank( q, v, offset, n )
#v: FFE vector.
##
InstallGlobalFunction( FG_Nbar_rank,
  function(q, v, offset, n)
    return FG_pos(q,v[offset + 0]) - 1;
end );

#############################################################################
# Low level ElementNumber/NumberElement functions (I left these currently as operations
# in case an extra debug round would be necessary, this could be useful).
#############################################################################

#############################################################################
#F  QElementNumber( d, q, a ) returns element number a of Q(d,q), see below.
##
InstallGlobalFunction( QElementNumber,
  function(d, q, a)

  ## Anton uses the bilinear form (for the 4 dimensional case)
	# 0 1 0 0 0
	# 1 0 0 0 0
	# 0 0 0 1 0
	# 0 0 1 0 0
	# 0 0 0 0 2
  ## Need to change form to canonical. We use the form
	# 2 0 0 0 0
	# 0 0 1 0 0
	# 0 1 0 0 0
	# 0 0 0 0 1
	# 0 0 0 1 0
 ## So we just permute the result by a cycle. Later we should just
 ## change the code so that we do not need this overhead.
 #
 ## Anton's code considers lists which begin at 0, and not at 1.
 ## So we need to make an adjustment for that.


    local n, x, i, one, zero, v, a2, cyc;
    cyc := PermList( Concatenation([2..d+1], [1]));
    one := Z(q)^0;
    zero := 0*Z(q);
    a2 := a - 1;
    v := List([1..d+1], i -> zero);
    n := d / 2;
    x := FG_nb_pts_Sbar(n, q);
    if a2 < x then
       FG_Sbar_unrank(q, v, 1, n, a2);
       return Permuted(v,cyc);
    fi;
    a2 := a2 - x;
    v[1 + 2 * n] := one;
    FG_N1_unrank(q, v, 1, n, a2);
    if IsOddInt(q) then
       for i in [0..n-1] do
	     v[1 + 2 * i] := -v[1 + 2 * i];
       od;
    fi;
    return Permuted(v,cyc);
end );

#############################################################################
#F  QplusElementNumber( d, q, a ) returns element number a of Q+(d,q), see below.
##
InstallGlobalFunction( QplusElementNumber,
  function(d, q, a)
      # The quadratic form here is simply the
      # "sums of hyperbolic pairs" form x1x2 + x3x4 +...
      # In this case, we do not need to permute the
      # result as there is no anisotropic part.

    local n, v, i, zero, a2;
    zero := 0*Z(q);	
    a2 := a - 1;
    v := List([1..d+1], i -> zero);
    n := (d+1) / 2;
    FG_Sbar_unrank(q, v, 1, n, a2);
    return v;
end );

#############################################################################
#F  QminusElementNumber( d, q, a ) returns element number a of Q-(d,q), see below.
##
InstallGlobalFunction( QminusElementNumber,
  function(d, q, a)
    local n, x, i, one, zero, v, w, a2, form, c1, c2, c3,
          b, c, minusz, x1, x2, u, vv, wprim, z, perm;

      # The quadratic form here is 
      # "sums of hyperbolic pairs" form x1x2 + x3x4 +...
      # plus c1 x_{2n}^2 + c2 x_{2n}x_{2n+1} + c3 x_{2n+1}^2
      #
      # The canonical form in FinInG has the anisotropic part
      # of an elliptic quadric in the first two coordinates. Thus
      # we simply apply a double transposition to the coordinates
      # so that Anton's form is the same as ours.

    perm := (1,d)(2,d+1);

    # Choose [c1, c2, c3] compatible with FinInG:
    if IsEvenInt(q) then
       form := QuadraticForm( EllipticQuadric(d, q) )!.matrix;
    else
       form := SesquilinearForm( EllipticQuadric(d, q) )!.matrix;
    fi; 
   
    c1 := form[1][1];
    c2 := form[1][2];
    c3 := form[2][2];

    wprim := Z(q);
    one := wprim^0;
    zero := 0*wprim;
    a2 := a - 1;
    v := List([1..d+1], i -> zero);
    n := (d-1) / 2;
    x := FG_nb_pts_Sbar(n, q); 
    if a2 < x then
       v[1 + 2 * n] := zero;
       v[1 + 2 * n + 1] := zero;
       FG_Sbar_unrank(q, v, 1, n, a2);
       return Permuted(v, perm);
    fi;
    a2 := a2 - x;
    x := FG_nb_pts_N1(n, q); 
    b := QuoInt(a2, x);
    c := RemInt(a2, x);
    if IsZero(b) then
       x1 := one;
	 x2 := zero;
    else
       b := b - 1;
       x1 := FG_ffenumber(q, b);
       x2 := one;
    fi;

    v[1 + 2 * n] := x1;
    v[1 + 2 * n + 1] := x2; 
    u := x1 * x1;
    u := c1 * u;	# different to Anton's code
    vv := x1 * x2;
    vv := c2 * vv;
    w := x2 * x2;
    w := c3 * w;
    z := u + vv;
    z := z + w;

    FG_N1_unrank(q, v, 1, n, c);
    minusz := -z;
    if not IsOne(minusz) then
       for i in [0..n-1] do
           v[1 + 2 * i] := minusz * v[1 + 2 * i];
       od;
    fi;

    return Permuted(v, perm);
end );

#############################################################################
#F  QNumberElement( d, q, var ) returns the number of element <var> of Q(d,q)
# see QElementNumber for specification of form.
##
InstallGlobalFunction( QNumberElement,
  function(d, q, var)
    local wittindex, x, a, b, v, i, cyc; 
    cyc := PermList( Concatenation([2..d+1], [1]));

    v := Permuted(StructuralCopy(var!.obj), cyc^-1);
    wittindex := d/2; 
    x := FG_nb_pts_Sbar(wittindex, q);
    if IsZero(v[2 * wittindex + 1]) then
	 a := FG_Sbar_rank(q, v, 1, wittindex);
	 return a + 1;
    fi;
    a := x;
    if not IsOne(v[1 + 2 * wittindex]) then
	 	PG_element_normalize(v, 1, d + 1);
    fi;  
    if IsOddInt(q) then
	   for i in [0..d/2-1] do
	       v[2 * i+1] := -v[2 * i+1];
	   od;
    fi;
    b := FG_N1_rank(q, v, 1, wittindex);
    return a + b + 1; ## adjustment for lists beginning at 1
end );

#############################################################################
#F  QplusNumberElement( d, q, var ) returns the number of element <var> of Q+(d,q)
# see QplusNumberElement for specification of form.
##
InstallGlobalFunction( QplusNumberElement,
  function(d, q, var)
    local wittindex, a, v; 
    v := StructuralCopy(var!.obj);        
    wittindex := (d+1)/2;
    a := FG_Sbar_rank(q, v, 1, wittindex);
    return a + 1; ## adjustment for lists beginning at 1
end );

## still need to fix QminusNumberElement. Fixing means optimizing. It is tested and works beautifully. jdb. 30/9
#############################################################################
#F  QminusNumberElement( d, q, var ) returns the number of element <var> of Q-(d,q)
# see QminusNumberElement for specification of form.
##
InstallGlobalFunction( QminusNumberElement,
  function(d, q, var)
    local wittindex, a, v, x, x1, x2, c1, c2, c3, perm,
          form, i, u, w, vv, z, minusz, minuszv, c, b;
	
    # Choose [c1, c2, c3] compatible with FinInG:
    if IsEvenInt(q) then
       form := QuadraticForm( EllipticQuadric(d, q) )!.matrix;
    else
       form := SesquilinearForm( EllipticQuadric(d, q) )!.matrix;
    fi; 
   
    c1 := form[1][1];
    c2 := form[1][2];
    c3 := form[2][2];

    wittindex := (d-1)/2;

    perm := (1,d)(2,d+1);   ## it would be nice to remove this permutation

    v := StructuralCopy(var!.obj); 
    v := Permuted(v, perm);

    PG_element_normalize(v, 1, d + 1);

    x1 := v[1 + 2 * wittindex];
    x2 := v[2 + 2 * wittindex];

    if IsZero( x1 ) and IsZero( x2 ) then
       a := FG_Sbar_rank(q, v, 1, wittindex);
       return a + 1;  ## adjustment for lists beginning at 1
    fi;

    a := FG_nb_pts_Sbar(wittindex, q);

    if IsOne( x1 ) and IsZero( x2 ) then
	 	b := 0;
    else 
	 	if not IsOne( x2 ) then
	    	Error("ERROR in Qminus_rank x2 <> 1");
	 	fi;
	 	b := FG_pos(q, x1) + 1;  
    fi;

    x := FG_nb_pts_N1(wittindex, q);
    u := c1 * x1 * x1; 	
    vv := c2 * x1 * x2;
    w := c3 * x2 * x2;  
    z := u + vv;  
    z := z + w;   
	if IsZero(z) then
		Error("ERROR Qminus_rank z = 0");
	fi;

    minusz := -z; 
    minuszv := minusz^-1; 

	# JB (19/08/2014): Found the bug and it was that only the odd places need to be multiplied
    if not IsOne( minusz ) then
		for i in [0..wittindex-1] do
			v[1 + 2 * i] := minuszv * v[1 + 2 * i];
		od;
        #v := minuszv * v;
    fi;
    c := FG_N1_rank(q, v, 1, wittindex);

    return a + b * x + c + 1;  ## adjustment for lists beginning at 1
end );

#############################################################################

## New hermitian stuff... These functions look quite interesting, but are never used...

#InstallGlobalFunction( enum_line,
#  function( A, B, n )
  
    ## This function returns the n-th point on the interval
    ## ]A,B[. This routine uses the ordering of a finite field GF(q).
    ## For instance, we know that 0 and 1 are the first two elements of GF(q).
    ## The integer n must be between 1 and q - 1.
   
#    local pg, q, x, C;
#    pg := A!.geo;
#    q := Size( pg!.basefield );
#    x := FG_ffenumber( q, n + 1 );  ## n = 1 -> primitive root
#    C := VectorSpaceToElement( pg, A!.obj + x * B!.obj ); 
#    return C;
#  end );

#InstallGlobalFunction( enum_BaerSubline,
#  function( q, n )
  
    ## This function returns the n-th point on the canonical Baer subline
    ## X0^(q+1) + X1^(q+1), of PG(1, q^2).
    ## Note: Z(q^2)^(q+1) = Z(q) in GAP.

#    local k, x;
    
    ## Our points are (1, Z(q^2)^i) such that Z(q)^i=-1  (i in [1..q^2-1]).
    ## This condition holds iff i = j (q-1) + k with j in [0..q], 
    ## k = (q-1)/2 for q odd, k = q-1 for q even.
    
#    if IsEvenInt(q) then 
#       k := q - 1; 
#    else 
#       k := (q - 1)/2; 
#    fi;
#    x := [ Z(q)^0, Z(q^2)^( (n-1) * (q-1) + k ) ];        
#    return x;    
#  end );
   
#InstallGlobalFunction( enum_unital,
#  function( q, n )
  
    ## This function returns the n-th point on the canonical unital
    ## X0^(q+1) + X1^(q+1) + X2^(q+1).
    ##    
    ## First we use the representation of the classical unital U_{0,beta}
    ## from page 70 of Barwick and Ebert's book "Unitals in Projective Planes".
    ## That is:
    ##    U_{0,beta} = {(x, beta * x^{q+1} + r, 1) : x in GF(q^2), r in GF(q)}\cup {(0,1,0)}.
    ## This representation has the advantage that it is simple to enumerate by
    ## elements of GF(q^2) x GF(q) (plus one extra point (0,1,0)).
    ## The Gram matrix for this Hermitian curve is
    ##    [[beta-beta^q,0,0],[0,0,-1],[0,1,0]] * e where e^q+e=0.
    ## 
        
#    local beta, one, i, j, x, r, point, e, mat, b, binv;
    
#    beta := Z(q^2);
#    one := beta^0;
    
#    if n = q^3 + 1 then 
#       point := [0,1,0] * one;
#    else
#       i := (n-1) mod q^2;                 ## an integer in [0..q^2-1]
#       j := (n-1-((n-1) mod q^2)) / q^2;   ## an integer in [0..q-1]
#       x := FG_ffenumber( q^2, i );
#       r := FG_ffenumber( q, j );
#       point := [x, beta * x^(q+1)+ r, one];
#    fi;
#    if IsEvenInt(q) then
#       e := Z(q^2)^(q+1);
#    else
#       e := Z(q^2)^((q+1)/2);
#    fi;
#    mat := [[beta-beta^q,0,0],[0,0,-1],[0,1,0]] * e;
#    b := BaseChangeToCanonical( HermitianFormByMatrix(mat, GF(q^2)) );
#    binv := b^-1;
#    return point * binv; 
#  end );

#############################################################################

#############################################################################
#F  HermElementNumber( d, q, a ) returns element number a of H(d,q^2). see 
# below for form.
##
InstallGlobalFunction( HermElementNumber,
  function(d, q, a)
      # The hermitian form here is simply 
      # x(1) x(2)^q + ... + x(n-1) x(n)^q

    local n, v, i, a2;
    #zero := 0*Z(q);	
    a2 := a - 1;
    v := ListWithIdenticalEntries(d+1,Z(q)^0);
	n := Int((d+1) / 2);
	FG_herm_Sbar_unrank(Sqrt(q),v,1,d+1,a2);
    return v;
end );

#############################################################################
#F  HermElementNumber( d, q, var ) returns the number of the element <var>
# of H(d,q^2), see HermElementNumber for the form.
##
InstallGlobalFunction( HermNumberElement,
  function(d, q, var)
    local wittindex, a, v,y;
	v := StructuralCopy(var!.obj);        
	y := v[PositionNonZeroFromRight(v)];
	if IsOddInt(q) then
	  y := y/(Z(q)^((Sqrt(q)-1)/2)); #Strange Anton normalization...
	fi;
	v := v/y;
    #wittindex := (d+1)/2;
    a := FG_herm_Sbar_rank(Sqrt(q), v, 1, d+1);
    return a + 1; ## adjustment for lists beginning at 1
end );

#############################################################################
# The enumerator for points of a polar space, bundled in one operation.
#############################################################################
InstallMethod( AntonEnumerator, 
  "for a collection of subspaces of a polar space",
  [IsSubspacesOfClassicalPolarSpace],

  ## This operation puts together the Anton's code into
  ## an organised form. It computes an enumerator just
  ## for points of a polar space. Thereafter, for higher variety
  ## types, induction by non-degenerate hyperplane sections is
  ## used (except for symplectic spaces).

  function( vs )
    local ps, enum, flavour, d, q, projenum;
    if vs!.type <> 1 then 
       Error("Only applicable to points");
    fi;

    ps := vs!.geometry;
    flavour := PolarSpaceType(ps);
    d := ps!.dimension;
    q := Size(ps!.basefield);   

    if flavour = "parabolic" then

       ## Parabolic

       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, QElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return QNumberElement(d, q, x); 
         end ) 
       );
    elif flavour = "hyperbolic" then

       ## Hyperbolic

       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, QplusElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return QplusNumberElement(d, q, x);
         end ) 
       );
    elif flavour = "elliptic" then

       ## Elliptic

       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, QminusElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return QminusNumberElement(d, q, x); 
         end ) 
       );

    elif flavour = "symplectic" then

       ## Symplectic (uses just the enumerator for the points of 
       ##             the ambient projective space)

       projenum := Enumerator( Points(AmbientSpace(ps)) );
       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, projenum[n]!.obj);
         end,
         NumberElement := function( e, x )
           return Position(projenum, x);
         end ) 
       );
    elif flavour = "hermitian" then

       ## Hermitian 
       
       enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return VectorSpaceToElement(ps, HermElementNumber(d, q, n));
         end,
         NumberElement := function( e, x )
           return HermNumberElement(d, q, x); 
         end ) 
       );
       

    #   projenum := EnumeratorByOrbit( Points( ps ) );  ## this computes the whole list of points!!
    #   enum := EnumeratorByFunctions( vs, rec(
    #     ElementNumber := function( e, n )
    #       return VectorSpaceToElement(ps, projenum[n]!.obj);
    #     end,

    #     NumberElement := function( e, x )
    #       return Position(projenum, x);
    #     end ) 
    #   );
    else
       Error("not implemented for this polar space");
    fi;
    return enum;
  end );

#############################################################################
# Low level enumerators
#############################################################################

#############################################################################
#F FG_specialresidual: our special helper enumerator function that returns
# an enumerator (actually just a list) of elements of dimension j on a given j-1 
# space, not in a given hyperplane.
##
InstallGlobalFunction(FG_specialresidual, 
  function(ps, v, hyp)

  ## This function returns an enumerator for the j spaces on a 
  ## given j-1 space v which are not contained in a hyperplane hyp.
  ## This could be made more efficient.

  local j, enum, res;
    j := v!.type + 1; 
    res := Enumerator( ShadowOfElement(ps, v, j) );
    enum := Filtered(res, x -> not x in hyp); 
    return enum;
  end );

#############################################################################
#F FG_enum_orthogonal.  This is a "helper" function which makes the enumerator
# for a set of elements of a canonical orthogonal space.
##
InstallGlobalFunction( FG_enum_orthogonal,
 
  function( ps, j )
  local hyp, pg, d, f, iter, x, ps2, vs, ressize, 
        em, varsps2jmin1, varsps2j, enum2, enum, enumextra, 
        vars, rep, disc, sqs, type, q, zero, one;

  vs := ElementsOfIncidenceStructure(ps, j);
  if j = 1 then
    enum := AntonEnumerator( vs );
  elif j > 1 then
    pg := AmbientSpace(ps);
    type := PolarSpaceType(ps);
    d := ps!.dimension;
    f := ps!.basefield;
    q := Size(f);   
    zero := Zero(f);
    one := One(f);
    
    ## The number of j spaces on a j-1 space is the number of points
    ## in the quotient polar space (over a j-1 space)
    
    x := List([1..d], i -> zero);
    hyp := ShallowCopy(IdentityMat(d,f));    
    if type = "parabolic" or type = "elliptic" then       
       hyp := Concatenation([x], hyp);     
    else 
       x[2] := one;
       x[d] := one;
       Append(hyp, [x]);
    fi;
    hyp := TransposedMat(hyp);
    hyp := VectorSpaceToElement(pg, hyp);               
    disc := TypeOfSubspace(ps, hyp); 

         ## Here ps2 is the polar space induced by the hyperplane section.
         ## ressize is the number of j spaces not contained in hyp 
         ## incident with a j-1 space contained in hyp. Of course, we
         ## have a special case when j is the Witt index.

    if type = "hyperbolic" then
       ps2 := ParabolicQuadric(d-1, f);
       ressize := Size(ElementsOfIncidenceStructure(HyperbolicQuadric(d+2-2*j, f), 1)); 
       #if j <= WittIndex( SesquilinearForm(ps2) ) then 
       if j <= WittIndex( QuadraticForm(ps2) ) then #was "SesquilinearForm"
           ressize := ressize - Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+1-2*j, f), 1));
       fi;
    elif type = "elliptic" then   
       ps2 := ParabolicQuadric(d-1, f);
       ressize := Size(ElementsOfIncidenceStructure(EllipticQuadric(d+2-2*j, f), 1));
       #if j <= WittIndex( SesquilinearForm(ps2) ) then
	   if j <= WittIndex( QuadraticForm(ps2) ) then #was "SesquilinearForm"
          ressize := ressize - Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+1-2*j, f), 1));
       fi;
    # type = "parabolic" in what follows
    elif disc = "elliptic" then
       ps2 := EllipticQuadric(d-1, f);
       ressize := Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+2-2*j, f), 1)); 
       #if j <= WittIndex( SesquilinearForm(ps2) ) then
	   if j <= WittIndex( QuadraticForm(ps2) ) then #was "SesquilinearForm"
          ressize := ressize - Size(ElementsOfIncidenceStructure(EllipticQuadric(d+1-2*j, f), 1));
       fi;
    else 
       ps2 := HyperbolicQuadric(d-1, f);        
       ressize := Size(ElementsOfIncidenceStructure(ParabolicQuadric(d+2-2*j, f), 1));
       #if j <= WittIndex( SesquilinearForm(ps2) ) then
       if j <= WittIndex( QuadraticForm(ps2) ) then #was "SesquilinearForm"
          ressize := ressize - Size(ElementsOfIncidenceStructure(HyperbolicQuadric(d+1-2*j, f), 1));
       fi;
    fi;

    em := NaturalEmbeddingBySubspace(ps2, ps, hyp);

    ## Enumerate last the j-spaces contained in the non-deg hyperplane  

    #if j <= WittIndex( SesquilinearForm(ps2) ) then
    if j <= WittIndex( QuadraticForm(ps2) ) then #was SesquilinearForm

       varsps2j := ElementsOfIncidenceStructure( ps2, j );
       enumextra := Enumerator( varsps2j );
    fi;
      
    varsps2jmin1 := ElementsOfIncidenceStructure( ps2, j-1 );
    enum2 := FG_enum_orthogonal( ps2, j-1);  

    enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           local l, k, enumres, t, v;   
           if n > Size(enum2) * ressize then
              l := n - Size(enum2) * ressize;
              return enumextra[l]^em;
           else

           ## The way this works is that n is the position
           ## of the l-th j-space incident with the k-th (j-1)-space
           ## of vars2ps2. So we must first decompose n as n=(k-1)ressize+l.

              l := RemInt(n, ressize);
              if l = 0 then l := ressize; fi;  
              k := (n-l) / ressize + 1;    
              v := enum2[k]^em;   
              enumres := FG_specialresidual(vs!.geometry, v, hyp);  
              return enumres[l];
           fi;
         end,
         NumberElement := function( e, x )

           ## Here we must first find the unique j-1 space
           ## incident with x, and then find its place in the ordering.

           local w, prew, k, enumres, l, numinhyp;

             ## first deal with the case that x is in hyp

             if x in hyp then
                numinhyp := Size(enum2) * ressize;
                prew := em!.prefun(x);
                return numinhyp + Position(enumextra, prew);
             else            
                w := Meet(hyp, x);        
                prew := em!.prefun(w);    
				k := Position(enum2, prew);   
                enumres := FG_specialresidual(vs!.geometry, w, hyp); 
                l := Position(enumres, x);
                return (k-1)*ressize + l;
             fi;
         end ) );
  else 
     Error("j must be a positive integer");
  fi;
  return enum;
  end );

#############################################################################
#F FG_enum_hermitian.  This is a "helper" function which makes the enumerator
# for a set of elements of a canonical hermitian space.
# bug repaired in NumberElement function. Bug found by Frédéric Vanhove. 
##
InstallGlobalFunction( FG_enum_hermitian,

  function( ps, j )
  local hyp, pg, d, f, ps2, vs, ressize, em, varsps2jmin1, 
        varsps2j, enum2, enum, enumextra, vars;
  vs := ElementsOfIncidenceStructure(ps, j);
  if j = 1 then
    enum := AntonEnumerator( vs );
  elif j > 1 then
    pg := AmbientSpace(ps);
    d := ps!.dimension;
    f := ps!.basefield;
    hyp := NullMat(d,d+1,f);
    hyp{[1..d]}{[2..d+1]} := IdentityMat(d, f);    
    hyp := VectorSpaceToElement(pg, hyp);
    
         ## ps2 is the polar space induced by the hyperplane section.
         ## ressize is the number of j spaces not contained in hyp 
         ## incident with a j-1 space contained in hyp. 

     ps2 := HermitianPolarSpace(d-1, f);
	 ressize := Size(ElementsOfIncidenceStructure(HermitianPolarSpace(d+2-2*j, f), 1)); 

     if j <= WittIndex( SesquilinearForm(ps2) ) then
        ressize := ressize - Size(ElementsOfIncidenceStructure(HermitianPolarSpace(d+1-2*j, f), 1));
        varsps2j := ElementsOfIncidenceStructure( ps2, j );
        enumextra := Enumerator( varsps2j );
     fi;
     
     em := NaturalEmbeddingBySubspace(ps2, ps, hyp);        
     varsps2jmin1 := ElementsOfIncidenceStructure( ps2, j-1 );
     enum2 := FG_enum_hermitian( ps2, j-1 ); 

     enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           local l, k, enumres, t, v;  
           if n > Size(enum2) * ressize then
              l := n - Size(enum2) * ressize;
              return enumextra[l]^em;
           else
              l := RemInt(n, ressize);
              if l = 0 then l := ressize; fi;  
              k := (n-l) / ressize + 1;    
              v := enum2[k]^em;                     
              enumres := FG_specialresidual(vs!.geometry, v, hyp);  
              return enumres[l];
           fi;
         end,
         NumberElement := function( e, x )
           local w, prew, k, enumres, l, numinhyp;
             if x in hyp then
                numinhyp := Size(enum2) * ressize;
                #prew := PreImage(em, x); #this was bug 1
                prew := em!.prefun(x);
				return numinhyp + Position(enumextra, prew);
             else            
                w := Meet(hyp, x);
                #prew := PreImage(em, w); #this was bug 2
                prew := em!.prefun(w);
				#k := Position(varsps2jmin1, prew); this was bug 3
				k := Position(enum2, prew);
                enumres := FG_specialresidual(vs!.geometry, w, hyp); 
                l := Position(enumres, x);
                return (k-1)*ressize + l;
             fi;
         end ) );
  else 
     Error("j must be a positive integer");
  fi;
  return enum;
end );

#############################################################################
#F FG_enum_symplectic.  
##
InstallGlobalFunction( FG_enum_symplectic,

  ## This is a "helper" function which makes the enumerator
  ## for a set of varieties of a symplectic space. We would
  ## like an algorithm for these. Recall that for the orthogonal
  ## and hermitian cases, we used induction by non-degenerate
  ## hyperplane sections to calculate the enumerators. We do not
  ## have non-degenerate hyperplanes in symplectic spaces!

  function( ps, j )
    local vs, vars, enum;
    vs := ElementsOfIncidenceStructure(ps, j);
    vars := EnumeratorByOrbit( vs );

    enum := EnumeratorByFunctions( vs, rec(
         ElementNumber := function( e, n )
           return vars[n];
         end,
         NumberElement := function( e, x )
           return Position(vars, x);
         end ) );

    return enum;
end );

#############################################################################
# Some left over
#############################################################################

#merge these two, give it a sensible name, see if one can be thrown away... done.

#InstallGlobalFunction(my_PG_element_normalize,
#	[IsFFECollection, IsPosInt, IsPosInt],
#	function(v, offset, n)

  ## This function takes the first nonzero element
  ## from the right (!) and normalises the vector v
  ## by this element.

#  local ii, i, a, one;
#  one := One(v[1]);	   
#  for ii in [0..n - 1] do 
#      i := n - 1 - ii;    
#      a := v[offset + i]; 
#      if not IsZero(a) then
#         if IsOne(a) then  
#	     return;
#         fi;
#         a := a^-1;
#         v{[offset..i-1+offset]} := a * v{[offset..i-1+offset]};
#         v[offset + i] := one;
#         return;
#      fi;
#  od;
#  Error("zero vector");
#
#end );

#############################################################################
# The enumerator using the orbit. This is the only operation declaration in 
# this file for a user intended operation. 
#############################################################################

#############################################################################
#O EnumeratorByOrbit. returns an enumerator using the orbit.  
##
InstallMethod( EnumeratorByOrbit, 
  "for a collection of subspaces of a polar space",
  [ IsSubspacesOfClassicalPolarSpace ],
  function( vs )

  ## In this operation, we provide a way to compute the full
  ## list of elements by using the transitive action of the isometry
  ## group. Using an enumerator to get a full list is much slower.                              

  ## We use the package "orb" by Mueller, Neunhoeffer and Noeske,
  ## which is much quicker than using the standard orbit algorithm
  ## that exists in GAP.

    local ps, j, rep, vars, isom, gens;    
     ps := vs!.geometry;
     j := vs!.type;
     isom := IsometryGroup(ps);
     gens := GeneratorsOfGroup(isom);
     rep := RepresentativesOfElements(ps)[j];
     vars := Orb(gens,rep,OnProjSubspaces, rec( hashlen:=Int(5/4*Size(vs)) )); 
     Enumerate(vars, Size(vs));   
     return vars;
   end );

# jdb: patched 15/2/2016, using a patch file from Max Horn.
#############################################################################
#O AsList. Lists all elements in a collection of subspaces of polar space.
##
InstallMethod( AsList, 
	"for subspaces of a polar space", 
	[IsSubspacesOfClassicalPolarSpace],
	function( vs )
		return AsList(EnumeratorByOrbit( vs ));
	end );

# jdb: patched 15/2/2016, using a patch file from Max Horn.
#############################################################################
#O AsSortedList. Lists all elements in a collection of subspaces of polar space.  
##
InstallMethod( AsSortedList, 
	"for subspaces of a polar space", 
	[IsSubspacesOfClassicalPolarSpace],
	function( vs )
		return AsSortedList(EnumeratorByOrbit( vs ));
	end );

#Exactly the same as above?
#############################################################################
#O AsSortedList. Lists all elements in a collection of subspaces of polar space.  
##
InstallMethod( AsSSortedList, 
	"for subspaces of a polar space", 
	[IsSubspacesOfClassicalPolarSpace],
	function( vs )
		return AsSortedList(EnumeratorByOrbit( vs ));
	end );

#############################################################################
#O Enumerator. 
# This is it! This is the big method that returns an enumerator for a collection
# of all elements of a given type of the polar space <ps>
##
InstallMethod( Enumerator, 
	"for subspaces of a polar space",
	[ IsSubspacesOfClassicalPolarSpace ],
	function( vs )

    ## This method returns the enumerator for the elements of
    ## a polar space, of a given type. There are three "helper"
    ## operations involved here: FG_enum_orthogonal, FG_enum_hermitian,
    ## and FG_enum_symplectic. At the moment, we do not have nice methods
    ## for the points of a hermitian space, and for varieties of 
    ## a symplectic space.

    local ps, j, rep, enum, e, type, eEN, eNE, iso, pscanonical;
     ps := vs!.geometry;
     j := vs!.type;
     type := PolarSpaceType(ps);

     ## A bug here was fixed. When using non-canonical polar spaces, there was some problem
     ## in using the enum_* commands before an isomorphism was made. Fixed now. John 02/06/2010

     if HasIsCanonicalPolarSpace( ps ) and IsCanonicalPolarSpace( ps ) then     

		if type in [ "hyperbolic", "elliptic", "parabolic" ] then
           e := FG_enum_orthogonal(ps, j);
        elif type = "hermitian" then
           e := FG_enum_hermitian(ps, j);
        elif type = "symplectic" then
           e := FG_enum_symplectic(ps, j);  
        else
           Error("Unknown polar space type");
        fi;

        enum := EnumeratorByFunctions( vs, rec(
                  ElementNumber := e!.ElementNumber,
                  NumberElement := e!.NumberElement,
                  PrintObj := function ( e )
                    Print( "EnumeratorOfSubspacesOfClassicalPolarSpace( " );
                    View( vs ); 
                    Print( " )" );
                    return;
                  end,
                  Length := function( e )
                    return Size(vs);
                    end
                ) );
     else
        ## The following is an isomorphism from the canonical polar space TO ps
  
        iso := IsomorphismCanonicalPolarSpace( ps );
        pscanonical := Source(iso)!.geometry;     

        if type in [ "hyperbolic", "elliptic", "parabolic" ] then
           e := FG_enum_orthogonal(pscanonical, j);
        elif type = "hermitian" then
           e := FG_enum_hermitian(pscanonical, j);
        elif type = "symplectic" then
           e := FG_enum_symplectic(pscanonical, j);  
        else
           Error("Unknown polar space type");
        fi;

        ## These are enumerators in the canonical polar spaces

        eEN := e!.ElementNumber;
        eNE := e!.NumberElement;
                                                                        
        enum := EnumeratorByFunctions( vs, rec(
                   ElementNumber := function(e, n)
                     return iso!.fun( eEN( e, n ) );
                   end,
                   NumberElement := function(e, x)
                     return eNE( e, iso!.prefun(x) );
                   end,
                   PrintObj := function ( e )
                     Print( "EnumeratorOfSubspacesOfClassicalPolarSpace( " );
                     View( vs ); 
                     Print( " )" );
                     return;
                   end,
                   Length := function( e )
                     return Size(vs);
                   end
                 ) );  
     fi;
  ##   SetIsSSortedList( enum, true );  this line introduces a bug. It would be nice for it to be sorted though
     return enum;
   end );

# 7/4/2018 adapted enumerator to work for shadow of flags of cps too.
#############################################################################
#O Enumerator. 
# returns an enumerator for a collection of shadow subspaces of an element of
# a polar space.
##
InstallMethod( Enumerator, 
	"for shadow subspaces of a polar space",
	[IsShadowSubspacesOfClassicalPolarSpace and IsShadowSubspacesOfClassicalPolarSpaceRep ],
	function( res )
    
    # Here we use the quotient geometry over res!.inner to compute
    # the residual.

    local pstype, psdim, f, b, vardim, perp, vs, 
          ps2, newdim, canon_quot, Wvectors, mb, compl,
          gen, m, newform, changeform_inv, cquot_to_res, 
          enumps2, enum, ps, var, j, out, m2, iso, proj, higherels,
          img, changeform, res_to_cquot, bas, canbas, zero, basimgs,
          flag, flagtypes, flagpg, pg;

    ps := res!.geometry;
    var := res!.inner;
    out := res!.outer;
    j := res!.type;
    f := ps!.basefield;
    psdim := ps!.dimension;
    pstype := PolarSpaceType(ps);
    flag := res!.parentflag;
    flagtypes := flag!.types;

    #if Minimum(flagtypes) <= j and j <= Maximum(flagtypes) then #jdb 7/4/2018.
    #    Print("vlammeste miljaardedju\n");
    #    enumps2 := Enumerator( Subspaces(ps2, j-Size(var) ) ); #7/4/2018 added here " - Size(var)"

    if IsEmpty(var) then # old situation must be kept.
       ## This is the residual for varieties contained in a subspace, such
       ## as "points on a line". Hence we need only use a method which 
       ## simulates the residual of a projective subspace.

       ps2 := res!.factorspace; 
	
       enumps2 := Enumerator( Subspaces(ps2, j));
	
       enum := EnumeratorByFunctions( res, rec(
          ElementNumber := function(e, num)
             local x;
             x := NewMatrix(IsCMatRep,ps!.basefield,ps!.dimension+1,BasisVectors( Basis( enumps2[num] ) ) );
             if j = 1 then
                return Wrap(ps, j, x[1]);
             else
                return Wrap(ps, j, x);
             fi;           
          end, 
          NumberElement := function( e, elm )
             local unelm;
             unelm := elm!.obj;
             if j = 1 then
                unelm := [ unelm ]; 
             fi;
             return Position(enumps2, SubspaceNC(ps2, unelm));
          end ) );

    elif var = out then    

       ## This is the trivial residual for the variety itself, i.e.,
       ## "lines incident with line" = "just the line itself".

       enum := EnumeratorByFunctions( res, rec(
          ElementNumber := function(e, num)
             return VectorSpaceToElement(ps, var);
          end, 
          NumberElement := function( e, elm )
             return 1;
          end ) );
    else

        # Now there are two possibilities: (1): the flag is not a single element,
        # and j is not a type of one of the flag elements, and (2) j is larger
        # than the largest type of a flag element. (1) is dealt partially in the
        # above cases, except when j is in between the smallest and largest type of
        # the flag, (2) means that j is larger than the largest type of the flag elements,
        # then we deal with subspaces through a given one which is the subspace of largest
        # type in the flag.

        higherels := Filtered(flag!.els,x->x!.type > j);
        if not IsEmpty(higherels) then

            # This means that j is somewhere between smallest and largest type in flag,
            # but not equal to one of the types of the flag (by above)
            # We go for the easy solution: use the "enumerator" for shadows of flags
            # in projective spaces. This results currently in a list, since there is
            # no proper enumerator for projective spaces and shadows of flags.
            # So when we get the list, we just convert all elements in this list to
            # elements of the polar space. If we develop code for an enumerator of a
            # projective space later, we can recycle the essential part of the code here.
            # To do so, we just create the appropriate flag.

            pg := AmbientSpace(ps);
            flagpg := FlagOfIncidenceStructure(pg,flag!.els);
            enum := List(ShadowOfFlag(pg,flagpg,j),x->Embed(ps,x));
        else

            # This is the residual for varieties containing a subspace, such
            # as "lines on a point". We use projection to create the enumerator.

            b := VectorSpaceToElement(ps, var);
            proj := NaturalProjectionBySubspace(ps, b);
            img := Range(proj)!.geometry;
            newdim := j - b!.type;
            canon_quot := ElementsOfIncidenceStructure(img, newdim);
            enumps2 := Enumerator( canon_quot );

            enum := EnumeratorByFunctions( res, rec(
                ElementNumber := function(e, num)
                    return proj!.prefun( enumps2[num] );
            end,
            NumberElement := function( e, elm )
                return Position( enumps2, proj!.fun(elm ) );
            end ) );
        fi;
    fi;
    return enum;
  end );

