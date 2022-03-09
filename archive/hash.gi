#############################################################################
##
##                             orb package
##  hash.gi
##                                                          Juergen Mueller
##                                                          Max Neunhoeffer
##                                                             Felix Noeske
##
##  Copyright 2005-2008 by the authors.
##  This file is free software, see license information at the end.
##
##  Implementation stuff for fast hashing.
##
#############################################################################

InstallGlobalFunction( InitHT, function(len, hfun, eqfun)
  return rec(els := [],        # the elements to hash 
             vals := [],       # a value for each element, "true" not stored
             len := len,       # the length of the hash
             nr := 0,          # number of elements in hash
             hf := hfun.func,  # the hash function
             hfd := hfun.data, # data for the second argument to hf
             eqf := eqfun,     # a comparison function
             collisions := 0,  # number of collisions
             accesses := 0,    # number of accesses
             cangrow := false, # flag, whether hash table may grow
             ishash := true,   # a "magic" entry
            );
end );

InstallGlobalFunction( NewHT, function(sample,len)
  local eqfun,hfun,ht;
  hfun := ChooseHashFunction(sample,len);
  if hfun = fail then
      return fail;
  fi;
  eqfun := ApplicableMethod(\=,[sample,sample]);
  if eqfun = fail then eqfun := EQ; fi;
  if len < 11 then len := 11; fi;  # to avoid complete fillup! 
  ht := InitHT(len,hfun,eqfun);
  ht.cangrow := true;
  return ht;
end );

InstallMethod(ViewObj, "for hash tables", [IsRecord],
  function(ht)
    if IsBound(ht.ishash) and
       IsBound(ht.len) and IsBound(ht.nr) and IsBound(ht.els) and
       IsBound(ht.vals) and IsBound(ht.hf) and IsBound(ht.eqf) and
       IsBound(ht.collisions) and IsBound(ht.hfd) then
      # This is obviously a hash table
      Print("<hash table len=",ht.len," used=",ht.nr," colls=",
            ht.collisions," accs=",ht.accesses);
      if IsBound(ht.alert) then
          Print(" COLLISION ALERT!>");
      elif IsBound(ht.cangrow) then
          Print(" (can grow)>");
      else
          Print(">");
      fi;
    else
      TryNextMethod();
    fi;
  end);

InstallGlobalFunction( AddHT, function(ht, x, val)
  local h,g;
  ht.accesses := ht.accesses + 1;
  if ht.nr * 10 > ht.len * 9 then
    if IsBound(ht.cangrow) then
      Info(InfoOrb,3,"Hash table too full, growing...");
      GrowHT(ht,x);
    else
      Info(InfoOrb,1,"Hash table too full, cannot grow...");
      return fail;
    fi;
  fi;
  h := ht.hf(x,ht.hfd);
  if IsBound(ht.els[h]) then
    g := GcdInt(ht.len,h);
    if g = 1 then g := h; else g := 1; fi;
    repeat
      ht.collisions := ht.collisions + 1;
      h := h+g;
      if h>ht.len then h := h - ht.len; fi;
      if not(IsBound(ht.alert)) and QuoInt(ht.collisions,ht.accesses) > 100 then
        # We have a problem!
        Info(InfoOrb,1,"Alarm: Collision warning: Collisions: ",
             ht.collisions," Accesses: ",ht.accesses,"!");
        if not(IsBound(ht.cangrow)) then
          ht.alert := true;
        else
          GrowHT(ht,x);
          return AddHT(ht,x,val);
        fi;
      fi;
    until not(IsBound(ht.els[h]));
  fi;
  ht.els[h] := x;
  if val <> true then ht.vals[h] := val; fi;
  ht.nr := ht.nr+1;
  return h;
end );

InstallGlobalFunction( ValueHT, function(ht, x)
  local h,g;
  ht.accesses := ht.accesses + 1;
  h := ht.hf(x,ht.hfd);
  g := 0;
  while IsBound(ht.els[h]) do
    if ht.eqf(ht.els[h],x) then
        if IsBound(ht.vals[h]) then
            return ht.vals[h];
        else
            return true;
        fi;
    fi;
    if g = 0 then
      g := GcdInt(ht.len,h);
      if g = 1 then g := h; else g := 1; fi;
    fi;
    ht.collisions := ht.collisions + 1;
    h := h+g;
    if h>ht.len then h := h - ht.len; fi;
  od;
  return fail;
end );

InstallGlobalFunction( GrowHT, function(ht,x)
  local i,oldels,oldlen,oldvals;

  oldels := ht.els;
  oldvals := ht.vals;
  oldlen := ht.len;

  ht.els := [];
  ht.vals := [];
  ht.len := NextPrimeInt(ht.len * 2+1);
  Info(InfoOrb,2,"Growing hash table to length ",ht.len," !!!");
  if IsBound(ht.hfbig) and IsBound(ht.htdbig) then
      ht.hf := ORB_HashFunctionModWrapper;
      ht.hfd := [ht.hfbig,ht.hfdbig,ht.len];
  else
      ht.hf := ChooseHashFunction(x,ht.len);
      ht.hfd := ht.hf.data;
      ht.hf := ht.hf.func;
  fi;
  ht.nr := 0;
  ht.collisions := 0;
  ht.accesses := 0;
  # Now copy into new hash:
  for i in [1..oldlen] do
      if IsBound(oldels[i]) then
          if IsBound(oldvals[i]) then
              AddHT(ht,oldels[i],oldvals[i]);
          else
              AddHT(ht,oldels[i],true);
          fi;
      fi;
  od;
  Info(InfoOrb,3,"Done.");
end );


# Here comes stuff for hash functions:

# First a few hash functions:

InstallGlobalFunction( ORB_HashFunctionForShortGF2Vectors,
function(v,data)
  return NumberFFVector(v,2) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionForShort8BitVectors,
function(v,data)
  return NumberFFVector(v,data[2]) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionForGF2Vectors,
function(v,data)
  return HASHKEY_BAG(v,101,2*GAPInfo.BytesPerVariable,data[2]) mod data[1] + 1;
end );

InstallGlobalFunction( ORB_HashFunctionFor8BitVectors,
function(v,data)
  return HASHKEY_BAG(v,101,3*GAPInfo.BytesPerVariable,data[2]) mod data[1] + 1;
end );

# Now the choosing methods for compressed vectors:

InstallMethod( ChooseHashFunction, "failure method if all fails",
  [IsObject,IsInt],
  function(p,hashlen)
    return fail;
  end );

InstallMethod( ChooseHashFunction, "for compressed gf2 vectors",
  [IsGF2VectorRep and IsList,IsInt],
  function(p,hashlen)
    local bytelen;
    bytelen := QuoInt(Length(p),8);
    # Note that unfortunately gf2 vectors are not "clean" after their
    # "official" length, therefore we *must not* use the last, half-used
    # byte. This inevitably leads to collisions!
    if bytelen <= 8 then
        return rec( func := ORB_HashFunctionForShortGF2Vectors,
                    data := [hashlen] );
    else
        return rec( func := ORB_HashFunctionForGF2Vectors,
                    data := [hashlen,bytelen] );
    fi;
  end );

InstallMethod( ChooseHashFunction, "for compressed 8bit vectors",
  [Is8BitVectorRep and IsList,IsInt],
  function(p,hashlen)
    local bytelen,i,q,qq;
    q := Q_VEC8BIT(p);
    qq := q;
    i := 0;
    while qq <= 256 do
        qq := qq * q;
        i := i + 1;
    od;
    # i is now the number of field elements per byte
    bytelen := QuoInt(Length(p),i);
    # Note that unfortunately 8bit vectors are not "clean" after their
    # "official" length, therefore we *must not* use the last, half-used
    # byte. This inevitably leads to collisions!
    if bytelen <= 8 then
        return rec( func := ORB_HashFunctionForShort8BitVectors,
                    data := [hashlen,q] );
    else
        return rec( func := ORB_HashFunctionFor8BitVectors,
                    data := [hashlen,bytelen] );
    fi;
  end );

InstallGlobalFunction( ORB_HashFunctionForCompressedMats,
function(x,data)
  local i,res;
  res := 0;
  for i in [1..Length(x)] do
      res := (res * data[3] + data[2].func(x[i],data[2].data)) mod data[1];
  od;
  return res + 1;
end );

InstallMethod( ChooseHashFunction, "for compressed gf2 matrices",
  [IsGF2MatrixRep and IsList,IsInt],
  function(p,hashlen)
    local data;
    data := [hashlen,ChooseHashFunction(p[1],hashlen),
             PowerMod(2,Length(p[1]),hashlen)];
    return rec( func := ORB_HashFunctionForCompressedMats,
                data := data );
  end );

InstallMethod( ChooseHashFunction, "for compressed 8bit matrices",
  [Is8BitMatrixRep and IsList,IsInt],
  function(p,hashlen)
    local data,q;
    q := Q_VEC8BIT(p[1]);
    data := [hashlen,ChooseHashFunction(p[1],hashlen),
             PowerMod(q,Length(p[1]),hashlen)];
    return rec( func := ORB_HashFunctionForCompressedMats,
                data := data );
  end );

InstallGlobalFunction( ORB_HashFunctionForIntegers,
function(x,data)
  return x mod data[1] + 1;
end );

InstallMethod( ChooseHashFunction, "for integers", [IsInt,IsInt],
  function(p,hashlen)
    return rec( func := ORB_HashFunctionForIntegers, data := [hashlen] );
  end );
    
InstallGlobalFunction( ORB_HashFunctionForMemory,
function(x,data)
  return data[1](x!.el,data[2]);
end );

InstallMethod( ChooseHashFunction, "for memory objects", 
  [IsObjWithMemory, IsInt],
  function(p,hashlen)
    local hf;
    hf := ChooseHashFunction(p!.el,hashlen);
    return rec( func := ORB_HashFunctionForMemory, data := [hf.func,hf.data] );
  end );

InstallGlobalFunction( ORB_HashFunctionForPermutations,
function(p,data)
  local l;
  l:=LARGEST_MOVED_POINT_PERM(p);
  if IsPerm4Rep(p) then
    # is it a proper 4byte perm?
    if l>65536 then
      return HashKeyBag(p,255,0,4*l) mod data + 1;
    else
      # the permutation does not require 4 bytes. Trim in two
      # byte representation (we need to do this to get consistent
      # hash keys, regardless of representation.)
      TRIM_PERM(p,l);
    fi;
   fi;
   # now we have a Perm2Rep:
   return HashKeyBag(p,255,0,2*l) mod data + 1;
end );

InstallMethod( ChooseHashFunction, "for permutations", 
  [IsPerm, IsInt],
  function(p,hashlen)
    return rec( func := ORB_HashFunctionForPermutations, data := hashlen );
  end );

InstallGlobalFunction( ORB_HashFunctionForIntList,
function(v,data)
  local i,res;
  res := 0;
  for i in v do
      res := (res * data[1] + i) mod data[2];
  od;
  return res+1;
end );

InstallMethod( ChooseHashFunction, "for short int lists",
  [IsList, IsInt],
  function(p,hashlen)
    if ForAll(p,IsInt) then
        return rec(func := ORB_HashFunctionForIntList, data := [101,hashlen]);
    fi;
    TryNextMethod();
  end );

InstallGlobalFunction( ORB_HashFunctionForNBitsPcWord,
function(v,data)
  return ORB_HashFunctionForIntList(ExtRepOfObj(v),data);
end );

InstallMethod( ChooseHashFunction, "for N bits Pc word rep",
  [IsNBitsPcWordRep, IsInt],
  function(p,hashlen)
    return rec(func := ORB_HashFunctionForNBitsPcWord, data := [101,hashlen]);
  end );

InstallGlobalFunction( ORB_HashFunctionModWrapper,
  function(p,data)
    return data[1](p,data[2]) mod data[3];
  end );

InstallGlobalFunction( ORB_HashFunctionForMatList,
  function(ob,data)
    local i,m,res;
    res := 0;
    for m in ob do
        res := (res * data[1] + data[3].func(m,data[3].data)) mod data[2];
    od;
    return res+1;
  end );
    
InstallMethod( ChooseHashFunction, "for lists of matrices",
  [IsList, IsInt],
  function( l, hashlen )
    # FIXME:
    local r;
    if ForAll(l,IsMatrix) then
        r := ChooseHashFunction( l[1], hashlen );
        return rec( func := ORB_HashFunctionForMatList, 
                    data := [101,hashlen,r] );
    fi;
    TryNextMethod();
  end );

InstallMethod( ChooseHashFunction, 
  "for finite field vectors over big finite fields",
  [IsList, IsInt],
  function( l, hashlen )
    local f,q;
    if NestingDepthA(l) = 1 and Length(l) > 0 and IsFFE(l[1]) then
        f := Field(l);
        q := Size(f);
        return rec( func := ORB_HashFunctionForShort8BitVectors,
                    data := [hashlen,q] );
    fi;
    TryNextMethod();
  end );

##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
