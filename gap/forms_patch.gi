Print("patching: forms\c ");

InstallOtherMethod( \^, "for a pair of FFE vectors and an hermitian form",
  [ IsVectorList and IsFFECollColl, IsHermitianForm ],
  function( pair, f )
    local frob,hh,bf,p;
    if Size(pair) <> 2 then 
       Error("The first argument must be a pair of vectors");
    fi;
    bf := f!.basefield;
	p := Characteristic(bf);
	hh := LogInt(Size(bf),p)/2;
	#frob := FrobeniusAutomorphism(f!.basefield); #here is a mistake!
	frob := FrobeniusAutomorphism(bf)^hh; #here is a mistake!
    return pair[1] * f!.matrix * (pair[2]^frob);
  end );

InstallOtherMethod( \^, "for a pair of FFE matrices and an hermitian form",
  [ IsFFECollCollColl, IsHermitianForm ],
  function( pair, f )
    local frob,hh,bf,p;
    if Size(pair) <> 2 then 
       Error("The first argument must be a pair of vectors");
    fi;
    bf := f!.basefield;
	p := Characteristic(bf);
	hh := LogInt(Size(bf),p)/2;
	#frob := FrobeniusAutomorphism(f!.basefield);  #here is a mistake!
	frob := FrobeniusAutomorphism(bf)^hh; #here is a mistake!
	return pair[1] * f!.matrix * (TransposedMat(pair[2])^frob);
  end );
