#############################################################################
##
##  forms.gi              Forms package
##                                                              John Bamberg
##                                                              Jan De Beule
##
##  Copyright 2006 Ghent University
##
##  Implementation stuff for quadratic and sesquilinear forms
##
#############################################################################

#############################################################################
# Constructor methods:
#############################################################################

InstallMethod( FormByMatrix, "for a ffe matrix, a field and a string",
  [IsMatrix and IsFFECollColl, IsField, IsString],
  function( m, f, string )
    local el;
    el := rec( matrix := m, basefield := f, type := string );

    if IsEvenInt(Size(f)) and
      (string = "elliptic" or string = "hyperbolic" or string = "parabolic") then
      Objectify(NewType( QuadraticFormFamily ,  IsFormRep),  el);
    elif string = "hermitian" then 
      Objectify(NewType( HermitianFormFamily ,  IsFormRep),  el);
    else 
      Objectify(NewType( SesquilinearFormFamily ,  IsFormRep),  el);
    fi;
    return el;
  end );

InstallMethod( MatrixToSesquilinearForm, 
  [ IsMatrix and IsFFECollColl, IsField, IsString ],
  function( m, f, s )
    return FormByMatrix(m, f, s);
  end );


InstallMethod( BaseChange, "for a sesquilinear form",
  [IsSesquilinearForm and IsFormRep],
  function(f)
  local string,m,n,q;
  string := f!.type;
  m := f!.matrix;
  n := Length(m)-1;
  q := Size(f!.basefield);
  if string = "elliptic" or string = "hyperbolic" or string = "parabolic" then
    return BASE_REDUCTION1(m,n,q);
  elif string = "symplectic" then
    return [ChangeSymplecticFormToCanonical( m, f!.basefield )];
  elif string = "hermitian" then
    return BASE_REDUCTION3(m,n,q);
  fi;
end );


InstallMethod( BaseChange, "for an hermitian form",
  [IsHermitianForm and IsFormRep],
  function(f)
    local m,n,q;
    m := f!.matrix; 
    n := Length(m)-1;
    q := Size(f!.basefield);
  return BASE_REDUCTION3(m,n,q);
end );

InstallMethod( ChangeFormToCanonical, [IsForm],
  function( m )
    local temp;
    temp := BaseChange(m)[1];
    return temp;
  end );



#############################################################################
# Viewing methods:
#############################################################################

InstallMethod( ViewObj, 
  [ IsHermitianForm ],
  function( f )
    Print("< hermitian form >");
  end );

InstallMethod( PrintObj,
  [ IsHermitianForm ],
        function( f )
        Print("Hermitian form given by Gram Matrix\n",f!.matrix,"\n");
        end );

InstallMethod( Display,
   [ IsHermitianForm ],
   function( f )
      Print("Hermitian form given by Gram Matrix\n");
      Display(f!.matrix);
   end);


InstallMethod( ViewObj, 
  [ IsQuadraticForm ],
  function( f )
    Print("< quadratic form >");
  end );

InstallMethod( PrintObj,
  [ IsQuadraticForm ],
        function( f )
          Print("Quadratic form given by Gram Matrix\n",f!.matrix,"\n");
        end );

InstallMethod( Display,
  [ IsQuadraticForm ],
     function( f )
     Print("Quadratic form given by Gram Matrix\n");
     Display(f!.matrix);
  end );


InstallMethod( ViewObj, 
  [ IsSesquilinearForm ],
  function( f )
    Print("< sesquilinear form >");
    end );

InstallMethod( PrintObj,
  [ IsSesquilinearForm ],
        function( f )
          Print("Sesquilinear form given by Gram Matrix\n",f!.matrix,"\n");
        end );

InstallMethod( Display,
  [ IsSesquilinearForm ],
  function( f )
    Print("Sesquilinear form given by Gram Matrix\n");
    Display(f!.matrix);
  end);

#############################################################################
# Functions:
#############################################################################

InstallGlobalFunction(SWR,
    function(i,j,n)
    local P;
    P := IdentityMat(n);
    P[i][i] := 0;
    P[i][j] := 1;
    P[j][j] := 0;
    P[j][i] := 1;
    return P;
end);

InstallGlobalFunction(SUM_OF_SQUARES,
    function(v,q)
    local stop,dummy,i,v1,v2;
    stop := false;
    i := 0;
    repeat
      dummy := LogFFE(v - Z(q)^(2*i),Z(q));
      if dummy mod 2 = 0 then
        v1 := Z(q)^i;
        v2 := Z(q)^(dummy/2);
        stop := true;
      else
        i := i + 1;
      fi;  
    until stop;
    return [v1,v2];
end);


InstallGlobalFunction(REDUCE2,
    function(start,stop,n,q)
    local t,i,P;
    t := Z(q)^(LogFFE(-1*One(GF(q)),Z(q))/2);
    P := IdentityMat(n,GF(q));
    i := start;
    while i < stop do
      P[i][i] := (1/2)*One(GF(q));
      P[i][i+1] := (1/2)*t;
      P[i+1][i] := (1/2)*One(GF(q));
      P[i+1][i+1] := (-1/2)*t;
      i := i + 2;
    od;
    return P;
end);


InstallGlobalFunction(REDUCE4,
    function(start,stop,n,q)
    local c,d,i,P,dummy;
    P := IdentityMat(n,GF(q));
    i := start;
    dummy := SUM_OF_SQUARES(-1*One(GF(q)),q);
    c := dummy[1];
    d := dummy[2];
    while i < stop do
      P[i+1][i+1] := c;
      P[i+1][i+3] := d;
      P[i+3][i+1] := d;
      P[i+3][i+3] := -c;
      i := i + 4;
    od;
    return P;
end);

 
InstallGlobalFunction(DIFF_2_S,
    function(start,stop,n,q)
    local i,P;
    i := start;
    P := IdentityMat(n,GF(q)); 
    while i < stop do
      P[i][i] := 1/2*One(GF(q));
      P[i][i+1] := 1/2*One(GF(q));
      P[i+1][i] := 1/2*One(GF(q));
      P[i+1][i+1] := -1/2*One(GF(q));
      i := i + 2;
    od;
    return P;
end);


InstallGlobalFunction(BASE_REDUCTION1,
    function(mat,n,q)
    local row,i,j,k,A,b,c,d,P,D,dummy,r,w,s,v,v1,v2,
          stop,stop2,nplus1,nplus2;
    A := mat;
    #n is the projective dimension
    nplus1 := n + 1;
    nplus2 := n + 2;
    D := IdentityMat(nplus1,GF(q));
    row := 0;
    stop := false;
    repeat       

    # We search for an element different from zero on
    # the main diagonal from row + 1 onwards

      i := 1 + row;
      dummy := false;
      while dummy = false and i <= nplus1 do
        if IsZero(A[i][i]) then
          i := i + 1;
        else
           dummy := true;
        fi;
      od;

      # if i is row + 1, then we do nothing since A[i][i] <> 0
      if 2 + row <= i and i <= nplus1 then
        P := SWR(row + 1,i,nplus1);
        A := P*A*P;
        D := P*D;

      # If we have not found it on the main diagonal, then we go and do other work.
      # key1 = 5. This we easy to find, no?
      # We search for a nonzero element off the main diagonal.

      elif i = nplus2 then
        i := 1 + row;
        dummy := false;
        while i <= n and dummy = false do
          k := i + 1; 
          while k <= nplus1 and dummy = false do
            if A[i][k] = Zero(GF(q)) then
              k := k + 1;
            else
              dummy := true;
            fi;
          od;
          if k = nplus2 then
            i := i + 1;
          fi;
        od;

        # if i is n+1, then they are all zero and we can stop.

        if i = nplus1 then
          stop := true;
          r := row;

        # Otherwise: Go and fetch...
        # Zet het op A[row+1,row+2]
        elif i = row + 1 then #Hier stond i = 1
          P := SWR(row+2,k,nplus1);
          A := P*A*P;
          D := P*D;
        else
          P := SWR(row+2,k,nplus1)*SWR(row+1,i,nplus1);
          A := P*A*TransposedMat(P);
          D := P*D;
        fi;

        #...take care that there is a nonzero on the main diagonal

        if not stop then
          b := (A[row+2][row+1])^-1;
          P := IdentityMat(nplus1,GF(q));
          P[row+1][row+2] := b;
          A := P*A*TransposedMat(P);
          D := P*D;
        fi;
      fi;   # end if 2+row <= i and i <= nplus1 ... elif ... fi

      #Er staat een niet nul element op de hoofddiagonaal, maak de rest nul      

      if not stop then
        P := IdentityMat(nplus1,GF(q));
        for i in [row+2..nplus1] do
           P[i][row+1] := -A[i][row+1]*(A[row+1][row+1])^-1;
        od;
        A := P*A*TransposedMat(P);
        D := P*D;
        row := row + 1;
      fi;
    until row = n or stop;

#Tel hoeveel variabelen er gebruikt zijn.    

    if not stop then
      if A[nplus1][nplus1] <> Zero(GF(q)) then
        r := nplus1;
      else
        r := n;
      fi;
    fi;

#Hier scheiden we de kwadraten van de niet kwadraten

    i := 1;
    s := 0;
    stop := false;
    while (not stop) and i < r do
      if LogFFE(A[i][i],Z(q)) mod 2 <> 0 then
        j := i + 1;
        stop2 := false;
        repeat
          if LogFFE(A[j][j],Z(q)) mod 2 = 0 then
            dummy := A[j][j];
            A[j][j] := A[i][i];
            A[i][i] := dummy;
            D := SWR(i,j,nplus1)*D;
            stop2 := true;
            i := i + 1;
            s := s + 1;
          else
            j := j + 1;
          fi;
        until stop2 or j = r + 1;
        if j = r + 1 then
          stop := true;
        fi;
      else
        i := i + 1;
        s := s + 1;
      fi;
    od; #einde while (not stop) and i < r do
    if LogFFE(A[r][r],Z(q)) mod 2 = 0 then
       s := s + 1;
    fi; 

#We gaan naar de vorm x_0^2 + ... + x_s^2 + v(x_s+1^2 + ... + x_r^2)
#s"le.u.t:e.l:twee = 6
#met v een niet kwadraat

    P := IdentityMat(nplus1,GF(q));
    v := Z(q);
    for i in [1..s] do
      P[i][i] := (Z(q)^(LogFFE(A[i][i],Z(q))/2))^-1;
    od;
    for i in [s+1..r] do
      P[i][i] := (Z(q)^(LogFFE(A[i][i]/Z(q),Z(q))/2))^-1;
    od;
    D := P*D;

#We gooien nu zoveel mogelijk niet kwadraten weg:
    
    s := s - 1;
    r := r - 1;
    
#We schrijven eerst v=v1^2 + v2^2
    
    dummy := SUM_OF_SQUARES(v,q);
    v1 := dummy[1];
    v2 := dummy[2];

#Gevallenonderscheid:

    P := IdentityMat(nplus1,GF(q));
    if not (s = -1 or r = s )  then
      if (r - s) mod 2 = 0 then
        for i in [s+2..r+1] do
          P[i][i] := v1/v;
        od;
        i := s + 2;
        repeat
          P[i][i+1] := -v2/v;
          P[i+1][i] := v2/v;
          i := i + 2;
        until i = r + 2;
        D := P*D;
        s := r;
      else
        if r mod 2 = 0 then
          for i in [1..s+1] do
            P[i][i] := v1;
          od;
          i := 1;
          repeat
            P[i][i+1] := v2;
            P[i+1][i] := -v2;
            i := i + 2;
          until i = s + 2;
          D := P*D;
          s := -1;
        elif not (s = r - 1) then
          for i in [s+2..r] do
            P[i][i] := v1/v;
          od;
          i := s + 2;
          repeat
            P[i][i+1] := -v2/v;
            P[i+1][i] := v2/v;
            i := i + 2;
          until i = r + 1;
          D := P*D;
          s := r - 1;
        fi; #einde if r mod 2 = 0
      fi; #einde if (r-s) mod 2 = 0
    fi; #einde if not (s=-1 or r=s) then
    
#Naar standaardvormen:

    if r mod 2 <> 0 then
      if s = -1 or s = r then
        if q mod 4 = 1 then
          P := REDUCE2(1,r+1,nplus1,q);
          D := P*D;
          w := 2;
        else
          if ((r-1)/2) mod 2 <> 0 then
            P := REDUCE4(1,r+1,nplus1,q);
            D := P*D;
            P := DIFF_2_S(1,r+1,nplus1,q);
            D := P*D;
            w := 2;
          else
            P := REDUCE4(3,r+1,nplus1,q);
            D := P*D;
            P := DIFF_2_S(3,r+1,nplus1,q);
            D := P*D;
            w := 0;
          fi; # if ((r-1)/2) mod 2 <> 0
        fi; # if q mod 4 = 1
      else   #s=r-1
        if q mod 4 = 1 then
          if 1 < r then  #Vroeger: 3 < r + 1, maar dit komt op tzelfde neer
            P := SWR(2,r+1,nplus1);
            D := P*D;
            P := REDUCE2(3,r+1,nplus1,q);
            D := P*D;
          fi;
          w := 0;
        else # q mod 4 = 3
          if ((r-1)/2) mod 2 <> 0 then
            P := SWR(4,r+1,nplus1);
            D := P*D;
            if 3 < r then
              P := REDUCE4(5,r+1,nplus1,q);
            else
              P := IdentityMat(nplus1,GF(q));
            fi;
            b := Z(q)^(LogFFE(-v,Z(q))/2);
            P[3][3] := (1/2)*One(GF(q));
            P[4][3] := (1/2)*One(GF(q));
            P[3][4] := -1/(2*b);
            P[4][4] := 1/(2*b);
            D := P*D;
            if 3 < r then
              P := DIFF_2_S(5,r+1,nplus1,q);
              D := P*D;
            fi;
            w := 0;
          else
            P := SWR(2,r+1,nplus1);
            D := P*D;
            if 1 < r then
              P := REDUCE4(3,r+1,nplus1,q);
            else
              P := IdentityMat(nplus1,GF(q));
            fi;  
            b := Z(q)^(LogFFE(-v,Z(q))/2);
            P[1][1] := 1/2;
            P[2][1] := 1/2;
            P[1][2] := -1/(2*b);
            P[2][2] := 1/(2*b);
            D := P*D;              
            if 1 < r then
              P := DIFF_2_S(3,r+1,nplus1,q);
              D := P*D;
            fi;
            w := 2;
          fi; # if ((r-1)/2) mod 2
        fi; # if q mod 4 = 1
      fi; # if s = -1 or s = r  #s!le!ute°l_3 = 7
    elif r <> 0 then #r is even
      w := 1;
      if q mod 4 = 1 then
        P := REDUCE2(2,r+1,nplus1,q);
        D := P*D;
      else
        if r mod 4 = 0 then
          P := REDUCE4(2,r+1,nplus1,q);
          D := P*D;
          P := DIFF_2_S(2,r+1,nplus1,q);
          D := P*D;
        else
          if 3 < r then
            P := REDUCE4(4,r+1,nplus1,q);
          else
            P := IdentityMat(nplus1,GF(q));
          fi;
          dummy := SUM_OF_SQUARES(-1,q);
          c := dummy[1];
          d := dummy[2];
          P[1][1] := c;
          P[1][3] := d;
          P[3][1] := d;
          P[3][3] := -c;
          D := P*D;
          P := DIFF_2_S(2,r+1,nplus1,q);
          D := P*D;
          P := IdentityMat(nplus1,GF(q));
          i := 3;
          while i <= r + 1 do
            P[i][i] := -1*One(GF(q));
            i := i + 2;
          od;
          D := P*D;
        fi; #if r mod 4 = 0
      fi; #if q mod 4 = 1
    else #r=0
      w := 1;
    fi; #if r mod 2 <> 0


    return [D,r,w];
#Oef, eindelijk het einde

end);


InstallGlobalFunction(RESET,
    function(mat,n,q)
    local i,j,A,B,t;
    t := Zero(GF(q));
    A := List(mat,ShallowCopy);
    B := List(TransposedMat(A),ShallowCopy);
    for i in [1..n] do
      B[i][i] := t;
    od;
    A := A + B;
    for i in [2..n] do  
      for j in [1..i-1] do 
        A[i][j] := t;
      od;
    od;
    return A;
end);


InstallGlobalFunction(SQRT2,
    function(a,q)
    if a = 0*Z(q) then
      return a;
    else
      return Z(q)^((q*LogFFE(a,Z(q)))/2);
    fi;
end);


InstallGlobalFunction(PERM_VAR,
    function(n,r)
    local i,P;
    P := IdentityMat(n);
    for i in [1..r-1] do
      P[i][i] := 0;
      P[i+1][i] := 1;
    od;
    P[r][r] := 0;
    P[1][r] := 1;
    return P;
end);

InstallGlobalFunction(C1,
    function(q,h)
    local i; 
    if h mod 2 = 0 then
      i := 1;
      while i <= h - 1 do
        if Trace(GF(q),Z(q)^i) <> Zero(GF(q)) then
        #if TRACE(Z(q)^i,q,h) <> Zero(GF(q)) then
          return Z(q)^i;
        else
          i := i + 1;
        fi;
      od;
    else
      return One(GF(q));
    fi;
end);


InstallGlobalFunction(QUAD_EQ,
    function(delta,q,h)
    local i,k,dummy,result;
    k := C1(q,h);
    dummy := 0*Z(q);
    result := 0*Z(q);
    for i in [1..h-1] do
      dummy := dummy + k^(2^(i-1));
      result := result + dummy*(delta^(2^i));
    od;
    return result;
end);


InstallGlobalFunction(BASE_REDUCTION2,
    function(mat,n,q)  
    local A,r,w,row,dummy,i,j,h,D,P,t,a,b,c,d,e,s,
      zeros,posr,posk,nplus1,control;
#n is de meetkundige dimensie!!!!!!
    nplus1 := n + 1;
    r := nplus1;   
    row := 1;
    A := List(mat,ShallowCopy);
    D := IdentityMat(nplus1,GF(q));
    zeros := [];
    for i in [1..nplus1] do
      zeros[i] := Zero(GF(q));
    od;      
    h := Length(Factors(q));
    while row + 2 <= r do
      if A[row][row] <> Zero(GF(q)) then
        control := false;
        i := row + 1;
#controle op de hoofddiagonaal, We zoeken een nul
        while i <= r and not control do
          if A[i][i] = Zero(GF(q)) then
            control := true;
          else
            i := i + 1;
          fi;
        od;
#Als er ergens een nul staat, ga ze halen
        if control then
          P := SWR(row,i,nplus1);
          A := P*A*P;   
          D := P*D;
          A := RESET(A,nplus1,q);
          #dummy := false;
#Anders, een truuk: Zoek op andere plaatsen
        else
          dummy := true;
          i := row;
          while i <= r - 1 and dummy do
            j := i + 1;
            while j <= r and dummy do
              if A[i][j] <> Zero(GF(q)) then
                posr := i;
                posk := j;   
                dummy := false;
              else
                j := j + 1;
              fi; 
            od;
            i := i + 1;
          od;
#Als daar alles nul is, STOP
          if dummy then
            P := IdentityMat(nplus1,GF(q));
            t := SQRT2(A[row][row],q);
            P[row][row] := 1/t;
            for i in [row + 1..r] do
              P[i][row] := SQRT2(A[i][i],q)/t;
            od;
            D := P*D;
#Permutatie van de variabelen, het is een parabolische
            r := row;
            P := PERM_VAR(nplus1,r);
            D := P*D;
            w := 1;
            r := r - 1;
            return [D,r,w];
#Anders: Een duistere basisovergang op stelten zetten
          else
            if A[row+1][row+2] = Zero(GF(q)) then
              if posr = row + 1 then
                P := SWR(posk,row+2,nplus1);
              elif posk = row + 2 then
                P := SWR(posr,row+1,nplus1);
              #elif posk = row + 1 then
              #  P := TransposedMat(PermutationMat((posr,row+1,row+2),nplus1));
              elif posr = row + 2 then
                P := TransposedMat(PermutationMat((posk,posr,row+1),nplus1));
              else
                P := SWR(posr,row+1,nplus1)*SWR(posk,row+2,nplus1);
              fi;
              A := P*A*TransposedMat(P);
              D := P*D;
              A := RESET(A,nplus1,q);
            fi;
#A[row+1][row+2] <> 0
            P := IdentityMat(nplus1,GF(q));
            t := A[row+1][row+2];
            P[row][row+2] := A[row][row+1]/t;
            P[row+2][row+2] := 1/t;  #DIT IS NIEUW OP 4 mei 2000 !!
            if row + 3 <= nplus1 then
              for i in [row+3..nplus1] do
                P[i][row+2] := A[row+1][i]/t;
              od;
            fi;
            A := P*A*TransposedMat(P);   
            A := RESET(A,nplus1,q);
            D := P*D;
#A heeft nu de speciale vorm a_11*X_1^2+X_1*X_2 + G(X_0,X_2,...,X_n);
            b := A[row][row];
            P :=  IdentityMat(nplus1,GF(q));
            t :=  SQRT2(b/A[row+1][row+1],q);
            P[row][row+1] := t;
            A := P*A*TransposedMat(P);
            A := RESET(A,nplus1,q);
            D := P*D;
#A[row][row] is nu 0 (eindelijk!) #s}l~e(ute~lnr(twee plus twee) = 8
          fi; #if dummy ... else ... fi; dummy = true als alles nul is
        fi; #if control ... else ... fi; control = true als A[i][i] = 0
      fi; #if A[row][row] <> 0*Z(q) 
#Controle op nulrij:
      dummy := true;
      i := row + 1;
      while  (dummy and i <= nplus1) do
        if A[row][i] <> 0*Z(q) then
          dummy := false;
        else
          i := i + 1;
        fi;
      od;
      posk := i;
#A een nulrij is dan:
      if dummy then
        P := IdentityMat(nplus1);
        P[row][row] := 0;
        P[r][row] := 1;
        for i in [row+1..r] do
          P[i][i] := 0;
          P[i-1][i] := 1;
        od;
        A := P*A*TransposedMat(P);
        #A := RESET(A,nplus1,q);
        D := P*D;
        r := r - 1;
#Anders:
      else
        if posk <> row + 1 then
          P := SWR(posk,row+1,nplus1);
          D := P*D;
          A := P*A*TransposedMat(P);
          A := RESET(A,nplus1,q);
        fi;
#nu is A[k][k+1] <> 0
        P := IdentityMat(nplus1,GF(q));
        t := A[row][row+1];
        P[row+1][row+1] := P[row+1][row+1]/t;
        for i in [row+2..nplus1] do
          P[i][row+1] := A[row][i]/t;
        od;
        D := P*D;
        A := P*A*TransposedMat(P);
        A := RESET(A,nplus1,q);  
        P := IdentityMat(nplus1,GF(q));
        for i in [row+1..nplus1] do
          P[i][row] := A[row+1][i];
        od;
        D := P*D;
        A := P*A*TransposedMat(P);
        A := RESET(A,nplus1,q);
        row := row + 2;
      fi; #if dummy ... else ... fi; dummy = true als er een nulrij is 
    od;
    
#nu kunnen er nog hoogstens 2 variabelen overshieten.
#Gevallenonderscheid
    
    if r = row then
      if A[row][row] = 0*Z(q) then
        r := r - 1;
        w := 2;
      else
        t := SQRT2(A[r][r],q);
        P := IdentityMat(nplus1,GF(q));
        P[r][r] := 1/t;
        D := P*D;
        P := PERM_VAR(nplus1,r);
        D := P*D;
        w := 1;
      fi;   #Deze is de correctie
    else
      a := A[row][row];   
      b := A[row][row+1];  
      c := A[row+1][row+1];
      t := Zero(GF(q));
      if a = t then
        if b = t then
          if c = t then 
            r := r - 2;
            w := 2;
          else
     #Hier start een correctie       
            P := IdentityMat(nplus1,GF(q));
            P[r][r] := 1/SQRT2(c,q);
            D := P*D;
     #Hier stopt ze    
            P := PERM_VAR(nplus1,r); 
            D := P*D;
            r := r - 1;
            w := 1;
          fi;
        else
          if c = t then
            P := IdentityMat(nplus1,GF(q));
            P[r][r] := 1/b;
            D := P*D;
          else
            P := IdentityMat(nplus1,GF(q));
            P[r-1][r-1] := 1/b;
            P[r][r-1] := c/b;
            D := P*D;
          fi;
          w := 2;
        fi;
      else #a <> t
        if b = t then   
          if c = t then
            #P := SWR(r-1,r,nplus1);
            #D := P*D;
            P := IdentityMat(nplus1,GF(q));
            P[r-1][r-1] := 1/SQRT2(a,q);
            D := P*D;  
            P := PERM_VAR(nplus1,r-1);
            D := P*D;
            r := r - 1; 
          else
            P := IdentityMat(nplus1,GF(q));
            P[r-1][r-1] := 1/SQRT2(a,q);
            P[r][r-1] := SQRT2(c,q)/SQRT2(a,q);
            D := P*D;
            P := PERM_VAR(nplus1,r-1);
            D := P*D;
            r := r - 1;
          fi;
          w := 1;
        else
          if c = t then
            P := IdentityMat(nplus1,GF(q));
            P[r-1][r] := a/b;
            P[r][r] := 1/b;
            D := P*D;
            w := 2;
          else
            d := (a*c)/(b^2);
            if Trace(GF(q),d) = t then
            #if TRACE(d,q,h) = t then
              e := SQRT2(a,q);
              P := IdentityMat(nplus1,GF(q));
              s := QUAD_EQ(d,q,h);
              P[r-1][r-1] := (s+One(GF(q)))/e;   
              P[r-1][r] := e/b;
              P[r][r-1] := s/e;
              P[r][r] := e/b;
              D := P*D;
              w := 2;
            else
              P := IdentityMat(nplus1,GF(q));
              P[r-1][r-1] := SQRT2(c,q)/b;
              P[r][r] := 1/SQRT2(c,q);
              D := P*D;
              if r > 2 then
                P := SWR(2,r-1,nplus1)*SWR(1,r,nplus1);
              else
                P := SWR(1,2,nplus1);
              fi;
              D := P*D;
              e := C1(q,h);
              if e <> d then
                a := QUAD_EQ(d+e,q,h);
                P := IdentityMat(nplus1,GF(q));
                P[2][1] := a; 
                D := P*D;
              fi;
              w := 0;
            fi;    #fi ....  sl(eu<tel-(zes min 1)=11
          fi;
        fi;
      fi;
    fi;
    r := r - 1;
    return [D,r,w];
end);
      

InstallGlobalFunction(HERM_CONJ,
    function(mat,n,t)
    local i,j,dummy;
    dummy := MutableTransposedMat(mat);
    for i in  [1..n] do
      for j in [1..n] do
        dummy[i][j] := dummy[i][j]^t;
      od;
    od;
    return dummy;
end);


InstallGlobalFunction(BASE_REDUCTION3,
    function(mat,n,q)
    local row,i,j,k,A,a,b,P,D,t,dummy,dummy2,r,stop,nplus1;
    A := mat;
    nplus1 := n + 1;
    D := IdentityMat(nplus1)*Z(q)^0;
    row := 0;
    stop := false;
    t := Sqrt(q);
    repeat  #eerste repeat
#n is de meetkundige dimensie
#We zoeken een element <> 0  op de hoofddiagonaal vanaf row + 1

      i := 1 + row;
      dummy := false;
      while dummy = false and i <= nplus1 do
        if A[i][i] = Zero(GF(q)) then
          i := i + 1;
        else
           dummy := true;
        fi;
      od;
#Als i row + 1 is doen we niets, dan is A[i][i] <> 0
      if 2 + row <= i and i <= nplus1 then
        P := SWR(row + 1,i,nplus1);
        A := P*A*P;  #HERM_CONJ(P) = P, dus HERM_CONJ is niet nodig
        D := P*D;

#Als we er geen vinden, halen we het ergens anders      

      elif i = n + 2 then
        i := 1 + row;
        dummy := false;
        while i <= n and dummy = false do
          k := i + 1; 
          while k <= nplus1 and dummy = false do
            if A[i][k] = Zero(GF(q)) then
              k := k + 1;
            else
              dummy := true;
            fi;
          od;
          if k = n + 2 then
            i := i + 1;
          fi;
        od;

#Als i n+1 is, dan zijn het allemaal nullen en mogen we stoppen        

        if i = nplus1 then
          stop := true;
          r := row;

#Anders: Ga het halen...        
    #Zet het op A[row+1,row+2]
        elif i = row + 1 then
          P := SWR(row+2,k,nplus1);
          A := P*A*P;
          D := P*D;
        else
          P := SWR(row+2,k,nplus1)*SWR(row+1,i,nplus1);
          A := P*A*TransposedMat(P);
          D := P*D;
        fi;

#...en zorg dat op de hoofddiagonaal een niet nul staat        

        if not stop then
          b := Z(q)*(A[row+2][row+1])^-1;
          P := IdentityMat(nplus1)*Z(q)^0;
          P[row+1][row+2] := b;
          A := P*A*HERM_CONJ(P,nplus1,t);
          D := P*D;
        fi;
      fi;

#Er staat een niet nul element op de hoofddiagonaal, maak de rest nul      

      if not stop then
        P := IdentityMat(nplus1)*Z(q)^0;
        for i in [row+2..nplus1] do
           P[i][row+1] := -A[i][row+1]*(A[row+1][row+1])^-1;
        od;
        A := P*A*HERM_CONJ(P,nplus1,t);
        D := P*D;
        row := row + 1;
      fi;
    until row = n or stop;

#Tel hoeveel variabelen er gebruikt zijn.    

    if not stop then
      if A[nplus1][nplus1] <> Zero(GF(q)) then
        r := nplus1;
      else
        r := n;
      fi;
    fi;
    r := r - 1;
    
#Zorgen dat de diagonaalelementen 1 worden.

    dummy := Difference(GF(q),GF(t));
    P := IdentityMat(nplus1)*Z(q)^0;
    for i in [1..r+1] do
      a := A[i][i];
      if a <> Z(q)^0 then
        j := 1;
        b := dummy[j];
        while a <> b*b^t do
          j := j + 1;
          b := dummy[j];
        od;
        P[i][i] := 1/b;
      fi;
    od;
    D := P*D;
    return [D,r];
end);

InstallMethod( ChangeSymplecticFormToCanonical, [ IsMatrix and IsFFECollColl, IsField and IsFinite ],

## This operation returns an isometry g such that g m g^T is
## the form arising from the block diagonal matrix
## with each block equal to J=[[0,1],[-1,0]]. 

  function( m, f )
    local c1, c2, pos, d, o, iter, basechange, alpha;
    d := Size(m);
    o := Zero(f);
    basechange := [];

    for pos in [1..d/2] do
        iter := Iterator(f^d);
        repeat
          c1 := NextIterator(iter);
        until not IsZero(c1) and not ForAny(basechange, x -> c1*m*x <> o);
      
        ## Find c2 which is orthogonal to c1
        iter := Iterator(f^d);
        repeat
          c2 := NextIterator(iter);
          alpha := c1*m*c2;
        until not IsZero(c2) and alpha <> o;

        ## normalise 
        c2 := c2 / alpha;
        Append(basechange, [c1,c2]);
    od;
    return basechange;
  end );


  
