
# The following function orders the elements of the finite field

FinInGFieldNumbering:=function(a,q)
# returns the integer i+1 such that a=w^i, with w the 
# PrimitiveRoot(GF(q)); returns q if a=Zero(GF(q)).
	local F;
	F:=GF(q);
	if not a in F then Error("The first argument should be an element of GF(q) !");
	elif a=Zero(F) then return q;
	else return LogFFE(a,PrimitiveRoot(F))+1;
	fi;
end;

# Illustration of this function:
# q:=81; List(Elements(GF(q)),a-> a = PrimitiveRoot(GF(q))^(FinInGFieldNumbering(a,q)-1));
# the first "false" is concerns the Zero(GF(q)), which gets labeled q.


FinInGFieldNumberingInverse:=function(num,q)
# this is the inverse of the function FinInGFieldNumbering
	local F;
	F:=GF(q);
	if not num in [1..q] then return Error("The first argument should be in the interval [1..q] !");
	elif num=q then return Zero(F);
	else return PrimitiveRoot(F)^(num-1);
	fi;
end;


# Enumerator for a the points of a projective space

ElementToNumber:=function(point)
# This function labels the points of a projective space
	local v,i,num,w,flag,n,q,pg,coefs,x,j;
	pg:=AmbientGeometry(point);
	q:=Size(BaseField(pg));
	flag:=false;
	v:=Coordinates(point);
	n:=Length(v);
	# First we find the first nonzero coordinate
	i:=0;
	num:=0;
	repeat i:=i+1; 
	if v[i] = One(GF(q)) then
		flag:=true;
	else # this means v[i] = Zero(GF(q))
		num:=num+q^(n-i); # for each 0 we add q^(n-i) to the number
	fi;	
	until flag=true;
	if i=n then return (q^n-1)/(q-1);
	else
		# with each coordinate associate the FinInGFieldNumbering minus one,
		# and use this as a coefficient in the q-ary expansion
		coefs:=List(v,x->FinInGFieldNumbering(x,q)-1);
		# we substract one because we want the coefs to take values from [0..q-1]		
		return num+Sum([i+1..n],j->coefs[j]*q^(n-j))+1;
		# we have to add one because we want the output to have values [1..(q^n-1)/(q-1)]
	fi;
end;

NumberToElement:=function(pg,num)
# This function associates a number in the [1.. #pts in pg] to a projective point in pg
	local n,q,coefs,i,v,j,k,r,rem,quo;
	n:=Dimension(pg)+1;
	q:=Size(BaseField(pg));
	if not num in [1..(q^n-1)/(q-1)] then return Error("The integer is not in the range !");
	else
		if num=(q^n-1)/(q-1) then # we have the point with coordinates (0,0,0,...,0,1)
			v:=Concatenation(List([1..n-1],i->Zero(GF(q))),[One(GF(q))]);
			return VectorSpaceToElement(pg,[v]);
		elif num=(q^n-1)/(q-1)-1 then # we have the point with coordinates (0,0,0,...,1,0)
			v:=Concatenation(Concatenation(List([1..n-2],i->Zero(GF(q))),[One(GF(q))]),[Zero(GF(q))]);
			return VectorSpaceToElement(pg,[v]);
		else
			rem:=num-1; # the points are now number from [0 ...] instead of from [1 ...]
			i:=0;r:=0;
			repeat i:=i+1;
				r:=r+q^(n-i);
			until r> rem; #now we in which interval the number lies, and this determines
			# the number of zeros in the coordinates of the point
			# if i=1, then its an affine point (1,x1,x2,x3,...)
			# if i=2, then its a point with coordinates (0,1,x2,x3,...) etc...
			if i=1 then v:=[One(GF(q))];
				else v:=Concatenation(List([1..i-1],i->Zero(GF(q))),[One(GF(q))]);
			fi;
			if i>1 then
				rem:=rem-Sum([1..i-1],j->q^(n-j));
			fi;
			coefs:=[];
			for k in [0..n-i-1] do
				j:=n-i-1-k;
				quo:=EuclideanQuotient(rem,q^j);
				rem:=EuclideanRemainder(rem,q^j);
				Add(coefs,quo);
			od;
		fi;
		# check num = Sum([0..n-1],i->coefs[i+1]*q^(n-1-i));
		for j in [1..n-i] do
			if coefs[j]=q-1 then Add(v,Zero(GF(q)));
			else Add(v,PrimitiveRoot(GF(q))^coefs[j]);
			fi;
		od;
		return VectorSpaceToElement(pg,[v]);
	fi;
end;

# Here is a test to see that there is a one-to-one correspondence
# Filtered(Points(pg),point->not NumberToElement(pg,ElementToNumber(point))=point);



