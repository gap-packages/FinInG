		prefun := function( subspace ) # This map is the inverse of func and returns an error, or a subspace of geom1
			local flag,basvecs,mat1,span,x,v,v1,i;
			flag:=true;
			if not subspace in pg2 then 
				Error("The input is not in the range fo the field reduction map!");
			fi;
			if not IsInt((Dimension(subspace)+1)/t) then 
				flag:=false;
			else
				basvecs:=BasisVectors(basis);
				mat1:=[];
				span:=[];
				repeat
					repeat 
						x:=Random(Points(subspace)); 
					until not x in span;
					v:=Coordinates(x);
					v1:=List([1..d1],i->v{[(i-1)*t+1..i*t]}*basvecs);
					Add(mat1,v1);
					span:=VectorSpaceToElement(pg2,BlownUpMat(basis,mat1));
				until Dimension(span)=Dimension(subspace);
				if not span = subspace then 
					flag:= false;
				fi;
			fi;
			if flag= false then 
				Error("The input is not in the range of the field reduction map!");
			fi;
			return VectorSpaceToElement(pg1,mat1);
		end;
