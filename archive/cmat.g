group := GL(3,9);
m := Random(group)*Z(81)^45;
group2 := GL(3,81);
x := Random(group2);
b := x*m*x^-1;

t := x^-1*b*x;
n := Z(81)^75;
t := t/n;

NewMatrix(IsCMatRep,GF(9),3,t);
NewMatrix(IsCMatRep,GF(81),3,t);

