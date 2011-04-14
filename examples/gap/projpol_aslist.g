# One of the best features of all of the orb package is the FindSuborbits command
pg:=PG(3,4);
lines:=AsList(Lines(pg));
g:=ProjectivityGroup(pg);
h:=SylowSubgroup(g,5);
FindSuborbits(lines,GeneratorsOfGroup(h));
quit;
