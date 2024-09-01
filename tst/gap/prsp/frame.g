#StandardFrame
frame := StandardFrame(PG(5,11));
Length(frame);
frame := StandardFrame(PG(2,19));
Length(frame);
Display(frame);
vects := List(frame,x->Unpack(UnderlyingObject(x)));
Display(vects);
quit;


