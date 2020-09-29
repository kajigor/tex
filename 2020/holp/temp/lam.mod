module lam.

mapsTo X Y [pair X Y | _] :- !.
mapsTo X Y [pair Y _ | T] :- (X > Y; Y > X), mapsTo X Y T.

delkey nil _ nil. 
delkey [pair X _ | T] X T.
delkey [pair X1 Y1 | T] X [pair X1 Y1 | T1] :- 
  (X1 > X; X > X1), delkey T X T1. 

alphaEq T1 T2 :- equal T1 T2 nil.

fv (var X) [X]. 
fv (app M1 N1) FV :- fv M1 FV1, fv M2 FV2, 

equal M N U :- 
  equalAux M N U,
  fv M FV, 
  mapdom U FM,
  invmap U V,
  equalAux N M V.

equalAux (var X) (var Y) Subst :- 
  mapsTo X (var Y) Subst. 

equalAux (app M1 N1) (app M2 N2) Subst :- 
  equalAux M1 M2 Subst, equalAux N1 N2 Subst. 

equalAux (abs X T) (abs Y U) Subst :- 
  equalAux T U Subst1, delkey Subst1 X Subst. 

