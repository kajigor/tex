module lam.

typ truth boolean.
typ false boolean.
typ (num X) integer.
typ (plus X Y) integer :- typ X integer, typ Y integer. 
typ (and X Y) boolean :- typ X boolean, typ Y boolean. 
typ (eq X Y) T :- typ X T, typ Y T.
typ (cond C T E) Type :- typ C boolean, typ T Type, typ E Type.

typ (app M N) T :- typ M (arrow T1 T), typ N T1.
typ (abs A) (arrow T1 T) :- pi X\ (typ X T1 => typ (A X) T).

alphaEq X X.
alphaEq (abs A) (abs B) :- pi X\ pi Y\ alphaEq (A X) (B Y).
alphaEq (app M N) (app M1 N1) :- alphaEq M M1, alphaEq N N1. 
alphaEq truth truth.
alphaEq false false. 
alphaEq (num X) (num X). 
alphaEq (plus X Y) (plus X1 Y1) :- alphaEq X X1, alphaEq Y Y1. 
alphaEq (and X Y) (and X1 Y1) :- alphaEq X X1, alphaEq Y Y1. 
alphaEq (eq X Y) (eq X1 Y1) :- alphaEq X X1, alphaEq Y Y1. 
alphaEq (cond C X Y) (cond C1 X1 Y1) :- 
  alphaEq C C1, alphaEq X X1, alphaEq Y Y1. 


