module lam.

typ _ truth boolean.
typ _ false boolean.
typ _ (num X) integer.
typ G (plus X Y) integer :- typ G X integer, typ G Y integer. 
typ G (and X Y) boolean :- typ G X boolean, typ G Y boolean. 
typ G (eq X Y) boolean :- typ G X T, typ G Y T.
typ G (cond C T E) Type :- 
  typ G C boolean, typ G T Type, typ G E Type.

typ G (var V) T :- assoc V G T.
typ G (app M N) T :- typ G M (arrow T1 T), typ G N T1.
typ G (abs V A) (arrow T1 T) :- 
  typ [pair V T1 | G] A T. 

assoc K [pair K V | _] V :- !.
assoc K [_ | T] V :- assoc K T V.
