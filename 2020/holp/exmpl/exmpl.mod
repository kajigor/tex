module exmpl.

human "Socrates".
human "Kate".

isMortal X :- human X.

child "Ivan" "Marya".
child "Marya" "Sveta".
child "Ivan" "Ilya".
child "Marya" "Dmitry".

grandchild X Z :- child X Y, child Y Z.

append nil X X.
append [H|X] Y [H|Z] :- append X Y Z.

assoc K V [pair K V | _].
assoc K V [pair K1 _ | T] :- assoc K V T.
