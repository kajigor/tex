sig lam.

kind term type -> type.

kind pr type -> type -> type. 

type pair X -> Y -> pr X Y. 

type var V -> term V.  

type abs V -> term V -> term V. 

type app term V -> term V -> term V.

type id term V -> term V -> o.

type alphaEq term V -> term V -> o.  

type equal term V -> term V -> list (pr V (term V)) -> o.
type equalAux term V -> term V -> list (pr V (term V)) -> o.

type mapsTo X -> Y -> list (pr X Y) -> o.

type delkey list (pr X Y) -> X -> list (pr X Y) -> o.
