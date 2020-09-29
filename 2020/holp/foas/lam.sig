sig lam. 

kind tm type.
kind tp type.
kind pr type -> type -> type. 

type pair X -> Y -> pr X Y.

type assoc X -> list (pr X Y) -> Y -> o.

type var Vr -> tm. 
type abs Vr -> tm -> tm.
type app tm -> tm -> tm.

type num int -> tm.
type plus tm -> tm -> tm.
type eq tm -> tm -> tm.

type truth tm.
type false tm.
type and tm -> tm -> tm.

type cond tm -> tm -> tm -> tm.

type typ list (pr Vr tp) -> tm -> tp -> o. 

type integer tp. 
type boolean tp.
type arrow tp -> tp -> tp. 
