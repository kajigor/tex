sig lam. 

kind tm type.
kind tp type.

type abs (tm -> tm) -> tm.
type app tm -> tm -> tm.

type num int -> tm.
type plus tm -> tm -> tm.
type eq tm -> tm -> tm.

type truth tm.
type false tm.
type and tm -> tm -> tm.

type cond tm -> tm -> tm -> tm.

type typ tm -> tp -> o. 

type integer tp. 
type boolean tp.
type arrow tp -> tp -> tp. 

type alphaEq tm -> tm -> o.
