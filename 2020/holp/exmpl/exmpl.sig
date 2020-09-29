sig exmpl.

kind pr type -> type -> type.

type human string -> o.

type isMortal string -> o.

type child string -> string -> o.
type grandchild string -> string -> o.

type append list X -> list X -> list X -> o.

type pair A -> B -> pr A B.

type assoc A -> B -> list (pr A B) -> o.
